# tr_fxns.R for Tourism & Recreation - supporting scripts for data_prep.R


tr_prep_data <- function(tr_data_files, reload = FALSE) {
  
  if(any(!file.exists(tr_data_files)) | reload == TRUE) {
    cat(sprintf('Raw data will be processed into: \n  %s\n', dir_int))
    cat(sprintf('    %s\n', basename(tr_data_files)))
    
    cat('Processing data from World Bank...\n')
    source(file.path(dir_git, 'R/process_WorldBank.R'), local = TRUE)
    cat('Processing data from WTTC...\n')
    source(file.path(dir_git, 'R/process_WTTC.R'), local = TRUE)
    cat('Processing data from World Economic Forum...\n')
    source(file.path(dir_git, 'R/process_WEF.R'), local = TRUE)
    # clear all data from this process
    #cat(sprintf('Currently within process_WEF.R - environment = %s\n', environment()))
    cat('Variables to be cleared: \n')
    cat(sprintf('%s', ls()))
    rm(list = ls())
    
  }
  cat(sprintf('Raw data has been processed; files exist in: \n  %s\n', dir_int))
  cat(sprintf('    %s\n', list.files(dir_int)))
}


tr_prep_layers <- function(tr_layers, tr_data_files, reload = FALSE) {
  ### Prep data into individual layers for toolbox ----
  ##############################################################################=
  ### Separate out just the model variables and save these to v201X/data directory,
  ### ready for use in the toolbox.
  if(any(!file.exists(tr_layers)) | reload == TRUE) {
    #   * tr_unemployment.csv from World Bank data
    tr_unem <- read.csv(tr_data_files[['unem']], stringsAsFactors = FALSE) %>%
      select(rgn_id, year, percent)
    write_csv(tr_unem, tr_layers[['unem']])
    
    #   * tr_jobs_total.csv from World Bank total labor force
    #     * rgn_id, year, count (individuals)
    tr_jobs_tot <- read.csv(tr_data_files[['jobs_tot']], stringsAsFactors = FALSE) %>%
      select(rgn_id, year, count)
    write_csv(tr_jobs_tot, tr_layers[['jobs_tot']])
    
    #   * tr_sustainability.csv from WEF TTCI
    # tr_sust <- read.csv(tr_data_files[['sust']], stringsAsFactors = FALSE) %>%
    #   select(rgn_id, score)
    # write_csv(tr_sust, tr_layers[['sust']])
    
    #   * tr_jobs_tourism.csv from WTTC direct tourism employment and 
    #   * tr_jobs_pct_tourism.csv from WTTC direct tourism employment percentage
    tr_jobs_tour <- read.csv(tr_data_files[['jobs_tour']], stringsAsFactors = FALSE) 
    write_csv(tr_jobs_tour %>%
                select(rgn_id, year, jobs_ct), 
              tr_layers[['jobs_tour']])
    write_csv(tr_jobs_tour %>%
                select(rgn_id, year, jobs_pct),
              tr_layers[['jobs_pct_tour']])
  }
  cat(sprintf('Data layers have been processed; files exist here: \n  %s\n', dir_data))
  cat(sprintf('    %s\n', list.files(dir_data)))
}


tr_assemble_layers <- function(tr_layers) {
  tr_unem          <- read.csv(tr_layers[['unem']],          stringsAsFactors = FALSE)
  tr_sust          <- read.csv(tr_layers[['sust']],          stringsAsFactors = FALSE)
  tr_jobs_tour     <- read.csv(tr_layers[['jobs_tour']],     stringsAsFactors = FALSE)
  tr_jobs_pct_tour <- read.csv(tr_layers[['jobs_pct_tour']], stringsAsFactors = FALSE)
  tr_jobs_tot      <- read.csv(tr_layers[['jobs_tot']],      stringsAsFactors = FALSE)
  
  rgn_names        <- read.csv('../ohi-global/eez2013/layers/rgn_global.csv', stringsAsFactors = FALSE) %>%
    rename(rgn_name = label)
  
  rgn_names <- rgn_names %>%
    left_join(data.frame(rgn_id = rep(1:max(rgn_names$rgn_id), each = 25),
                         year   = rep(c((year_max-24):year_max), max(rgn_names$rgn_id))),
              by = 'rgn_id')
  ### this looks stupid but it assigns a list of years to all regions, just to
  ###   avoid filtering them out later.  Probably a way better way to do this...
    
  tr_data_raw <- rgn_names %>%
    full_join(tr_jobs_tour %>%
                rename(Ed = jobs_ct),
              by = c('rgn_id', 'year')) %>%
    full_join(tr_jobs_pct_tour %>%
                rename(Ep = jobs_pct) %>%
                mutate(Ep = Ep/100,
                       Ep = ifelse(Ep > 1, NA, Ep)),
              by = c('rgn_id', 'year')) %>%
    full_join(tr_jobs_tot %>%
                rename(L = count),
              by = c('rgn_id', 'year')) %>%
    full_join(tr_unem %>%
                rename(U = percent) %>%
                mutate(U = U/100),
              by = c('rgn_id', 'year')) %>%
    full_join(tr_sust %>%
                rename(S_score = score),
              by = 'rgn_id') %>%
    filter(year <= year_max) %>%
    filter(!is.na(rgn_name))
  return(tr_data_raw)
}


gapfill_flags <- function(data) {
  ### Identify the gaps in data.  '_' indicates no gap; a letter indicates a gap
  ### that will force an NA result.  If E term used the Ep data, then U and L are no barrier;
  ### mark them with a '*'. If S_score is present, then GDP gaps don't matter; mark with '*'.
  data <- data %>%
    mutate(ed_gap  = ifelse(is.na(Ed), 'E', '_'),
           u_gap   = ifelse(is.na(U),  ifelse(!is.na(Ep), '*', 'U'), '_'),
           s_gap   = ifelse(is.na(S_score), 'S', '_'),
           l_gap   = ifelse(is.na(L),  ifelse(!is.na(Ep), '*', 'L'), '_'),
           gdp_gap = ifelse(is.na(pcgdp), ifelse(is.na(S_score), 'G', '*'), '_'),
           gaps    = paste(ed_gap, u_gap, s_gap, l_gap, gdp_gap, sep = '')) %>%
    select(-ed_gap, -u_gap, -s_gap, -l_gap, -gdp_gap)
  return(data)
}


gdp_gapfill <- function(data) { #data <- tr_data_raw
  ### Gapfill GDP figures using CIA data to sub for missing WB data for 
  ### current year, so that the TTCI regression can include values.
  no_gdp <- data %>% filter(is.na(pcgdp) & year == year_max & is.na(S_score))  %>% arrange(rgn_name)
  # missing gdp data for the following countries (2013): 
  # Anguilla | Argentina | Aruba | Barbados | Bermuda | British Virgin Islands
  # Cayman Islands | Cuba | East Timor | French Polynesia | Guadeloupe and Martinique | Kuwait
  # Myanmar | New Caledonia | North Korea | Northern Mariana Islands and Guam | Oman | R_union
  # Somalia | Syria | Taiwan | United Arab Emirates
  
  
  gdp_r2_mean <- data %>%
    group_by(r2, year) %>%
    summarize(gdp_r2 = mean(pcgdp, na.rm = TRUE))
  
  data1 <- data %>%
    left_join(gdp_r2_mean, by = c('r2', 'year')) %>%
           mutate(pcgdp = ifelse(is.na(pcgdp), gdp_r2, pcgdp)) %>%
    select(-gdp_r2)

# skip this - there are others as well; this doesn't add much value.  Rely on regional averages.
#   # hand-fill select values: 
#   gdp_reun <- 23501 # from http://www.insee.fr/fr/insee_regions/reunion/themes/dossiers/ter/ter2008_resultats_economiques.pdf
#   # in 2007, not PPP
#   gdp_mart <- 24118 # from: http://web.archive.org/web/20080216021351/http://prod-afd.afd.zeni.fr/jahia/webdav/site/cerom/users/admin_cerom/public/Pdf/CR2006_ma.pdf
#   # in 2006, real exchange rate (PPP?)
#   gdp_guad <- 21780 # from: http://www.insee.fr/fr/regions/guadeloupe/default.asp?page=publications/publications.htm
#   # in 2006 dollars, not PPP.
#   data1 <- data1 %>%
#     mutate(pcgdp = ifelse(rgn_id == 32  & year == year_max, gdp_reun, pcgdp),
#            pcgdp = ifelse(rgn_id == 140 & year == year_max, (gdp_guad+gdp_mart)/2, pcgdp),
#            gaps  = ifelse(rgn_id %in% c(32, 140) & year == year_max, str_replace(gaps, 'G', 'h'), gaps))

  return(data1)
}

s_regr_r1 <- function(data, y_max = year_max) {
  ### create a regression model of S as a function of PPP-adjusted per-capita GDP, and
  ### with a dummy variable correlating to r1 level georegions
  s1 <- data %>% filter(year == y_max)
  s1_coef <- unlist(lm(S_score ~ pcgdp + r1, data = s1)['coefficients'])
  
  s1_mdl  <- stack(s1_coef)
  colnames(s1_mdl) <- c('r1_coef', 'r1')
  
  s1_mdl  <- s1_mdl %>%
    mutate(r1_int      = s1_coef[1],
           r1_gdp_coef = s1_coef[2],
           r1 = str_replace(r1, 'coefficients.r1', '')) # strip the prefix
  data <- data %>%
    left_join(s1_mdl, by = 'r1')
  
  # process r1 level model:
  dropped_rgn <- levels(as.factor(data$r1))[1] # auto figure out which region was first in the list
  data <- data %>%
    mutate(r1_coef = ifelse(r1 == dropped_rgn, 0, r1_coef),
           r1_mdl  = r1_int + r1_gdp_coef * pcgdp + r1_coef) %>%
    select(-r1_coef, -r1_gdp_coef, -r1_int)
  
  return(data)
}

s_regr_r2 <- function(data, y_max = year_max) {
  ### create a regression model of S as a function of PPP-adjusted per-capita GDP, and
  ### with a dummy variable correlating to r2 level georegions
  s2 <- data %>% filter(year == y_max)
  s2_coef <- unlist(lm(S_score ~ pcgdp + r2, data = s2)['coefficients'])
  
  s2_mdl  <- stack(s2_coef)
  colnames(s2_mdl) <- c('r2_coef', 'r2')
  
  s2_mdl  <- s2_mdl %>%
    mutate(r2_int      = s2_coef[1],
           r2_gdp_coef = s2_coef[2],
           r2 = str_replace(r2, 'coefficients.r2', '')) # strip the prefix
  data <- data %>%
    left_join(s2_mdl, by = 'r2')
  
  # process r2 level model:
  dropped_rgn <- levels(as.factor(data$r2))[1] # auto figure out which region was first in the list
  data <- data %>%
    mutate(r2_coef = ifelse(r2 == dropped_rgn, 0, r2_coef),
           r2_mdl  = r2_int + r2_gdp_coef * pcgdp + r2_coef) %>%
    select(-r2_coef, -r2_gdp_coef, -r2_int)
  
  return(data)
}

s_gapfill_r2_r1 <- function(data, y_max = year_max) {
  data <- data %>% 
    s_regr_r2(y_max) %>%
    s_regr_r1(y_max)
  
  data <- data %>%
    mutate(s_mdl = ifelse(!is.na(r2_mdl), r2_mdl, r1_mdl),
           gaps  = ifelse(is.na(S_score) & (!is.na(s_mdl)), str_replace(gaps, 'S', 'g'), gaps),
           S_score = ifelse(is.na(S_score), s_mdl, S_score)) %>%
    select(-r1_mdl, -r2_mdl, -s_mdl)
  
  return(data)
}


tr_calc_model <- function(data_chunk) {
  ### Model modified to prefer the Ep term (percent of tourism jobs from WTTC) to determine E;
  ### if Ep term is not available, calculate old way: E = Ed / (L - (L * U))
  ### Model modified to normalize S_score by the maximum value, after subtracting one.
  
  data_chunk <- data_chunk %>%
    mutate(
      E       = ifelse(is.na(Ep), Ed / (L - (L * U)), Ep),
      S       = (S_score - 1) / (7 - 1), # scale score from 1 to 7.
      Xtr     = E * S )
  return(data_chunk)
}

