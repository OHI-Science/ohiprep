### data_prep.R for WHO/UNICEF sanitation data
### reformat and add rgn_ids to WHO-UNICEF JMP (Joint Monitoring Programme) data
### previously clean_WHOUNICEF.r:
###   name_to_rgn
###   georegional gapfilling with gapfill_georegions.r


# Nature 2012 SOM p. 51: 'Access to improved sanitation facilities is defined as the percentage
#   of the population within a country with at least adequate access to excreta disposal facilities
#   that can effectively prevent human, animal, and insect contact with excreta. These data are a 
#   country-wide average (not specific to the coastal region) and the most recent year available is 
#   2008. Percentages (0-100) for each country were rescaled to 0-1 based on a maximum target of 
#   100% of the population with access to improved sanitation, and a minimum value of 0'

# compare http://www.wssinfo.org/data-estimates/tables/ (226 unique countries' data (coastal and non-coastal))
# with Quandl, which has 452 individual datasets for each country:
# http://www.quandl.com/WHO_UNICEF?keyword=%20&page=4&code=WHO_UNICEF&source_ids=439. But with Quandl, you have to read them in separately.


##############################################################################=
### setup -----
##############################################################################=

library(ohicore) # devtools::install_github('ohi-science/ohicore') # may require uninstall and reinstall
library(zoo)     # for na.locf: Last Observation Carried Forward

setwd('~/github/ohiprep')
source('src/R/common.R')

source('src/R/ohi_clean_fxns.R') # has functions: cbind_rgn(), sum_na()

goal     <- 'globalprep/prs_cw_pathogen/v2015'
scenario <- 'v2015'
dir_git  <- file.path('~/github/ohiprep', goal)
dir_data <- file.path(dir_git, scenario, 'data')
dir_int  <- file.path(dir_git, scenario, 'int')


##############################################################################=
### read in files, clean up data ----
##############################################################################=

sani_raw <- read.csv(file.path(goal, 'raw', 'whounicef_sanitation_1990-2015.csv'), 
                     na.strings = c(NA, ''), skip = 3, stringsAsFactors = FALSE) 
### note: na.strings argument important for na.locf to work

sani <- sani_raw %>%
  select(country,
         year           = Year,
         pop            = x1000,
         imp_tot_ct     = Total.Improved..x1000. ,
         imp_tot_pct    = Total.Improved.... ,
         unimp_tot_ct   = Total.Unimproved..x1000. ,
         unimp_tot_pct  = Total.Unimproved....) %>%
  mutate(pop            = pop          * 1000,
         imp_tot_ct     = imp_tot_ct   * 1000,
         unimp_tot_ct   = unimp_tot_ct * 1000,
         country        = na.locf(country))      # use na.locf() function from zoo package to fill down for country names

### select and scale percent of population with access to improved sanitation
sani_improved <- sani %>%
  select(country, year, access_pct = imp_tot_pct) %>%
  mutate(access_pct = access_pct/100)
summary(sani_improved); head(sani_improved)

### add rgn_id: country to rgn_id
rgn_sani <- name_to_rgn(sani_improved, 
                        fld_name     = 'country', 
                        flds_unique  = c('country','year'), 
                        fld_value    = 'access_pct', 
                        collapse_fxn = 'mean', 
                        add_rgn_name = TRUE,
                        dir_lookup = "src/LookupTables") %>%
  arrange(rgn_id, year)
    ### original note: must use mean, otherwise prop_access > 1
    ### new note (CCO 2015): what about a population-weighted mean?  The following countries could be affected:
    ###   * China (incl Macao and Hong Kong)
    ###   * Guadeloupe/Martinique
    ###   * Puerto Rico/US Virgin Islands
    ###   * Northern Marianas and Guam
    ### Quick inspection shows the percentages aren't outrageously different; and since Macao and HK have
    ### no data, skews China downward.  Stick with simple mean for now.


##############################################################################=
### georegional gapfilling with gapfill_georegions.r ----
##############################################################################=

georegions <- read.csv('../ohi-global/eez2013/layers/rgn_georegions.csv', na.strings='') %>%
  spread(level, georgn_id) 

georegion_labels <- read.csv('../ohi-global/eez2013/layers/rgn_georegion_labels.csv') %>%    
  mutate(level_label = sprintf('%s_label', level)) %>%
  select(-level) %>%
  spread(level_label, label) %>%
  left_join(
    read.csv('../ohi-global/eez2013/layers/rgn_labels.csv') %>%
      select(rgn_id, v_label=label),
    by='rgn_id') %>%
  arrange(r0_label, r1_label, r2_label, rgn_id); head(georegion_labels)

layersave <- file.path(goal, "int", 'rgn_jmp_san_2015a_raw_prop_access.csv')
attrsave  <- file.path(goal, "int", 'rgn_jmp_san_2015a_attr.csv')

r_g_a <- gapfill_georegions(
  data = rgn_sani %>%
    filter(!rgn_id %in% c(213,255)) %>%
    select(rgn_id, year, access_pct),
  fld_id = 'rgn_id',
  georegions = georegions,
  georegion_labels = georegion_labels,
  r0_to_NA = TRUE, 
  attributes_csv = attrsave) # don't chain 

# investigate attribute tables
head(attr(r_g_a, 'gapfill_georegions'))  # or to open in excel: system(sprintf('open %s', attrsave))


##############################################################################=
### save raw proportion with access ----
##############################################################################=
rgn_sani <- r_g_a %>%
  select(rgn_id, year, access_pct) %>%
  arrange(rgn_id, year); head(rgn_sani)
write.csv(rgn_sani, layersave, na = '', row.names = FALSE)

### read raw proportion with access ----
rgn_sani <- read.csv(layersave, stringsAsFactors = FALSE)


##############################################################################=
### model the trend and rescale, different from 2013a style ----
##############################################################################=
# based from neptune_data:model/GL-NCEAS-Pressures_v2013a/model_pathogens.R

popn_density_file <- read.csv(file.path(dir_M, 
                                        'model/GL-NCEAS-CoastalPopulation_v2013/data', 
                                       'rgn_popsum_area_density_2005to2015_inland25mi.csv'))

# population density - strong right skew
popn_density <- popn_density_file %>%  
  filter(pop_per_km2 != 0) ### remove any unpopulated regions (rgn_id 149, 158)

# to add any missing regions as NA
rgns <- read.csv('src/LookupTables/eez_rgn_2013master.csv') %>%
  select(rgn_id = rgn_id_2013,
         rgn_name = rgn_nam_2013)  %>%
  filter(rgn_id < 255)


##############################################################################=
### calculate by scenario ----
##############################################################################=

# identify  years for each scenario and overall.
maxyear_all      <- max(rgn_sani$year, na.rm=T)
scenarios <- c('eez2015' = maxyear_all,
               'eez2014' = maxyear_all - 1,
               'eez2013' = maxyear_all - 2,
               'eez2012' = maxyear_all - 3)

for (i in 1:length(names(scenarios))) { # i=2
  maxyear <- scenarios[i]
  minyear <- maxyear - 4
  sc_name <- names(scenarios)[i]
  
  unsani_pop <- rgn_sani %>%  ### 'poopers' (lack of access) * population density
    filter(year %in% minyear:maxyear, 
           !is.na(access_pct)) %>%  
    select(rgn_id, year, access_pct) %>%
    left_join(popn_density %>%
                filter(year %in% minyear:maxyear) %>%
                select(rgn_id, year, pop_per_km2), 
              by=c('rgn_id', 'year')) %>%
    mutate(propWO_x_pop     = (1 - access_pct) * pop_per_km2, # this is the population density of people without access
           propWO_x_pop_log = log(propWO_x_pop + 1))          # log is important because the skew was high otherwise

  
  ### rescale with all previous scenarios, to 110%:  
  ###   Neptune: model/GL-NCEAS-CleanWatersPressures/pathogens/sanitation-population-combo/model.R
  ### Previous assessments rescaled pressure to maximum 'poopers' since eez2012; now
  ###   pressure is rescaled to 110% of max 'poopers' within past five years.
  unsani_pop <- unsani_pop %>%
    mutate(pressure_score = propWO_x_pop_log / (1.1 * max(propWO_x_pop_log, na.rm = TRUE)))
      
  ### Combine with any missing regions as 0.  In 2015, these regions are:
  ###   Antarctica,	Bouvet Island, Heard and McDonald Islands, Kerguelen Islands, 
  ###   Amsterdam Island and Saint Paul Island, Crozet Islands, Prince Edward Islands,
  ###   South Georgia and the South Sandwich Islands
  sp_pressure <- rbind(unsani_pop %>%
                         filter(year == maxyear) %>%
                         select(rgn_id, pressure_score), 
                       rgns %>%
                         anti_join(unsani_pop, by = 'rgn_id') %>% 
                         mutate(pressure_score = 0) %>%
                         select(-rgn_name))

  stopifnot(anyDuplicated(sp_pressure[,c('rgn_id')]) == 0)  
  
  summary(sp_pressure)
  
  pressures_file <- file.path(dir_data, 
                              sprintf('po_pathogens_popdensity25mi_%sa.csv', substr(sc_name, 4, 7))) 
  write.csv(sp_pressure, pressures_file, row.names = FALSE, na = '')
  

  ##############################################################################=
  ### model 5 year trend ----
  ##############################################################################=
  ### trend is slope of linear regression of pressure scores over the past five 
  ### data years (annual change in pressure), times 5. 
  sp_mdl <- unsani_pop %>%
    filter(!is.na(pressure_score)) %>% # lm can't handle NAs!!
    group_by(rgn_id) %>%
    do(mdl = lm(pressure_score ~ year, data=.)) %>% 
    summarize(
      rgn_id = rgn_id, 
      year_ix0  = coef(mdl)['(Intercept)'],
      year_coef = coef(mdl)['year']) %>%
    mutate(
      trend_raw = year_coef * 5,
      trend = ifelse(trend_raw > 1, 1,
                         ifelse(trend_raw < -1, -1,
                                trend_raw))) %>%
        ### deal with anything outside of +/- 1 - not likely but just in case
    select(rgn_id, trend)
    
  # save 5-year trend 
  # add any missing regions as 0 (note: includes all regions with pressure == NA)
  sp_mdl <- rbind(sp_mdl, 
                  rgns %>%
                    anti_join(sp_mdl, by = 'rgn_id') %>%
                    mutate(trend = 0) %>%
                    select(-rgn_name))

  stopifnot(anyDuplicated(sp_mdl[ , c('rgn_id')]) == 0)
  
  summary(sp_mdl)
  
  trend_file <- file.path(dir_data, 
                          sprintf('pathogens_popdensity25mi_trend_%sa.csv', 
                                  substr(sc_name, 4, 7)))  
  
  write.csv(sp_mdl, trend_file, row.names = FALSE, na = '')
}



# --- fin ---


