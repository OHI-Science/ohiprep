# Get all species from IUCN. Limit to those found in marine habitats.

# Adapted from Ben Best script for 2013

#Jamie Afflerbach

# # load packages - note this should be sourced by data_prep.R, so no need to load libraries already loaded from there.
# library(ohi)
library(RCurl)
library(XML)
library(parallel)

#############################################################################=
generate_iucn_all_spp_list <- function(reload = FALSE) {
### get full IUCN species list from git-annex or web.  If generating a new
### dataframe, saves to git-annex and returns dataframe; otherwise reads
### then returns the dataframe.
#############################################################################=
  
  all_spp_file <- file.path(dir_anx, 'tmp/spp_iucn_all.csv')
  all_spp_url  <- 'http://api.iucnredlist.org/index/all.csv'
  if (!file.exists(all_spp_file) | reload){
    cat(sprintf('Reading remote full IUCN redlist species from: %s\n', all_spp_url))
    spp_iucn_all <- read.csv(all_spp_url, stringsAsFactors = FALSE) 
    spp_iucn_all <- spp_iucn_all[!duplicated(spp_iucn_all), ] 
    spp_iucn_all <- spp_iucn_all %>%
      rename(sciname  = Scientific.Name,
             iucn_sid = Red.List.Species.ID) %>%
      select(-Primary)
    spp_iucn_all <- unique(spp_iucn_all)
    names(spp_iucn_all) = tolower(names(spp_iucn_all))
    
    cat(sprintf('Writing full IUCN redlist species list to: %s\n', all_spp_file))
    write_csv(spp_iucn_all, all_spp_file)
  } else {
    cat(sprintf('Reading local full IUCN redlist species list from: %s\n', all_spp_file))
    spp_iucn_all <- read.csv(all_spp_file, stringsAsFactors = FALSE)
  }
  return(invisible(spp_iucn_all))
}


#############################################################################=
getHabitats <- function(sid, download_tries = 10) {
### function to extract just habitat from the IUCN API given the iucn_sid 
### (= Red.List.Species.ID).
### support function for generate_iucn_habitat_list().
  
  url <- sprintf('http://api.iucnredlist.org/details/%d/0', sid)
  htm <- file.path(dir_anx, sprintf('cache/iucn_details/%d.htm', sid))

  i <- 0
  if(!file.exists(htm)) cat(sprintf('No file found for %s.  Attempting to download from %s. ', htm, url))
  # else cat(sprintf('.htm file found for %s.  No need to download. \n', htm))
  while (!file.exists(htm) | (file.info(htm)$size == 0 & i < download_tries)) {
    download.file(url, htm, method = 'auto', quiet = TRUE, mode = 'wb')
    i=i+1
    cat(sprintf('%d... ', i))
  }
  if (file.info(htm)$size == 0) stop(sprintf('Only getting empty file for: %s \n', url))

  h <- htmlParse(htm)
  habitats = xpathSApply(h, '//li[@class ="system"]', xmlValue)
  return(setNames(rep(sid, length(habitats)), habitats))
}


#############################################################################=
generate_iucn_habitat_list <- function(spp_list = spp_iucn_all, reload = FALSE) {
### Applies the getHabitat() function to all lines within the spp_list
### dataframe, and creates a master list of habitat per species ID.
### If generating a new list, saves to git-annex then returns dataframe;
### otherwise reads from git-annex and returns.
  spp_hab_file <- file.path(dir_anx, scenario, 'intermediate/spp_iucn_habitats.csv')
  if (!file.exists(spp_hab_file) | reload) {
    if(!file.exists(spp_hab_file)) cat(sprintf('No species-habitat file found at %s. '))
    cat(sprintf('Generating species habitat list from IUCN scraped data.\n', spp_hab_file))
    print(system.time({    
      # r <- lapply(spp_list$iucn_sid, getHabitats) # DEBUG
      r <- mclapply(spp_list$iucn_sid, getHabitats, mc.cores = detectCores(), mc.preschedule = FALSE) 
            # runs faster across multiple cores, but no reporting out from within getHabitats()
    }))
    r <- unlist(r)
    spp_iucn_habitats <- data.frame(iucn_sid = r, habitat = names(r))
    cat(sprintf('Writing species-habitat list to:\n  %s\n', spp_hab_file))
    write_csv(spp_iucn_habitats, spp_hab_file)
  } else {
    cat(sprintf('Reading local species-habitat list from:\n  %s\n', spp_hab_file))
    spp_iucn_habitats <- read.csv(spp_hab_file, stringsAsFactors = FALSE)
  }
  return(invisible(spp_iucn_habitats))
}


#############################################################################=
get_mar_spp <- function(reload = FALSE) {
### Gets full IUCN list, and IUCN habitats list; inner_joins the two to create
### a marine-only IUCN list.
### Cleans up scientific names by trimming whitespace on ends and in middle.
### Cleans up pre-2001 IUCN categories into current categories.
### Removes infrarank listings, and drops those columns.
    # get distinct marine species
  spp_iucn_all      <- generate_iucn_all_spp_list()
  spp_iucn_habitats <- generate_iucn_habitat_list()
  
  df <- spp_iucn_all %>%
    inner_join(spp_iucn_habitats %>% 
                 filter(habitat == 'Marine'),
               by = 'iucn_sid')
  ### clean whitespace from ends and middle:
  df <- df %>% 
    mutate(sciname = gsub('\\s+', ' ', str_trim(sciname)))
           
  # remove infraranks: 30 subspecies records ('ssp.')
  #d = subset(d, infrarank.type!='ssp.', !names(d) %in% c('infrarank','infrarank.type','infrarank.authority'))
  df <- df %>%
    filter(infrarank.type != 'ssp.') %>%
    select(-infrarank, -infrarank.type, -infrarank.authority)
  
  # convert IUCN category from IUCN Red List Categories & Criteria v2.3 (1994-2000) to v3.1 (since 2001)
  df <- df %>%
    mutate(category = ifelse(category %in% c('LR/cd', 'LR/nt', 'LR/lc'), 'NT',
                             ifelse (category == 'LR/lc', 'LC',
                                     category)))
           
  df_dupes <- df[duplicated(df), ]
  df <- df %>% unique() # why so many duplicates?

  return(invisible(df))
}


#############################################################################=
getDetails = function(sid, download_tries = 10) {
### function to extract subpopulation information and population trend information
### for a species, given the IUCN species ID.
### example species Oncorhynchus nerka: sid=135301 # (parent) ## sid=135322 # (child)  # sid=4162
  url <- sprintf('http://api.iucnredlist.org/details/%d/0', sid)
  htm <- file.path(dir_anx, sprintf('cache/iucn_details/%d.htm', sid)) # htm = '135322.htm'
  
  i <- 0
  while (!file.exists(htm) | (file.info(htm)$size == 0 & i < download_tries)){
    download.file(url, htm, method = 'auto', quiet = TRUE, mode = 'wb')
    i <- i + 1
  }
  
  if (file.info(htm)$size == 0) stop(sprintf('Only getting empty file for: %s', url))
  h <- htmlParse(htm)
  subpops_href <- xpathSApply(h, '//li[@class ="subspecie"]/a', xmlGetAttr, 'href')
  subpops      <- as.integer(sub('/details/([0-9]+)/0', '\\1', subpops_href))  
  popn_trend   <- xpathSApply(h, '//div[@id="population_trend"]', xmlValue)
  
  # NOTE greater specificity 
  #   for http://www.iucnredlist.org/details/135322/0 -- United States (Alaska) 
  #   vs  http://api.iucnredlist.org/details/135322/0 -- United States
  #countries = xpathSApply(h, '//td[@class="label"][strong="Countries:"]/following-sibling::td/div/text()', xmlValue)    
  
  return(list(subpops = subpops, popn_trend = popn_trend))
}

#############################################################################=
get_trend_and_subpops <- function(df = spp_iucn_mar, reload = FALSE) {
### Create dataframes for subpopulation info and population trend info,
### from IUCN info scraped from web.  
### Subpops frame correlates parent ID with subpopulation IDs.  Trend frame includes
### species ID and population trend.
### Returns a combination of the two (full_join) with variables:
### iucn_sid (use as parent_id) | subpop_sid | popn_trend

  spp_trend_file <- file.path(dir_anx, 'tmp/trend_and_subpops.csv')
  if(!file.exists(spp_trend_file)) {
    cat('Creating trend and subpopulations list.\n')
    spp_subpop     <- data.frame() # initialize
    spp_popn_trend <- data.frame()
    
    i  <- 0
    ii <- nrow(df)
    
    for (sid in df$iucn_sid) { # sid = df$iucn_sid[3]
      sp <-  getDetails(sid)
      # subpop
      if (length(sp$subpops) > 0) {
        cat(sprintf('Found subpopulations for parent %d: %s\n', sid, paste(sp$subpops, collapse = ' ')))
        sp_subpop = data.frame(parent_sid = sid, subpop_sid = sp$subpops)
        spp_subpop = rbind(spp_subpop, sp_subpop)
      }
      # popn_trend
      if (length(sp$popn_trend) > 0) {
        sp_popn_trend <- data.frame(iucn_sid = sid, popn_trend = sp$popn_trend)
        spp_popn_trend = rbind(spp_popn_trend, sp_popn_trend)
      }  
      i <- i + 1
      if(i == round(i, -2)) cat(sprintf('Processed %d out of %d species.\n', i, ii))
    }
    cat(sprintf('Joining trends and subspecies data frames: %d and %d rows respectively.\n', nrow(spp_popn_trend), nrow(spp_subpop)))
    spp_trend_subpops <- full_join(spp_popn_trend, 
                                   spp_subpop, 
                                   by = c('iucn_sid' = 'parent_sid')) %>%
      unique()
    cat(sprintf('Writing trend and subpops file to: \n  %s\n', spp_trend_file))
    write_csv(spp_trend_subpops, spp_trend_file)
  } else {
    cat(sprintf('Reading trend and subpops file from: \n  %s\n', spp_trend_file))
    spp_trend_subpops <- read.csv(spp_trend_file, stringsAsFactors = FALSE)
  }
  return(spp_trend_subpops)
}


spp_iucn_mar <- get_mar_spp()

spp_trend_subpops <- get_trend_and_subpops(spp_iucn_mar)

spp_iucn_mar <- spp_iucn_mar %>%
  left_join(spp_trend_subpops, 
            by = 'iucn_sid')
spp_iucn_mar <- spp_iucn_mar %>%
  left_join(spp_trend_subpops %>%
              filter(!is.na(subpop_sid)) %>%
              select(-popn_trend) %>%
              rename(parent_sid = iucn_sid),
            by = c('iucn_sid' = 'subpop_sid'))

summary(spp_iucn_mar$popn_trend)
#    Unknown Decreasing     Stable Increasing       NA's 
#       5559       1338       1002        193        258 

spp_iucn_mar_file <- file.path(dir_anx, scenario, 'intermediate/spp_iucn_mar.csv')
cat(sprintf('Writing IUCN marine species file to: \n  %s\n', spp_iucn_mar_file))
write_csv(spp_iucn_mar, spp_iucn_mar_file)
