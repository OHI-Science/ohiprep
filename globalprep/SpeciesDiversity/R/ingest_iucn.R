# Get all species from IUCN. Limit to those found in marine habitats.

# Adapted from Ben Best script for 2013

#Jamie Afflerbach

# # load packages - note this should be sourced by data_prep.R, so no need to load libraries already loaded from there.
# library(ohi)
library(RCurl)
library(XML)
library(parallel)
# library(plyr)
# 
# library(foreign)
# library(data.table) # for fread()
# library(sp)
# library(rgdal)
# library(raster)
# library(readr)      # for read_csv()
# 
# setwd('~/github/ohiprep')
# source('src/R/common.R')
# 
# goal     <- 'globalprep/SpeciesDiversity'
# scenario <- 'v2015'
# dir_anx  <- file.path(dir_neptune_data, 'git-annex', goal) 
# dir_git  <- file.path('~/github/ohiprep', goal)
# 
# source(file.path(dir_git, 'R/spp_fxn.R'))
# source(file.path(dir_git, 'R/ico_fxn.R'))
# # SPP-specific and ICO-specific functions

# # set working directory
# wd = '/var/data/ohi/git-annex/globalprep/SpeciesDiversity' # (Mac/Linux)
# setwd(wd)

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
ingest_iucn_errorcheck1 <- function() {
  # check no remaining download errors. might need to manually run. should be 0:
  cat(sprintf('Download errors (habitat == NA) - should be zero: %d\n', 
              nrow(spp_iucn_habitats %>% filter(is.na(habitat)))))
  
  # species without habitats assigned (n=67) - mostly birds (n=58)
  sid_miss <- setdiff(spp_iucn_all$iucn_sid, spp_iucn_habitats$iucn_sid)
  print(addmargins(table(filter(spp_iucn_all, iucn_sid %in% sid_miss)$class, useNA = 'ifany')))
  #   BIVALVIA    GASTROPODA MAGNOLIOPSIDA      REPTILIA           Sum 
  #          2             2             3             1             8 
  # get counts of species / subpopulations
  print(addmargins(table(spp_iucn_habitats$habitat, useNA='ifany')))
  #   Freshwater      Marine Terrestrial         Sum 
  #        43501       19779       77225      140505 
}

ingest_iucn_errorcheck1()

#####################

get_mar_spp <- function(reload = FALSE) {
### Gets full IUCN list, and IUCN habitats list; inner_joins the two to create
### a marine-only IUCN list.
### Cleans up scientific names by trimming whitespace on ends and in middle.
### Cleans up pre-2001 IUCN categories into current categories.
### Removes infrarank listings, and drops those columns.
  spp_iucn_marine_file <- file.path(dir_anx, 'tmp/spp_iucn_marine.csv')
  
  if(!file.exists(spp_iucn_marine_file) | reload) {
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

    cat(sprintf('Writing IUCN marine species file to: \n  %s\n', spp_iucn_marine_file))
    write_csv(df, spp_iucn_marine_file)
  } else {
    # get list of unique species and their global extinction status
    cat(sprintf('Reading IUCN marine species file from: \n  %s\n', spp_iucn_marine_file))
    df <- read.csv(spp_iucn_marine_file, stringsAsFactors = FALSE)
  }
  return(invisible(df))
}

spp_iucn_mar <- get_mar_spp()

# function to extract just habitat from the IUCN API given the Red.List.Species.ID
getDetails = function(sid, download_tries = 10) {
  # example species Oncorhynchus nerka: sid=135301 # (parent) ## sid=135322 # (child)  # sid=4162
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

get_trend_and_subpops <- function(df = spp_iucn_mar) {
  # get list of all subpopulations and countries
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
  spp_trend_subpops1 <- full_join(spp_popn_trend, 
                                 spp_subpop, 
                                 by = c('iucn_sid' = 'parent_sid')) %>%
    unique()
  write_csv(spp_trend_subpops, file.path(dir_anx, 'tmp/trend_and_subpops.csv'))
  return(spp_trend_subpops)
}

spp_trend_subpops <- get_trend_and_subpops(spp_iucn_mar)

# assign popn_trend to d, before subsetting into subpopulations and global species and 
spp_iucn_mar <- spp_iucn_mar %>%
  left_join(spp_trend_subpops, 
            by = 'iucn_sid')

summary(spp_iucn_mar$popn_trend)
#    Unknown Decreasing     Stable Increasing       NA's 
#       5559       1338       1002        193        258 


# manual subpopulations discovered later, somehow not listed in the IUCN details page of parent 
# (again difference b/n API and WWW versions), after defining spp_global with the following:
#   subset(spp_global, sciname %in% spp_global$sciname[duplicated(spp_global$sciname)])
spp_subpop.manual = c('Pristis pectinata'        = 18175,
                      'Centrophorus moluccensis' = 42838,
                      'Pristis pristis'          = 18584848)
for (i in 1:length(spp_subpop.manual)) { # i=1
  parent_sid = spp_subpop.manual[[i]]
  sp_subpop = data.frame(parent_sid=parent_sid, 
                         sid=subset(d, sciname %in% names(spp_subpop.manual)[i] & sid!=parent_sid, sid, drop=T))
  spp_subpop = rbind(spp_subpop, sp_subpop)
}
#write.csv(spp_subpop   , 'tmp/spp_iucn_marine_subpopulations.csv', row.names=F, na='')
#write.csv(spp_countries, 'tmp/spp_iucn_marine_countries.csv', row.names=F, na='')

# TODO: next time remove data deficient (DD) first, before using to extract rangemaps, for expediency



# get list of subpopulations and countries, for overriding global category with local one
#   TODO: ISSUE: can have more than one subpopulation with different categories in the same country, eg 135301 sockey salmon
# tmp/spp_iucn_marine_subpop.csv: sid, parent_sid, sciname, category
# tmp/spp_iucn_marine_subpop_countries.csv: sid, country
spp_subpop = merge(spp_subpop, d[, c('sid','sciname','category','popn_trend')], by='sid')
spp_subpop_countries = subset(spp_countries, sid %in% spp_subpop$sid)
cat(sprintf('\nFound %d marine subpopulations for %d species in %d countries.\n', nrow(spp_subpop), length(unique(spp_subpop$parent_sid)), nrow(spp_subpop_countries)))
# Found 177 marine subpopulations for 51 species in 736 countries.
write.csv(spp_subpop          , 'tmp/spp_iucn_marine_subpop.csv'          , row.names=F, na='')
write.csv(spp_subpop_countries, 'tmp/spp_iucn_marine_subpop_countries.csv', row.names=F, na='')


