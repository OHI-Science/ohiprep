# Get all species from IUCN. Limit to those found in marine habitats.

# Adapted from Ben Best script for 2013

#Jamie Afflerbach

# # load packages - note this should be sourced by data_prep.R, so no need to load libraries already loaded from there.
# library(ohi)
# library(RCurl)
# library(XML)
# library(parallel)
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

#######################################################
# get all species
all_spp_file <- file.path(dir_anx, scenario, 'intermediate/spp_iucn_all.csv')
all_spp_url  <- 'http://api.iucnredlist.org/index/all.csv'
if (!file.exists(all_spp_file) | reload){
  cat(sprintf('Reading full IUCN redlist species from: %s\n', all_spp_url))
  spp_iucn_all <- read.csv(all_spp_url)                     # nrows = 122843
  spp_iucn_all <- spp_iucn_all[!duplicated(spp_iucn_all), ] # remove duplicates # nrows = 120484
  spp_iucn_all <- spp_iucn_all %>%
    rename(sciname  = Scientific.Name,
           iucn_sid = Red.List.Species.ID,
           category = Category)  
  cat(sprintf('Writing full IUCN redlist species list to: %s\n', all_spp_url))
  write_csv(spp_iucn_all, all_spp_file)
} else {
  cat(sprintf('Reading full IUCN redlist species list from: %s\n', all_spp_url))
  spp_iucn_all <- read.csv(all_spp_file)
}

# function to extract just habitat from the IUCN API given the iucn_sid (= Red.List.Species.ID)
getHabitats <- function(sid, download_tries = 10) {
  url <- sprintf('http://api.iucnredlist.org/details/%d/0', sid)
  htm <- file.path(dir_anx, sprintf('cache/iucn_details/%d.htm', sid))

  i <- 0
  if(!file.exists(htm)) cat(sprintf('No file found for %s.  Attempting to download from %s. ', htm, url))
  else cat(sprintf('.htm file found for %s.  No need to download. \n', htm))
  while (!file.exists(htm) | (file.info(htm)$size == 0 & i < download_tries)) {
    download.file(url, htm, method = 'auto', quiet = TRUE, mode = 'wb')
    i=i+1
    cat(sprintf('%d... ', i))
  }
  if (file.info(htm)$size == 0) stop(sprintf('Only getting empty file for: %s', url))
  cat('\n')
  
  h <- htmlParse(htm)
  habitats = xpathSApply(h, '//li[@class ="system"]', xmlValue)
  return(setNames(rep(sid, length(habitats)), habitats))
}

spp_hab_file <- file.path(dir_anx, scenario, 'intermediate/spp_iucn_habitats.csv')
if (!file.exists(spp_hab_file) | reload) {  
  print(system.time({    
    #r <- lapply(spp_iucn_all$iucn_sid, getHabitats) # DEBUG
    r <- mclapply(spp_iucn_all$iucn_sid, getHabitats, mc.cores = detectCores(), mc.preschedule = F) # took ~ 4 hrs on neptune
  }))
  r <- unlist(r)
  spp_iucn_habitats <- data.frame(iucn_sid = r, habitat = names(r))
  write_csv(spp_iucn_habitats, spp_hab_file)
} else {
  spp_iucn_habitats <- read.csv(spp_hab_file)
}

# check no remaining download errors. might need to manually run. should be 0:
download_errors <- spp_iucn_habitats %>%
  filter(is.na(habitat))
cat(sprintf('Download errors (habitat == NA) - should be zero: %d\n', download_errors))

# species without habitats assigned (n=67) - mostly birds (n=58)
sid_miss = setdiff(spp_iucn_all$iucn_sid, spp_iucn_habitats$iucn_sid)
print(addmargins(table(filter(spp_iucn_all, iucn_sid %in% sid.miss)$Class, useNA='ifany')))
#       AMPHIBIA           AVES CHONDRICHTHYES      CRUSTACEA     GASTROPODA  HOLOTHUROIDEA        INSECTA       MAMMALIA            Sum 
#              1             58              1              1              2              2              1              1             67

# get counts of species / subpopulations
print(addmargins(table(spp_iucn_habitats$habitat, useNA='ifany')))
#head(spp_iucn_habitats[is.na(spp_iucn_habitats$habitat),])
#  Freshwater      Marine Terrestrial         Sum 
#       25245        8380       50135       83760 

#######################################################################

# get distinct marine species
options('stringsAsFactors'=T)
spp_iucn_all = read.csv('tmp/spp_iucn_all.csv')
spp_iucn_habitats = read.csv('tmp/spp_iucn_habitats.csv')
d = merge(spp_iucn_all, subset(spp_iucn_habitats, habitat=='Marine'), by.x='Red.List.Species.ID', by.y='sid')
nrow(d) # 8380
length(unique(d$Scientific.Name)) # 8178
print(addmargins(table(d$Class, useNA='ifany')))
#     ACTINOPTERYGII           ANTHOZOA               AVES           BIVALVIA CEPHALASPIDOMORPHI        CEPHALOPODA      CHLOROPHYCEAE     CHONDRICHTHYES          CRUSTACEA         ECHINOIDEA             ENOPLA 
#               3332                842                839                 34                  4                195                  1               1113                256                  1                  1 
#    FLORIDEOPHYCEAE         GASTROPODA      HOLOTHUROIDEA           HYDROZOA            INSECTA         LILIOPSIDA      MAGNOLIOPSIDA           MAMMALIA        MEROSTOMATA             MYXINI       PHAEOPHYCEAE 
#                 58                806                369                 16                  1                 78                 64                170                  4                 76                 15 
#         POLYCHAETA     POLYPODIOPSIDA           REPTILIA      SARCOPTERYGII        ULVOPHYCEAE                Sum 
#                  2                  3                 97                  2                  1               8380
names(d) = tolower(names(d))
d = rename(d, c('red.list.species.id'='sid','scientific.name'='sciname'))
d = d[, !names(d) %in% c('primary')]

# remove infraranks: 30 subspecies records ('ssp.')
d = subset(d, infrarank.type!='ssp.', !names(d) %in% c('infrarank','infrarank.type','infrarank.authority'))
# CCO: subset dataframe 'd', rows != ssp, and deselect infrarank columns.
table(d$category)
#    CR    DD    EN    EW    EX    LC LR/cd LR/lc LR/nt    NT    VU 
#   147  2175   228     0    24  4557     6     5    12   518   678

# convert IUCN category from IUCN Red List Categories & Criteria v2.3 (1994-2000) to v3.1 (since 2001)
# http://en.wikipedia.org/wiki/Wikipedia:Conservation_status
#   LR/cd: Least Risk / Conservation Dependent -> NT
#   LR/nt: Least Risk / Near Threatened -> NT
#   LR/lc: Least Risk / Least Concern -> LC
x.cat = c('LR/cd'='NT','LR/nt'='NT','LR/lc'='LC')
for (i in 1:length(x.cat)){ # g = c('LR/cd'='NT','LR/nt'='NT','LR/lc'='LC')[1]
  idx = which(d$category==names(x.cat)[i])
  print(sprintf('%s: %d', x.cat[[i]], length(idx)))
  d$category[idx] = x.cat[[1]]  
}
d$category = factor(x=as.character(d$category), 
                                  levels=c('DD','LC','NT','VU','EN','CR','EX'), ordered=T)
table(d$category)
#   DD   LC   NT   VU   EN   CR   EX 
# 2175 4580  518  678  228  147   24

# categories used:
#   EX = Extinct (if not EX in 2012)           
#   CR = Critically Endangered
#   EN = Endangered
#   VU = Vulnerable
#   NT = Near Threatened
#   LC = Least Concern
# other:
#   DD = Data deficient
#   NE = Not evaluated
# missing:
#   EW = Extinct in the wild
#   PE = Probably extinct (informal)
#   PEW = Probably extinct in the wild (informal)

# # only include EXtinct species if previously showed up in 2012 not as EXtinct
# # UPDATE DECISION 2013-08-13: including ALL EXtinct species for 2013 vs not doing so last year because of baseline reference issues.
# spp_2012 = read.csv('tmp/spp_2012.csv')
# sci.ex = as.character(subset(spp_2012, as.character(scientific) %in% subset(d, category=='EX', sciname, drop=T), scientific, drop=T))
# sci.ex
# # [1] "Oncorhynchus nerka" # only one species (sockeye salmon), and just for 5 local subpopulations
# subset(d, category=='EX' & sciname %in% sci.ex)
# d = subset(d, category !='EX' | sciname %in% sci.ex)
# table(d$category)
#   DD   LC   NT   VU   EN   CR   EX 
# 2175 4580  518  678  228  147    5

table(d$category)
#   DD   LC   NT   VU   EN   CR   EX 
# 2175 4580  518  678  228  147   24

nrow(subset(d, category!='DD')) # 6175
write.csv(d, 'tmp/spp_iucn_marine.csv', row.names=F, na='')

# get list of unique species and their global extinction status
d = read.csv('tmp/spp_iucn_marine.csv')

# function to extract just habitat from the IUCN API given the Red.List.Species.ID
getDetails = function(sid, download.tries=10){
  # example species Oncorhynchus nerka: sid=135301 # (parent) ## sid=135322 # (child)  # sid=4162
  url = sprintf('http://api.iucnredlist.org/details/%d/0', sid)
  htm = sprintf('cache/iucn_details/%d.htm', sid) # htm = '135322.htm'
  i=0
  while ( !file.exists(htm) | (file.info(htm)$size==0 & i<download.tries) ){
    download.file(url, htm, method='auto', quiet=T, mode='wb')
    i=i+1
  }
  if (file.info(htm)$size==0) stop(sprintf('Only getting empty file for: %s', url))
  h = htmlParse(htm)
  subpopulations.href = xpathSApply(h, '//li[@class ="subspecie"]/a', xmlGetAttr, 'href')
  subpopulations = as.integer(sub('/details/([0-9]+)/0', '\\1', subpopulations.href))  
  countries = xpathSApply(h, '//ul[@class="country_distribution"]/li/ul/li', xmlValue)
  popn_trend = xpathSApply(h, '//div[@id="population_trend"]', xmlValue)
  
  # NOTE greater specificity 
  #   for http://www.iucnredlist.org/details/135322/0 -- United States (Alaska) 
  #   vs  http://api.iucnredlist.org/details/135322/0 -- United States
  #countries = xpathSApply(h, '//td[@class="label"][strong="Countries:"]/following-sibling::td/div/text()', xmlValue)    
  
  return(list(subpopulations=subpopulations, countries=countries, popn_trend=popn_trend))
}

