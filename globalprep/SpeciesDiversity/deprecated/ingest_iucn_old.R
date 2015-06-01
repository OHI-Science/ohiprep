
dir_git <- '~/github/ohiprep'
setwd(dir_git)

source('src/R/common.R') # set pathnames, load common libraries

goal       <- 'globalprep/SpeciesDiversity'
prod       <- 'v2015_sandbox'
dir_loc    <- file.path(dir_git, goal, prod)
dir_anx    <- file.path(dir_neptune_data, 'git-annex', goal) #, prod)
dir_tmp    <- file.path(dir_loc, 'tmp') # ??? dir_loc was dir_anx

# Get all species from IUCN. Limit to those found in marine habitats.

# Adapted from Ben Best script for 2013

#Jamie Afflerbach

# load packages
library(ohicore) # ??? was library(ohi)
library(RCurl)
library(XML)
library(parallel)

# # set working directory
# wd = '/var/data/ohi/git-annex/globalprep/SpeciesDiversity' # (Mac/Linux)
# setwd(wd)

# flags & vars
reload <- FALSE

### set up cache and tmp dir -
### * delete cache and recreate if reload == TRUE
if (dir.exists(file.path(dir_loc, 'cache/iucn_details')) & reload) # ??? dir_loc was dir_anx
  unlink(file.path(dir_loc, 'cache'), recursive = TRUE) #  ??? dir_loc was dir_anx
if (!dir.exists(file.path(dir_loc, 'cache/iucn_details')))
  dir.create(file.path(dir_loc, 'cache/iucn_details'), recursive = TRUE)
if (!dir.exists(dir_tmp))
  dir.create(dir_tmp, recursive = TRUE)

### Load full IUCN species list
### * if not yet created, or reload == TRUE, download from IUCN site.
### * otherwise, read from existing file.
if (!file.exists(file.path(dir_tmp, 'spp_iucn_all.csv')) | reload) {
  spp_iucn_all <- read.csv('http://api.iucnredlist.org/index/all.csv')           # nrows = 122843 5/4/2015
  spp_iucn_all <- spp_iucn_all[!duplicated(spp_iucn_all), ] # remove duplicates  # nrows = 120484 5/4/2015
  write.csv(spp_iucn_all, file.path(dir_tmp, 'spp_iucn_all.csv'), row.names = FALSE, na = '')
} else {
  spp_iucn_all <- read.csv(file.path(dir_tmp, 'spp_iucn_all.csv')) # old file
}

# function to extract just habitat from the IUCN API given the Red.List.Species.ID
getHabitats = function(sid, download_tries = 10){
  url <- sprintf('http://api.iucnredlist.org/details/%d/0', sid)
  htm <- sprintf('%s/cache/iucn_details/%d.htm', dir_loc, sid) # dir_loc was dir_anx
  
  i <- 0
  while ( !file.exists(htm) | (file.info(htm)$size==0 & i < download_tries) ){
    download.file(url, htm, method='auto', quiet=T, mode='wb')
    i <- i+1
  }
  
  if (file.info(htm)$size==0) 
    stop(sprintf('Only getting empty file for: %s', url))
  
  h <- htmlParse(htm) 
  habitats <- xpathSApply(h, '//li[@class ="system"]', xmlValue) 
  
  return(setNames(rep(sid, length(habitats)), habitats))
}

# spp_iucn_all = head(spp_iucn_all, 1000) # DEBUG
# unlink('tmp/spp_iucn_habitats.csv')    # DEBUG
#options(error=recover)       # 46181060 # DEBUG    

### identify location of habitat info; if not already created, or reload == TRUE,
### extract habitat info using getHabitats() defined above.
#file_loc_hab <- file.path(dir_tmp, 'spp_iucn_habitats.csv') # for local copy... 
#file_loc_hab <- file.path(dir_anx, 'tmp/spp_iucn_habitats.csv') # to download old from git-annex


if (!file.exists(file_loc_hab) | reload){  
  print(system.time({    
    #r = lapply(spp_iucn_all$Red.List.Species.ID, getHabitats) # DEBUG
    r <- mclapply(spp_iucn_all$Red.List.Species.ID, getHabitats, mc.cores=detectCores(), mc.preschedule = F) # took ~ 4 hrs on neptune
    # ??? for each element of spp_iucn_all, applies getHabitats(), and spreads this function across multiple cores.
    # ??? since many of these already exist in git-annex, perhaps re-tool to just collect new habitats?
  }))
  r <- unlist(r)
  
  spp_iucn_habitats <- data.frame(sid = r, habitat = names(r))
  write.csv(spp_iucn_habitats, file_loc_hab, row.names=F, na='')
} else {
  spp_iucn_habitats <- read.csv(file_loc_hab)
}

# check no remaining download errors. might need to manually run. should be 0:
subset(spp_iucn_habitats, is.na(habitat))

# species without habitats assigned (n=67) - mostly birds (n=58)
sid.miss <- setdiff(spp_iucn_all$Red.List.Species.ID, spp_iucn_habitats$sid)
print(addmargins(table(subset(spp_iucn_all, Red.List.Species.ID %in% sid.miss)$Class, useNA='ifany')))
#       AMPHIBIA           AVES CHONDRICHTHYES      CRUSTACEA     GASTROPODA  HOLOTHUROIDEA        INSECTA       MAMMALIA            Sum 
#              1             58              1              1              2              2              1              1             67

# get counts of species / subpopulations
print(addmargins(table(spp_iucn_habitats$habitat, useNA='ifany')))
#head(spp_iucn_habitats[is.na(spp_iucn_habitats$habitat),])
#  Freshwater      Marine Terrestrial         Sum 
#       25245        8380       50135       83760 

# # get distinct marine species
# options('stringsAsFactors' = TRUE)
# spp_iucn_all      = read.csv(file.path(dir_tmp, 'spp_iucn_all.csv'))
# spp_iucn_habitats = read.csv(file.path(dir_tmp, 'spp_iucn_habitats.csv'))

d <- inner_join(
        spp_iucn_all, 
        filter(spp_iucn_habitats, habitat=='Marine'),
        by = c('Red.List.Species.ID' = 'sid'))
# ??? use descriptive name? d --> data?

nrow(d) # 19779

length(unique(d$Scientific.Name)) # 19135

print(addmargins(table(d$Class, useNA='ifany')))
# old:
#     ACTINOPTERYGII           ANTHOZOA               AVES           BIVALVIA CEPHALASPIDOMORPHI        CEPHALOPODA      CHLOROPHYCEAE     CHONDRICHTHYES          CRUSTACEA         ECHINOIDEA             ENOPLA 
#               3332                842                839                 34                  4                195                  1               1113                256                  1                  1 
#    FLORIDEOPHYCEAE         GASTROPODA      HOLOTHUROIDEA           HYDROZOA            INSECTA         LILIOPSIDA      MAGNOLIOPSIDA           MAMMALIA        MEROSTOMATA             MYXINI       PHAEOPHYCEAE 
#                 58                806                369                 16                  1                 78                 64                170                  4                 76                 15 
#         POLYCHAETA     POLYPODIOPSIDA           REPTILIA      SARCOPTERYGII        ULVOPHYCEAE                Sum 
#                  2                  3                 97                  2                  1               8380
names(d) <- tolower(names(d))
d <- rename(d, c('red.list.species.id'='sid','scientific.name'='sciname'))
d <- d[, !names(d) %in% c('primary')]

# remove infraranks: 30 subspecies records ('ssp.')
d <- d %>% 
  filter(infrarank.type != 'ssp.') %>%
  select(-infrarank, -infrarank.type, -infrarank.authority)

table(d$category)
# test:
#    CR    DD    EN    EW    EX    LC LR/cd LR/lc LR/nt    NT    VU 
#   216  3662   360     0    32 13410     5     8    29   881  1075 
# old:
#    CR    DD    EN    EW    EX    LC LR/cd LR/lc LR/nt    NT    VU 
#   147  2175   228     0    24  4557     6     5    12   518   678

# convert IUCN category from IUCN Red List Categories & Criteria v2.3 (1994-2000) to v3.1 (since 2001)
# http://en.wikipedia.org/wiki/Wikipedia:Conservation_status
#   LR/cd: Least Risk / Conservation Dependent -> NT
#   LR/nt: Least Risk / Near Threatened -> NT
#   LR/lc: Least Risk / Least Concern -> LC

# x.cat = c('LR/cd'='NT','LR/nt'='NT','LR/lc'='LC')
# for (i in 1:length(x.cat)){ # g = c('LR/cd'='NT','LR/nt'='NT','LR/lc'='LC')[1]
#   idx = which(d$category==names(x.cat)[i])
#   print(sprintf('%s: %d', x.cat[[i]], length(idx)))
#   d$category[idx] = x.cat[[1]]  
# }
# d$category = factor(x=as.character(d$category), 
#                     levels=c('DD','LC','NT','VU','EN','CR','EX'), ordered=T)
# ??? replace above with:
d <- d %>% mutate(
  category = as.character(category),
  category = ifelse(category == 'LR/cd', 'NT', category),
  category = ifelse(category == 'LR/nt', 'NT', category),
  category = ifelse(category == 'LR/lc', 'LC', category))

table(d$category)
# test:
#    CR    DD    EN    EX    LC    NT    VU 
#   216  3662   360    32 13410   923  1075 
# old:
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
# test:
#    CR    DD    EN    EX    LC    NT    VU 
#   216  3662   360    32 13410   923  1075 
# old:
#    DD    LC    NT    VU    EN    CR    EX 
#  2175  4580   518   678   228   147    24

write.csv(d, file.path(dir_tmp, 'spp_iucn_marine_preDD.csv'), row.names=F, na='')


# get list of unique species and their global extinction status
#d = read.csv('tmp/spp_iucn_marine.csv')

# function to extract just habitat from the IUCN API given the Red.List.Species.ID
getDetails = function(sid, download_tries = 10) {
  # example species Oncorhynchus nerka: sid = 135301 # (parent) ## sid = 135322 # (child)  # sid = 4162
  url <- sprintf('http://api.iucnredlist.org/details/%d/0', sid)
  htm <- sprintf('%s/cache/iucn_details/%d.htm', dir_loc, sid) # htm = '135322.htm'
  
  i <- 0
  
  while (!file.exists(htm) | (file.info(htm)$size == 0 & i < download_tries)) {
    download.file(url, htm, method = 'auto', quiet = TRUE, mode = 'wb')
    i <- i + 1 
  }
  
  if (file.info(htm)$size == 0) 
    stop(sprintf('Only getting empty file for: %s', url))
  
  h <- htmlParse(htm)
  subpopulations_href <- xpathSApply(h, '//li[@class = "subspecie"]/a', xmlGetAttr, 'href')
  subpopulations      <- as.integer(sub('/details/([0-9]+)/0', '\\1', subpopulations_href))  
  countries           <- xpathSApply(h, '//ul[@class = "country_distribution"]/li/ul/li', xmlValue)
  popn_trend          <- xpathSApply(h, '//div[@id = "population_trend"]', xmlValue)
  
  # NOTE greater specificity 
  #   for http://www.iucnredlist.org/details/135322/0 -- United States (Alaska) 
  #   vs  http://api.iucnredlist.org/details/135322/0 -- United States
  #countries = xpathSApply(h, '//td[@class="label"][strong="Countries:"]/following-sibling::td/div/text()', xmlValue)    
  
  return(list(subpopulations = subpopulations, countries = countries, popn_trend = popn_trend))
}

### ??? For each species in the list, get list of all subpopulations and countries
suppressWarnings(rm(list = c('spp_subpop','spp_countries','spp_popn_trend')))
for (sid in d$sid){ # sid = d$sid[1] # sid
  sp <- getDetails(sid)
  
  # subpop
  if (length(sp$subpopulations) > 0) {
    sp_subpop <- data.frame(parent_sid = sid, sid = sp$subpopulations)
    if (!exists('spp_subpop')) {
      spp_subpop <- sp_subpop
    } else {
      spp_subpop <- bind_rows(spp_subpop, sp_subpop)
    }
  }
  
  # countries
  if (length(sp$countries) > 0) {
    sp_countries <- data.frame(sid = sid, country = sp$countries)
    if (!exists('spp_countries')) {
      spp_countries <- sp_countries
    } else {
      spp_countries <- bind_rows(spp_countries, sp_countries)
    }
  }
  
  # popn_trend
  if (length(sp$popn_trend) > 0) {
    sp_popn_trend <- data.frame(sid = sid, popn_trend = sp$popn_trend)
    if (!exists('spp_popn_trend')) {
      spp_popn_trend <- sp_popn_trend    
    } else {
      spp_popn_trend <- bind_rows(spp_popn_trend, sp_popn_trend)
    }
  }  
}

# assign popn_trend to d, before subsetting into subpopulations and global species and 
# ??? old way:
  d$popn_trend <- NA
  d[match(spp_popn_trend$sid, d$sid), 'popn_trend'] <- as.character(spp_popn_trend$popn_trend) 
  d$popn_trend <- factor(d$popn_trend, c('Unknown', 'Decreasing', 'Stable', 'Increasing'), ordered = TRUE)
# ??? can we do this with mutate?  e.g. new way:
# d2 <- d %>% 
#   left_join(spp_popn_trend, by = c('sid' = 'sid') %>% 
#   mutate(popn_trend = as.character(popn_trend)) # new way?


summary(d$popn_trend)
#    Unknown Decreasing     Stable Increasing       NA's 
#       5559       1338       1002        193        258 

# manual subpopulations discovered later, somehow not listed in the IUCN details page of parent 
# (again difference b/n API and WWW versions), after defining spp_global with the following:
#   subset(spp_global, sciname %in% spp_global$sciname[duplicated(spp_global$sciname)])
spp_subpop_manual = c('Pristis pectinata'        = 18175,
                      'Centrophorus moluccensis' = 42838,
                      'Pristis pristis'          = 18584848)

for (i in 1:length(spp_subpop_manual)) { # i=1
  parent_sid = spp_subpop_manual[[i]]
  sid <- d %>% 
    filter(sciname %in% names(spp_subpop_manual)[i] & sid!=parent_sid) %>% 
    select(sid)
  sp_subpop <- data.frame(parent_sid = parent_sid, 
                          sid        = sid)

  spp_subpop <- bind_rows(spp_subpop, sp_subpop)
}

write.csv(spp_subpop   , file.path(dir_tmp, 'spp_iucn_marine_subpopulations.csv', row.names = FALSE, na = '')
write.csv(spp_countries, file.path(dir_tmp, 'spp_iucn_marine_countries.csv',      row.names = FALSE, na = '')

# TODO: next time remove data deficient (DD) first, before using to extract rangemaps, for expediency
nrow(d %>% filter(category != 'DD')) # 6175 (old) test: 16016 (lots of new LCs)
d <- d %>% filter(category != 'DD')


# get list of subpopulations and countries, for overriding global category with local one
#   TODO: ISSUE: can have more than one subpopulation with different categories in the same country, eg 135301 sockey salmon
# tmp/spp_iucn_marine_subpop.csv: sid, parent_sid, sciname, category
# tmp/spp_iucn_marine_subpop_countries.csv: sid, country
# spp_subpop1 <- merge(spp_subpop, d[, c('sid','sciname','category','popn_trend')], by = 'sid')
spp_subpop2 <- left_join(
  spp_sub_pop, 
  d %>% select(sid, sciname, category, popn_trend), by = 'sid')

spp_subpop_countries <- spp_countries %>% 
  filter(sid %in% spp_subpop$sid)

cat(sprintf('\nFound %d marine subpopulations for %d species in %d countries.\n', nrow(spp_subpop), length(unique(spp_subpop$parent_sid)), nrow(spp_subpop_countries)))
# Found 177 marine subpopulations for 51 species in 736 countries.
write.csv(spp_subpop          , file.path(dir_tmp, 'spp_iucn_marine_subpop.csv'          , row.names=F, na='')
write.csv(spp_subpop_countries, file.path(dir_tmp, 'spp_iucn_marine_subpop_countries.csv', row.names=F, na='')

# get list of all master species populations (ie not a subpopulation, used to extract IUCN rangemaps and apply category globally) 
# tmp/spp_iucn_marine_global.csv: sid, sciname, category
# tmp/spp_iucn_marine_global_countries.csv: sid, country
spp_global           = d %>%
  filter(!sid %in% spp_subpop$sid)

spp_global_countries = spp_countries %>%
  filter(sid  %in% spp_global$sid)

cat(sprintf('\nFound %d marine global species in %d countries.\n', nrow(spp_global), nrow(spp_global_countries)))
# Found 8173 marine global species in 120469 countries.

# check for duplicate scientific names
filter(spp_global, sciname %in% spp_global$sciname[duplicated(spp_global$sciname)])

# assign popn_trend to spp_global
# ???


# Found 8181 marine global species in 120,563 countries.
write.csv(spp_global          , 'tmp/spp_iucn_marine_global.csv'          , row.names=F, na='')
write.csv(spp_global_countries, 'tmp/spp_iucn_marine_global_countries.csv', row.names=F, na='')

# get distinct countries for doing an eez lookup (and later distribution for global species or catgory override for subpopulation)
spp_countries_distinct = as.data.frame(table(subset(spp_countries, sid %in% d$sid, country)))
spp_countries_distinct = rename(spp_countries_distinct[order(as.character(spp_countries_distinct[[1]])),],
                                c('Var1'='country','Freq'='count'))
write.csv(spp_countries_distinct, 'tmp/spp_iucn_marine_distinct_countries.csv', row.names=F, na='')


# supplement with Davies & Baum (2012) ------------------------------------

# clean excess white spaces
spp_db12 = read.csv('raw/DaviesBaum2012/DaviesBaum2012.csv', stringsAsFactors=F)
require(gdata)
for (fld in names(spp_db12)){ # fld = names(spp_db12)[1] # fld = 'Scientific_name'
  if (class(spp_db12[[fld]])=='character'){
    spp_db12[[fld]] = gdata::trim(spp_db12[[fld]])
  }
}
# lowercase field names
names(spp_db12) = tolower(names(spp_db12))
write.csv(spp_db12, 'tmp/DaviesBaum2012.csv', row.names=F, na='')

# find spp not already in IUCN
spp_db12 = read.csv('tmp/DaviesBaum2012.csv', stringsAsFactors=F)
nrow(spp_db12) # 166 populations
length(unique(spp_db12$scientific_name)) # 94 unique species
spp_db12_notiucn = subset(spp_db12, !scientific_name %in% d$sciname)
cat(sprintf('\nFound %d populations for %d species with Davies and Baum (2012) novel to IUCN species.\n', nrow(spp_db12_notiucn), length(unique(spp_db12_notiucn$scientific_name))))
# Found 110 populations for 69 species with Davies and Baum (2012) novel to IUCN species.
write.csv(spp_db12_notiucn, 'tmp/spp_DaviesBaum2012_notIUCN.csv', row.names=F, na='')


