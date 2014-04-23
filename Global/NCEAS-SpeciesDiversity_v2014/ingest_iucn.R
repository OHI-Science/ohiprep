# Get all species from IUCN. Limit to those found in marine habitats.
# Now using 2013.1 as of July 1st, 2013


# load packages
library(RCurl)
library(XML)
library(parallel)
library(plyr)
library(dplyr)

# paths
source('src/R/common.R')
# working dir in local github repo
wd = file.path(getwd(), 'Global/NCEAS-SpeciesDiversity_v2014')
# data dir
dd = file.path(dir_neptune_data, 'git-annex/Global/NCEAS-SpeciesDiversity_v2014')
# cache dir
cd = file.path(dd, 'cache')
# files
spp_iucn_all_csv      = file.path(cd, 'spp_iucn_all.csv')
spp_iucn_habitats_csv = file.path(cd, 'spp_iucn_habitats.csv')
# flags
reload = F

# cache dirs
if (file.exists(file.path(cd, 'iucn_details')) & reload) unlink(file.path(cd, 'iucn_details'), recursive=T)
dir.create(file.path(cd, 'iucn_details'), showWarnings=F, recursive=T)

# get all species
if (!file.exists(spp_iucn_all_csv) | reload){
  spp_iucn_all = read.csv('http://api.iucnredlist.org/index/all.csv')         # 2013 nrow = 72,329; 2014 nrow = 112,455
  write.csv(spp_iucn_all, file.path(cd, 'spp_iucn_all_plusnonprimary.csv'), row.names=F, na='')
  
  # remove duplicates
  spp_iucn_all = subset(spp_iucn_all, Primary=='true')   # nrow(spp_iucn_all) #                     2014 nrow = 73,679 (without duplicates)
  write.csv(spp_iucn_all, spp_iucn_all_csv, row.names=F, na='')
} else {
  spp_iucn_all = read.csv(spp_iucn_all_csv)
}


# function to extract just habitat from the IUCN API given the Red.List.Species.ID
getHabitats = function(sid, download.tries=10){
  url = sprintf('http://api.iucnredlist.org/details/%d/0', sid)
  htm = sprintf('%s/iucn_details/%d.htm', cd, sid)
  i=0
  while ( !file.exists(htm) | (file.info(htm)$size==0 & i<download.tries) ){
    download.file(url, htm, method='auto', quiet=T, mode='wb')
    i=i+1
  }
  if (file.info(htm)$size==0) stop(sprintf('Only getting empty file for: %s', url))
  h = htmlParse(htm)
  habitats = xpathSApply(h, '//li[@class ="system"]', xmlValue)
  return(setNames(rep(sid, length(habitats)), habitats))
}

# for (f in c('spp_iucn_habitats','spp_iucn_marine','spp_iucn_marine_global',
#             'spp_iucn_marine_global_countries','spp_iucn_marine_subpop','spp_iucn_marine_subpop_countries')){
#   file.rename(sprintf('%s/%s.csv', cd, f), sprintf('%s/%s_plusnonprimary.csv', cd, f))
# }

if (!file.exists(spp_iucn_habitats_csv) | reload){  
  print(system.time({    
    r = mclapply(spp_iucn_all$Red.List.Species.ID, getHabitats, mc.cores=detectCores(), mc.preschedule=F) # took ~ 4 hrs on neptune
  }))
  r = unlist(r)
  spp_iucn_habitats = data.frame(sid = r, habitat = names(r))
  write.csv(spp_iucn_habitats, spp_iucn_habitats_csv, row.names=F, na='')
} else {
  spp_iucn_habitats = read.csv(spp_iucn_habitats_csv)
  
  # check for number of habitats with duplicate species ids
  head(spp_iucn_habitats)
  table(duplicated(spp_iucn_habitats$sid))
  # 2014:
  #    FALSE   TRUE 
  #   73,677 11,551   
  spp_iucn_habitats = subset(spp_iucn_habitats, !duplicated(spp_iucn_habitats$sid))
  write.csv(spp_iucn_habitats, spp_iucn_habitats_csv, row.names=F, na='')
}

# check no remaining download errors. might need to manually run. should be 0:
print(subset(spp_iucn_habitats, is.na(habitat)), row.names=F)

# species without habitats assigned (n=67) - mostly birds (n=58)
sid.miss = setdiff(spp_iucn_all$red.list.species.id, spp_iucn_habitats$sid)
cat(sid.miss)
print(addmargins(table(as.character(subset(spp_iucn_all, red.list.species.id %in% sid.miss, class, drop=T)), useNA='ifany')), row.names=F)
# 2013
#       AMPHIBIA           AVES CHONDRICHTHYES      CRUSTACEA     GASTROPODA  HOLOTHUROIDEA        INSECTA       MAMMALIA            Sum 
#              1             58              1              1              2              2              1              1             67

# 2014:
#           BIVALVIA         GASTROPODA           REPTILIA                Sum
#                  2                  2                  1                  5
# 2014 (without duplicates):
#           BIVALVIA GASTROPODA   REPTILIA        Sum 
#                  1          2          1          4

# get counts of species / subpopulations
print(addmargins(table(spp_iucn_habitats$habitat, useNA='ifany')), row.names=F)
#head(spp_iucn_habitats[is.na(spp_iucn_habitats$habitat),])
# 2013 (without duplicate removal):
#   Freshwater      Marine  Terrestrial          Sum 
#       25,245       8,380       5,0135       83,760 
#
# 2014 (with duplicate removal):
#   Freshwater      Marine  Terrestrial          Sum 
#       15,565       6,635       51,475       73,677 
#
# 2014 (with duplicate removal 2):
#  Freshwater      Marine Terrestrial         Sum 
#       25,356        8,395      51,475       85,226


# get distinct marine species
options('stringsAsFactors'=T)
spp_iucn_all      = read.csv(spp_iucn_all_csv)
spp_iucn_habitats = read.csv(spp_iucn_habitats_csv)
names(spp_iucn_all) = tolower(names(spp_iucn_all))
# paste(setdiff(tolower(names(spp_iucn_all)), c('primary', 'red.list.species.id', 'scientific.name')), collapse=',')
d = spp_iucn_habitats %.%
  filter(habitat=='Marine') %.%
  merge(
    spp_iucn_all %.%
      select(
        sid     = red.list.species.id,
        sciname = scientific.name,
        class,order,family,genus,species,authority,infrarank,infrarank.type,infrarank.authority,modified.year,category,criteria),
    by='sid')

cat(sprintf('\nRows of Marine species: %d\nUnique Marine species : %d\n\n', nrow(d), length(unique(d$sciname))))
# 2013 (without duplicate removal):
#   Rows of Marine species:  8,380
#   Unique Marine species :  8,178
#
# 2014 (before duplicate removal):
#   Rows of Marine species: 79,382
#   Unique Marine species : 16,315
#
# 2014 (after duplicate removal):
#   Rows of Marine species:  6,635
#   Unique Marine species :  6,556

# 2014 (after duplicate removal 2):
#   Rows of Marine species:  8,395
#   Unique Marine species :  8,186


print(addmargins(table(as.character(d$class))), row.names=F)
# 2013 (without duplicate removal)::
#     ACTINOPTERYGII           ANTHOZOA               AVES           BIVALVIA CEPHALASPIDOMORPHI        CEPHALOPODA      CHLOROPHYCEAE     CHONDRICHTHYES          CRUSTACEA         ECHINOIDEA             ENOPLA 
#               3332                842                839                 34                  4                195                  1               1113                256                  1                  1 
#    FLORIDEOPHYCEAE         GASTROPODA      HOLOTHUROIDEA           HYDROZOA            INSECTA         LILIOPSIDA      MAGNOLIOPSIDA           MAMMALIA        MEROSTOMATA             MYXINI       PHAEOPHYCEAE 
#                 58                806                369                 16                  1                 78                 64                170                  4                 76                 15 
#         POLYCHAETA     POLYPODIOPSIDA           REPTILIA      SARCOPTERYGII        ULVOPHYCEAE                Sum 
#                  2                  3                 97                  2                  1               8380
#
# 2014 (before duplicate removal):
#     ACTINOPTERYGII           ANTHOZOA               AVES           BIVALVIA CEPHALASPIDOMORPHI 
#              57836               1026               1299                552                 15 
#        CEPHALOPODA      CHLOROPHYCEAE     CHONDRICHTHYES         ECHINOIDEA             ENOPLA 
#                594                  1               3071                  1                  4 
#    FLORIDEOPHYCEAE         GASTROPODA      HOLOTHUROIDEA           HYDROZOA            INSECTA 
#                101              10614               1009                 16                  1 
#         LILIOPSIDA      MAGNOLIOPSIDA       MALACOSTRACA           MAMMALIA        MAXILLOPODA 
#                325                 79               1321                367                  4 
#        MEROSTOMATA             MYXINI       PHAEOPHYCEAE         POLYCHAETA     POLYPODIOPSIDA 
#                  7                271                 36                  5                  3 
#           REPTILIA      SARCOPTERYGII        ULVOPHYCEAE                Sum 
#                821                  2                  1              79382
#
# 2014 (after duplicate removal):
#   ACTINOPTERYGII        ANTHOZOA            AVES        BIVALVIA     CEPHALOPODA   CHLOROPHYCEAE  CHONDRICHTHYES      ECHINOIDEA FLORIDEOPHYCEAE      GASTROPODA   HOLOTHUROIDEA        HYDROZOA      LILIOPSIDA   MAGNOLIOPSIDA 
#             2758             842               3              10             195               1            1093               1              58             711             371              16              66               1 
#     MALACOSTRACA        MAMMALIA     MAXILLOPODA     MEROSTOMATA          MYXINI    PHAEOPHYCEAE      POLYCHAETA        REPTILIA   SARCOPTERYGII     ULVOPHYCEAE             Sum 
#              248             110               4               4              76              15               2              47               2               1           6,635
#
# 2014 (after duplicate removal 2):
#     ACTINOPTERYGII           ANTHOZOA               AVES           BIVALVIA CEPHALASPIDOMORPHI        CEPHALOPODA      CHLOROPHYCEAE     CHONDRICHTHYES         ECHINOIDEA             ENOPLA 
#               3332                842                840                 34                  4                195                  1               1112                  1                  1 
#    FLORIDEOPHYCEAE         GASTROPODA      HOLOTHUROIDEA           HYDROZOA            INSECTA         LILIOPSIDA      MAGNOLIOPSIDA       MALACOSTRACA           MAMMALIA        MAXILLOPODA 
#                 58                812                371                 16                  1                 78                 65                252                170                  4 
#        MEROSTOMATA             MYXINI       PHAEOPHYCEAE         POLYCHAETA     POLYPODIOPSIDA           REPTILIA      SARCOPTERYGII        ULVOPHYCEAE                Sum 
#                  4                 76                 15                  2                  3                103                  2                  1               8395


# remove infraranks: 30 subspecies records ('ssp.')
d = subset(d, infrarank.type!='ssp.', !names(d) %in% c('infrarank','infrarank.type','infrarank.authority'))
table(d$category)
# 2014 (before duplicate removal):
#    CR    DD    EN    EW    EX    LC LR/cd LR/lc LR/nt    NT    VU 
#   371  8450   598     0    38 63906    30    13   167  2121  2613
# 2014 (after duplicate removal):
#    CR    DD    EN    EW    EX    LC LR/cd LR/lc LR/nt    NT    VU 
#    78  1983   140     0     6  3443     5     3     8   411   543 
# 2013:
#    CR    DD    EN    EX    LC LR/cd LR/lc LR/nt    NT    VU 
#   147  2175   228    24  4557     6     5    12   518   678
# 2014 (after duplicate removal 2):
#    CR    DD    EN    EX    LC LR/cd LR/lc LR/nt    NT    VU 
#   148  2178   231    24  4564     6     5    12   521   676 

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

print(addmargins(table(d$category)), row.names=F)
# 2013:
#    DD    LC    NT    VU    EN    CR    EX 
#  2175  4580   518   678   228   147    24
# 2014:
#    DD    LC    NT    VU    EN    CR    EX 
#  8450 63906  2331  2613   598   371    38
#
# 2014 (after duplicate removal 2):
#   DD   LC   NT   VU   EN   CR   EX 
# 2178 4564  544  676  231  148   24 

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
nrow(subset(d, category!='DD')) # 2013: 6,175; 2014: 69,857; 2014( after duplicate removal 2): 6,187
write.csv(d, file.path(cd, 'spp_iucn_marine.csv'), row.names=F, na='')

# get list of unique species and their global extinction status
d = read.csv(file.path(cd, 'spp_iucn_marine.csv'))

table(duplicated(d$sid))
# 2014 (before duplicate removal)
#  FALSE    TRUE 
#  8,365  69,942
# 2014 (after duplicate removal 2):
#  FALSE 
#  8,365

# function to extract just habitat from the IUCN API given the Red.List.Species.ID
options(stringsAsFactors=F)
getDetails = function(sid, download.tries=10){
  # example species Oncorhynchus nerka: sid=135301 # (parent) ## sid=135322 # (child)  # sid=4162
  url = sprintf('http://api.iucnredlist.org/details/%d/0', sid)
  htm = sprintf('%s/iucn_details/%d.htm', cd, sid) # htm = '135322.htm'
  i=0
  while ( !file.exists(htm) | (file.info(htm)$size==0 & i<download.tries) ){
    download.file(url, htm, method='auto', quiet=T, mode='wb')
    i=i+1
  }
  if (file.info(htm)$size==0) stop(sprintf('Only getting empty file for: %s', url))
  h = htmlParse(htm)
  subpopulations.href = xpathSApply(h, '//li[@class ="subspecie"]/a', xmlGetAttr, 'href')
  subpopulations = as.integer(sub('/details/([0-9]+)/0', '\\1', subpopulations.href))  
  countries      = as.character(xpathSApply(h, '//ul[@class="country_distribution"]/li/ul/li', xmlValue))
  popn_trend     = as.character(xpathSApply(h, '//div[@id="population_trend"]', xmlValue))
  
  # NOTE greater specificity 
  #   for http://www.iucnredlist.org/details/135322/0 -- United States (Alaska) 
  #   vs  http://api.iucnredlist.org/details/135322/0 -- United States
  # parent: http://api.iucnredlist.org/details/135301/0
  #countries = xpathSApply(h, '//td[@class="label"][strong="Countries:"]/following-sibling::td/div/text()', xmlValue)    
  return(
    rbind.fill(data.frame(subpopulations = subpopulations), 
               data.frame(countries      = countries), 
               data.frame(popn_trend     = popn_trend)) %.%
      mutate(sid=sid))
}

# get list of all subpopulations and countries
suppressWarnings(rm(list=c('spp_subpop','spp_countries','spp_popn_trend')))
  
# for (sid in d$sid){ # sid = d$sid[2] # sid
#   sp = getDetails(sid)
#   
#   # subpop
#   if (length(sp$subpopulations)>0){
#     sp_subpop = data.frame(parent_sid=sid, sid=sp$subpopulations)
#     if (!exists('spp_subpop')){
#       spp_subpop = sp_subpop
#     } else{
#       spp_subpop = rbind(spp_subpop, sp_subpop)
#     }
#   }
#   
#   # countries
#   if (length(sp$countries)>0){
#     sp_countries = data.frame(sid=sid, country=sp$countries)
#     if (!exists('spp_countries')){
#       spp_countries = sp_countries
#     } else{
#       spp_countries = rbind(spp_countries, sp_countries)
#     }
#   }
#   
#   # popn_trend
#   if (length(sp$popn_trend)>0){
#     sp_popn_trend = data.frame(sid=sid, popn_trend=sp$popn_trend)
#     if (!exists('spp_popn_trend')){
#       spp_popn_trend = sp_popn_trend    
#     } else{
#       spp_popn_trend = rbind(spp_popn_trend, sp_popn_trend)
#     }
#   }  
# }

#getDetails(135301)
#getDetails(10016)
getDetails(d$sid[4911])

print(Sys.time())
r = mclapply(d$sid, getDetails, mc.cores=detectCores(), mc.preschedule=T) # took ~ ? hrs on neptune
print(Sys.time())
rd = rbind_all(r)
print(Sys.time())

spp_subpop = rd %.%
  filter(!is.na(subpopulations)) %.%
  select(sid, subpopulations)

spp_countries = rd %.%
  filter(!is.na(countries)) %.%
  select(sid, countries)

spp_popn_trend = rd %.%
  filter(!is.na(popn_trend)) %.%
  mutate(popn_trend = factor(popn_trend, c('Unknown','Decreasing','Stable','Increasing'), ordered=T)) %.%
  select(sid, popn_trend)

# assign popn_trend to d, before subsetting into subpopulations and global species and 
d$popn_trend = factor(d$popn_trend, c('Unknown','Decreasing','Stable','Increasing'), ordered=T)
print(addmargins(table(spp_popn_trend$popn_trend, useNA='ifany')), row.names=F)
# 2013:
# Unknown Decreasing     Stable Increasing       NA's 
#       5559       1338       1002        193        258 
# 2014:
#    Unknown Decreasing     Stable Increasing        Sum 
#       5565       1341       1006        195       8107 

# manual subpopulations discovered later, somehow not listed in the IUCN details page of parent 
# (again difference b/n API and WWW versions), after defining spp_global with the following:
#   subset(spp_global, sciname %in% spp_global$sciname[duplicated(spp_global$sciname)])
spp_subpop.manual = c('Pristis pectinata'        = 18175,
                      'Centrophorus moluccensis' = 42838,
                      'Pristis pristis'          = 18584848)
for (i in 1:length(spp_subpop.manual)){ # i=1
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
write.csv(spp_subpop          , file.path(cd, 'spp_iucn_marine_subpop.csv')          , row.names=F, na='')
write.csv(spp_subpop_countries, file.path(cd, 'spp_iucn_marine_subpop_countries.csv'), row.names=F, na='')

# get list of all master species populations (ie not a subpopulation, used to extract IUCN rangemaps and apply category globally) 
# tmp/spp_iucn_marine_global.csv: sid, sciname, category
# tmp/spp_iucn_marine_global_countries.csv: sid, country
spp_global           = subset(d            , !sid %in% spp_subpop$sid)
spp_global_countries = subset(spp_countries,  sid %in% spp_global$sid)
cat(sprintf('\nFound %d marine global species in %d countries.\n', nrow(spp_global), nrow(spp_global_countries)))
# 2013: Found 8173 marine global species in 120469 countries.

# check for duplicate scientific names
subset(spp_global, sciname %in% spp_global$sciname[duplicated(spp_global$sciname)])

# assign popn_trend to spp_global

# Found 8181 marine global species in 120,563 countries.
write.csv(spp_global          , file.path(cd, 'spp_iucn_marine_global.csv')          , row.names=F, na='')
write.csv(spp_global_countries, file.path(cd, 'spp_iucn_marine_global_countries.csv'), row.names=F, na='')

# get distinct countries for doing an eez lookup (and later distribution for global species or catgory override for subpopulation)
spp_countries_distinct = as.data.frame(table(subset(spp_countries, sid %in% d$sid, country)))
spp_countries_distinct = rename(spp_countries_distinct[order(as.character(spp_countries_distinct[[1]])),],
                                c('Var1'='country','Freq'='count'))
write.csv(spp_countries_distinct, file.path(cd, 'spp_iucn_marine_distinct_countries.csv'), row.names=F, na='')


# supplement with Davies & Baum (2012) ------------------------------------

# # clean excess white spaces
# spp_db12 = read.csv('raw/DaviesBaum2012/DaviesBaum2012.csv', stringsAsFactors=F)
# require(gdata)
# for (fld in names(spp_db12)){ # fld = names(spp_db12)[1] # fld = 'Scientific_name'
#   if (class(spp_db12[[fld]])=='character'){
#     spp_db12[[fld]] = gdata::trim(spp_db12[[fld]])
#   }
# }
# # lowercase field names
# names(spp_db12) = tolower(names(spp_db12))
# write.csv(spp_db12, 'tmp/DaviesBaum2012.csv', row.names=F, na='')
# 
# # find spp not already in IUCN
# spp_db12 = read.csv('tmp/DaviesBaum2012.csv', stringsAsFactors=F)
# nrow(spp_db12) # 166 populations
# length(unique(spp_db12$scientific_name)) # 94 unique species
# spp_db12_notiucn = subset(spp_db12, !scientific_name %in% d$sciname)
# cat(sprintf('\nFound %d populations for %d species with Davies and Baum (2012) novel to IUCN species.\n', nrow(spp_db12_notiucn), length(unique(spp_db12_notiucn$scientific_name))))
# # Found 110 populations for 69 species with Davies and Baum (2012) novel to IUCN species.
# write.csv(spp_db12_notiucn, 'tmp/spp_DaviesBaum2012_notIUCN.csv', row.names=F, na='')

# old circa 2011 code ----

# iucn_summary = function(sid){ 
#   spec <- tolower(sciname)
#   spec <- gsub(" ", "-", spec)
#   url <- sprintf("http://api.iucnredlist.org/details/%d/0", sid)
#   #  browser()
#   h <- htmlParse(url)
#   
#   habitats = xpathSApply(h, '//li[@class ="system"]', xmlValue)
#   
#   #   # bbest adding taxonomy
#   #   classification <- data.frame(rank = c('Kingdom','Phylum','Class','Order','Family'),
#   #                                taxon = c(
#   #                                  toCamel(xpathSApply(h, '//div[@id ="kingdom"]', xmlValue)),
#   #                                  toCamel(xpathSApply(h, '//div[@id ="phylum"]', xmlValue)),
#   #                                  toCamel(xpathSApply(h, '//div[@id ="class"]', xmlValue)),
#   #                                  toCamel(xpathSApply(h, '//div[@id ="order"]', xmlValue)),
#   #                                  toCamel(xpathSApply(h, '//div[@id ="family"]', xmlValue))))
#   #   
#   #   status <- xpathSApply(h, '//div[@id ="red_list_category_code"]', xmlValue)
#   #   history <- data.frame(year = xpathSApply(h, '//div[@class="year"]', xmlValue),
#   #                         category = xpathSApply(h, '//div[@class="category"]', xmlValue))
#   #   distr <- xpathSApply(h, '//ul[@class="countries"]', xmlValue)
#   #   distr <- unlist(strsplit(distr, "\n"))
#   #   pop <- xpathSApply(h, '//div[@id="population"]/text()[preceding-sibling::br]', xmlValue)
#   #   
#   #   if (length(pop)>0){ # bbest adding check
#   #     pop <- do.call(rbind, lapply(strsplit(pop, split=":"), rbind)) 
#   #   }
#   #   trend <- xpathSApply(h, '//div[@id="population_trend"]', xmlValue)
#   #   out <- list(status = status,
#   #               classification = classification,
#   #               history = history, 
#   #               distr = distr, 
#   #               pop = pop, 
#   #               trend = trend,
#   #               habitats = habitats)
#   out <- list(habitats = habitats)
#   return(out)
# }
# 
# # iterate
# library(R.utils)
# pb <- txtProgressBar(min=1, max=nrow(spp), style=3)
# habitats = c()
# for (i in 1:nrow(spp)){ # i=1
#   sid = spp$Red.List.Species.ID[i]
#   sp = spp$Scientific.Name[i]
#   h = iucn_summary(sid)$habitats
#   habitats = c(habitats, setNames(h, rep(sid, length(h))))
#   setTxtProgressBar(pb, i)
# }
# 
# 
# 
# 
# 
# # original spp missing taxonomy (n=47)
# #q = 'SELECT sid, iucn_src_shp, scientific, status FROM spp WHERE ("class" IS NULL) ORDER BY iucn_src_shp, scientific'
# #spp = dbGetQuery(pg, q)
# #write.csv(spp, 'data/spp_missing_taxonomy.csv')
# 
# # work on latest species still missing taxonomy
# q = "SELECT sid, iucn_src, scientific, status FROM spp WHERE TRIM(\"class\")='' OR \"class\" IS NULL ORDER BY iucn_src_shp, scientific"
# spp = dbGetQuery(pg, q)
# 
# # correct bad names
# scientific.corrections = c('Eptatretus rubucundus'    ='Eptatretus rubicundus',
#                            'Sarda chilensis lineolata'='Sarda chiliensis lineolata')
# for (sci.bad in names(scientific.corrections)){ # sci.bad = names(scientific.corrections)[1]
#   spp$scientific[spp$scientific==sci.bad] = scientific.corrections[[sci.bad]]
# }
# 
# # get taxonomic serial numbers
# tsns = lapply(spp$scientific, get_tsn, searchtype='sciname')
# 
# # iterate 
# for (i in 1:length(spp$scientific)){ # i=1
#   sid = spp$sid[i]
#   sp = as.character(spp$Scientific.Name[i])
#   #sp.tsn = tsns[[i]][[1]] # may have multiple tsns, but probably just subspecies so assuming ok to use first
#   cat(sprintf('%02d: %s - %s [%s]\n', i, spp$iucn_src[i], sp, sp.tsn))
#   if (!is.na(sp.tsn)){
#     # fetch taxonomic classification
#     x = classification(sp.tsn, ID='tsn')[[1]]
#   } 
#   if (is.na(sp.tsn) | (!'Class' %in% x$rank)) {
#     # try IUCN classification
#     x = iucn_summary(sp)$classification
#   }
#   q = paste("UPDATE spp SET
#          kingdom ='", x$taxon[x$rank=='Kingdom'],"',
#          phylum  ='", x$taxon[x$rank=='Phylum' ],"',
#          class   ='", x$taxon[x$rank=='Class'  ],"',     
#        \"order\"  ='", x$taxon[x$rank=='Order'  ],"',
#          family  ='", x$taxon[x$rank=='Family' ],"'
#        WHERE sid=",sid,"\n", sep='')
#   cat(q)
#   r = dbGetQuery(pg, q); head(r)
# }            
# spp = read.csv('data/spp_missing_taxonomy.csv')
# q = sprintf("SELECT iucn_src, scientific, kingdom, phylum, class, \"order\", family, sid FROM spp WHERE sid IN (%s) ORDER BY iucn_src, scientific",
#             paste(spp$sid, collapse=','))
# spp = dbGetQuery(pg, q)
# write.csv(spp, 'data/spp_missing_taxonomy_updated.csv', row.names=F,na='')
