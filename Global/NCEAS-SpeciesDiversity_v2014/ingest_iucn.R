# Get all species from IUCN. Limit to those found in marine habitats.
# Now using 2013.1 as of July 1st, 2013

# load packages
library(ohi)
library(RCurl)
library(XML)
library(parallel)
library(plyr)

# set working directory
#wd = '/var/data/ohi/model/GL-NCEAS-SpeciesDiversity_2013' # (Mac/Linux)
wd = 'N:/model/GL-NCEAS-SpeciesDiversity_v2013a' # Windows
setwd(wd)

# flags & vars
reload = F

# cache dir
if (file.exists('cache/iucn_details') & reload) unlink('cache', recursive=T)
dir.create('cache/iucn_details', showWarnings=F, recursive=T)

# get all species
if (!file.exists('tmp/spp_iucn_all.csv') | reload){
  spp_iucn_all = read.csv('http://api.iucnredlist.org/index/all.csv')         # nrows = 72,329
  spp_iucn_all = spp_iucn_all[!duplicated(spp_iucn_all),] # remove duplicates # nrows = 72,329
  write.csv(spp_iucn_all, 'tmp/spp_iucn_all.csv', row.names=F, na='')
} else {
  spp_iucn_all = read.csv('tmp/spp_iucn_all.csv')
}

# function to extract just habitat from the IUCN API given the Red.List.Species.ID
getHabitats = function(sid, download.tries=10){
  url = sprintf('http://api.iucnredlist.org/details/%d/0', sid)
  htm = sprintf('cache/iucn_details/%d.htm', sid)
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

# spp_iucn_all = head(spp_iucn_all, 1000) # DEBUG
# unlink('tmp/spp_iucn_habitats.csv')    # DEBUG
#options(error=recover)       # 46181060 # DEBUG    

if (!file.exists('tmp/spp_iucn_habitats.csv') | reload){  
  print(system.time({    
    #r = lapply(spp_iucn_all$Red.List.Species.ID, getHabitats) # DEBUG
    r = mclapply(spp_iucn_all$Red.List.Species.ID, getHabitats, mc.cores=detectCores(), mc.preschedule=F) # took ~ 4 hrs on neptune
  }))
  r = unlist(r)
  spp_iucn_habitats = data.frame(sid = r, habitat = names(r))
  write.csv(spp_iucn_habitats, 'tmp/spp_iucn_habitats.csv', row.names=F, na='')
} else {
  spp_iucn_habitats = read.csv('tmp/spp_iucn_habitats.csv')
}

# check no remaining download errors. might need to manually run. should be 0:
subset(spp_iucn_habitats, is.na(habitat))

# species without habitats assigned (n=67) - mostly birds (n=58)
sid.miss = setdiff(spp_iucn_all$Red.List.Species.ID, spp_iucn_habitats$sid)
print(addmargins(table(subset(spp_iucn_all, Red.List.Species.ID %in% sid.miss)$Class, useNA='ifany')))
#       AMPHIBIA           AVES CHONDRICHTHYES      CRUSTACEA     GASTROPODA  HOLOTHUROIDEA        INSECTA       MAMMALIA            Sum 
#              1             58              1              1              2              2              1              1             67

# get counts of species / subpopulations
print(addmargins(table(spp_iucn_habitats$habitat, useNA='ifany')))
#head(spp_iucn_habitats[is.na(spp_iucn_habitats$habitat),])
#  Freshwater      Marine Terrestrial         Sum 
#       25245        8380       50135       83760 

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

# get list of all subpopulations and countries
suppressWarnings(rm(list=c('spp_subpop','spp_countries','spp_popn_trend')))
for (sid in d$sid){ # sid = d$sid[1] # sid
  sp = getDetails(sid)
  
  # subpop
  if (length(sp$subpopulations)>0){
    sp_subpop = data.frame(parent_sid=sid, sid=sp$subpopulations)
    if (!exists('spp_subpop')){
      spp_subpop = sp_subpop
    } else{
      spp_subpop = rbind(spp_subpop, sp_subpop)
    }
  }
  
  # countries
  if (length(sp$countries)>0){
    sp_countries = data.frame(sid=sid, country=sp$countries)
    if (!exists('spp_countries')){
      spp_countries = sp_countries
    } else{
      spp_countries = rbind(spp_countries, sp_countries)
    }
  }
  
  # popn_trend
  if (length(sp$popn_trend)>0){
    sp_popn_trend = data.frame(sid=sid, popn_trend=sp$popn_trend)
    if (!exists('spp_popn_trend')){
      spp_popn_trend = sp_popn_trend    
    } else{
      spp_popn_trend = rbind(spp_popn_trend, sp_popn_trend)
    }
  }  
}

# assign popn_trend to d, before subsetting into subpopulations and global species and 
d$popn_trend = NA
d[match(spp_popn_trend$sid, d$sid),'popn_trend'] = as.character(spp_popn_trend$popn_trend)
d$popn_trend = factor(d$popn_trend, c('Unknown','Decreasing','Stable','Increasing'), ordered=T)
summary(d$popn_trend)
#    Unknown Decreasing     Stable Increasing       NA's 
#       5559       1338       1002        193        258 

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
write.csv(spp_subpop          , 'tmp/spp_iucn_marine_subpop.csv'          , row.names=F, na='')
write.csv(spp_subpop_countries, 'tmp/spp_iucn_marine_subpop_countries.csv', row.names=F, na='')

# get list of all master species populations (ie not a subpopulation, used to extract IUCN rangemaps and apply category globally) 
# tmp/spp_iucn_marine_global.csv: sid, sciname, category
# tmp/spp_iucn_marine_global_countries.csv: sid, country
spp_global           = subset(d            , !sid %in% spp_subpop$sid)
spp_global_countries = subset(spp_countries,  sid %in% spp_global$sid)
cat(sprintf('\nFound %d marine global species in %d countries.\n', nrow(spp_global), nrow(spp_global_countries)))
# Found 8173 marine global species in 120469 countries.

# check for duplicate scientific names
subset(spp_global, sciname %in% spp_global$sciname[duplicated(spp_global$sciname)])

# assign popn_trend to spp_global


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
