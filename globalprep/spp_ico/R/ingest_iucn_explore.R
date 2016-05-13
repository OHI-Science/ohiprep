# ingest_iucn.R - exploratory work by Ben Best, 2013


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


