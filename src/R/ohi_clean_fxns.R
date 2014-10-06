# Data Prep functions to use for OHI cleaning and gapfilling 

# created fall 2013 by JStewart; updated March 2014 by JStewartLowndes
# location of original file on Neptune local_edit: /src/R/jstewart/ohi_clean_fxns.R

# add rgn_id
add_rgn_id = function(uidata, uifilesave, 
                      dpath = 'src/LookupTables',
                      rgn_master.csv   = file.path(dpath, 'eez_rgn_2013master.csv'),
                      rgn_synonyms.csv = file.path(dpath, 'rgn_eez_v2013a_synonyms.csv')) {
  
  # debug: dpath = 'src/LookupTables'; rgn_master.csv   = file.path(dpath, 'eez_rgn_2013master.csv'); rgn_synonyms.csv = file.path(dpath, 'rgn_eez_v2013a_synonyms.csv')
  
  
  ##  setup ----
  print('-->>> add_rgn_id.r expects that the first two columns of the matrix will be country_name, value_units ')
  
  # load libraries
  library(dplyr)
  
  # read in lookup files, combine into one dataframe
  rk = read.csv(rgn_master.csv); head(rk) # master file by BB
  rk2 = read.csv(rgn_synonyms.csv); head(rk2) # synonym file by JS
  
  # manage master rk
  rk = rk %.%
    filter(rgn_id_2013 < 255) %.% # remove open ocean and disputed
    mutate(rgn_typ = 'ohi_region') %.%
    arrange(rgn_id_2013)
  
  # manage synonym rk2
  rkb  = data.frame(rk$rgn_id_2013,  rk$rgn_key_2013,  rk$rgn_nam_2013,  rk$region_id_2012,  rk$rgn_typ) 
  rk2b = data.frame(rk2$rgn_id_2013, rk2$rgn_key_2013, rk2$rgn_nam_2013, rk2$region_id_2012, rk2$rgn_typ)
  
  # create regionkey: combine official and synonym region_id data
  names(rkb)  = c('rgn_id_2013', 'rgn_key_2013', 'rgn_nam_2013', 'region_id_2012', 'rgn_typ')
  names(rk2b) = c('rgn_id_2013', 'rgn_key_2013', 'rgn_nam_2013', 'region_id_2012', 'rgn_typ')
  regionkey = rbind(rkb, rk2b)
  regionkey$rgn_nam_2013 = as.character(regionkey$rgn_nam_2013)
  regionkey = regionkey[!duplicated(regionkey), ] # remove duplicate rows in regionkey. this is what SELECT DISTINCT rgn_nam_2013, rgn_key_2013, rgn_id_2013, rgn_typ did with SQlite
  
  # remove accents
  col_num = grep('country', names(uidata), ignore.case = TRUE)
  names(uidata)[col_num] = 'rgn_nam'
  uidata[,col_num] = gsub('^\'', '', uidata[,col_num]) # get rid of any errant quotes
  uidata[,col_num] = gsub('.+voire', 'Ivory Coast', uidata[,col_num]) # Ivory Coast
  uidata[,col_num] = gsub('.+union', 'Reunion', uidata[,col_num]) # Reunion
  uidata[,col_num] = gsub('.+publique du', 'Republic of', uidata[,col_num]) # Congo
  uidata[,col_num] = gsub('Cura.+', 'Curacao', uidata[,col_num]) # Curacao 
  uidata[,col_num] = gsub('Saint Barth.+', 'Saint Barthelemy', uidata[,col_num]) # Saint Barthelemy 
  uidata[,col_num] = gsub('.+Principe', 'Sao Tome and Principe', uidata[,col_num]) # Sao Tome and Principe
  head(uidata)
  
  ## join uidata with regionkey, check that all rgn_ids are accounted for ----
  
  ## join uidata with regionkey
  uidata_regionkey = regionkey %.%
    select(rgn_id = rgn_id_2013,
           rgn_nam = rgn_nam_2013, 
           rgn_typ) %.%
    inner_join(uidata, by = 'rgn_nam'); head(uidata_regionkey) 
  
  # only keep ohi_regions
  uidata_rgn = uidata_regionkey %.%
    filter(rgn_typ == 'ohi_region' | is.na(rgn_typ)); head(uidata_rgn) 
  
  # indicate which were removed
  RemovedRegions = uidata_regionkey %.%
    filter(rgn_typ == 'landlocked' | rgn_typ == 'largescale' | rgn_typ == 'disputed') %.%
    select(rgn_nam)
  print('These landlocked/largescale/disputed regions were removed:')
  print(unique(data.frame(RemovedRegions)))
  
  # indicate which still need to be assigned:
  uidata_rgn.na = uidata_rgn %.%
    filter(is.na(rgn_typ))
  print('These non-landlocked countries do not have assigned rgn_ids:')
  print(unique(data.frame(uidata_rgn.na$country_id)))
  
  print('TRUE if everything is working properly: ')
  print(dim(uidata_rgn)[1] + dim(RemovedRegions)[1] == dim(uidata_regionkey)[1]) # make sure this is TRUE
  
  ## save, but keep rgn_nam for gapfilling purposes. remove in add_gapfill.r ----
  uidata_rgn$rgn_typ <- NULL
  uidata_rgn = uidata_rgn %.%
    arrange(rgn_id); head(uidata_rgn)
  
  
  print('Be sure to inspect saved .csv file for additional or missing rgn_ids.')
  write.csv(uidata_rgn, uifilesave, na = '', row.names=FALSE)
  
}

# identify duplicate regions and sum them (currently supports rgn_id = 13, 116, 140, 209) 
sum_duplicates = function(cleandata, dup_ids, fld.nam = 'value') {
  
  d = cleandata
  
  n = names(d)
  fld_ids_join = names(d)[!names(d) %in% c('rgn_nam', fld.nam)]
  names(d)[names(d) %in% fld.nam] = 'value'; head(d) # call this value for processing; revert back below 
  
  # to rbind all
  d.all =  matrix(nrow=0, ncol=0) 
  
  ## fix Northern Mariana Islands and Guam (rgn_id=13) ----
  if (13 %in% dup_ids) { 
    dn = filter(d, rgn_nam == 'Northern Mariana Islands'); head(dn)
    dg = filter(d, rgn_nam == 'Guam'); head(dg)
    
    nmi = dn %.%
      left_join(dg, by=fld_ids_join); head(nmi) 
    
    # calculate sum--causing weirdness otherwise: sum giving different values with rm.na=T (??!)
    nmi_sum = nmi %.%
      select(value.x, value.y)
    nmi_sum$value_tot = rowSums(nmi_sum, na.rm = T); tail(nmi_sum)
    nmi_sum$value_tot[is.na(nmi_sum$value.x) & is.na(nmi_sum$value.y)] = NA; tail(nmi_sum) # fix because NA+NA=0
    
    # cbind sum in place of individual values
    nmi_tot = cbind(nmi, nmi_sum) %.%
      mutate(rgn_nam_tot = 'Northern Mariana Islands and Guam'); head(nmi_tot)
    nmi_tot$rgn_nam.x = NULL; nmi_tot$rgn_nam.y = NULL; nmi_tot$value.x = NULL; nmi_tot$value.y = NULL; 
    nmi_tot = plyr::rename(nmi_tot, 
                           c('value_tot' = 'value', 'rgn_nam_tot' = 'rgn_nam')); head(nmi_tot)
    
    d.all = rbind(d.all, nmi_tot)
  }
  
  
  ## fix Puerto Rico and Virgin Islands of the United States (rgn_id=116) ----
  if (116 %in% dup_ids) { 
    dp = filter(d, rgn_nam == 'Puerto Rico'); head(dp)
    dv = filter(d, rgn_nam == 'Virgin Islands (U.S.)' | rgn_nam == 'US Virgin Islands'); head(dv)
    
    prvi = dp %.%
      left_join(dv, by=fld_ids_join); head(prvi)
    
    # calculate sum--causing weirdness otherwise: sum giving different values with rm.na=T (??!)
    prvi_sum = prvi %.%
      select(value.x, value.y)
    prvi_sum$value_tot = rowSums(prvi_sum, na.rm = T); head(prvi_sum)
    prvi_sum$value_tot[is.na(prvi_sum$value.x) & is.na(prvi_sum$value.y)] = NA; head(prvi_sum) # fix because NA+NA=0
    
    # cbind sum in place of individual values. Easier this way rather than 'select' since you don't know all the columns involved in fld_ids_join
    prvi_tot = cbind(prvi, prvi_sum) %.%
      mutate(rgn_nam_tot = 'Puerto Rico and Virgin Islands of the United States'); head(prvi_tot)
    prvi_tot$rgn_nam.x = NULL; prvi_tot$rgn_nam.y = NULL; prvi_tot$value.x = NULL; prvi_tot$value.y = NULL; 
    prvi_tot = plyr::rename(prvi_tot, 
                            c('value_tot' = 'value', 'rgn_nam_tot' = 'rgn_nam')); head(prvi_tot)
    
    d.all = rbind(d.all, prvi_tot)
  }
  
  ## fix Guadeloupe and Martinique (rgn_id=140) ----
  if (140 %in% dup_ids) { 
    du = filter(d, rgn_nam == 'Guadeloupe'); head(du)
    da = filter(d, rgn_nam == 'Martinique'); head(da)
    
    gma = du %.%
      left_join(da, by=fld_ids_join); head(gma) 
    
    # calculate sum--causing weirdness otherwise: sum giving different values with rm.na=T (??!)
    gma_sum = gma %.%
      select(value.x, value.y)
    gma_sum$value_tot = rowSums(gma_sum, na.rm = T); tail(gma_sum)
    gma_sum$value_tot[is.na(gma_sum$value.x) & is.na(gma_sum$value.y)] = NA; tail(gma_sum) # fix because NA+NA=0
    
    # cbind sum in place of individual values
    gma_tot = cbind(gma, gma_sum) %.%
      mutate(rgn_nam_tot = 'Guadeloupe and Martinique'); head(gma_tot)
    gma_tot$rgn_nam.x = NULL; gma_tot$rgn_nam.y = NULL; gma_tot$value.x = NULL; gma_tot$value.y = NULL; 
    gma_tot = plyr::rename(gma_tot, 
                           c('value_tot' = 'value', 'rgn_nam_tot' = 'rgn_nam')); head(gma_tot)
    
    d.all = rbind(d.all, gma_tot)
  }
  
  ## fix China (rgn_id=209) ----
  if (209 %in% dup_ids) { 
    dh = filter(d, rgn_nam == 'Hong Kong SAR, China' | rgn_nam == 'China, Hong Kong SAR'); head(dh)
    dm = filter(d, rgn_nam == 'Macao SAR, China'); head(dm)
    dc = filter(d, rgn_nam == 'China'); head(dc)
    
    chn = dc %.%
      left_join(dm, by=fld_ids_join) %.%
      left_join(dh, by=fld_ids_join); head(chn)
    
    # calculate sum--causing weirdness otherwise: sum giving different values with rm.na=T (??!)
    chn_sum = chn %.%
      select(value.x, value.y, value)
    chn_sum$value_tot = rowSums(chn_sum, na.rm = T); head(chn_sum)
    chn_sum$value_tot[is.na(chn_sum$value.x) & is.na(chn_sum$value.y) & is.na(chn_sum$value)] = NA; tail(chn_sum) # fix because NA+NA=0
    
    # cbind sum in place of individual values
    chn_tot = cbind(chn, chn_sum) %.%
      mutate(rgn_nam_tot = 'China'); head(chn_tot)
    chn_tot$rgn_nam = NULL; chn_tot$rgn_nam.x = NULL; chn_tot$rgn_nam.y = NULL; 
    chn_tot$value = NULL; chn_tot$value.x = NULL; chn_tot$value.y = NULL; 
    chn_tot = plyr::rename(chn_tot, 
                           c('value_tot' = 'value', 'rgn_nam_tot' = 'rgn_nam')); head(chn_tot)
    
    d.all = rbind(d.all, chn_tot)
  }
  
  ## rbind all total values for all duplicates ----
  d_fix = data.frame(rbind(d %.%
                             filter(rgn_id != 13, rgn_id !=116, rgn_id !=140, rgn_id != 209), 
                           d.all)) %.%
    arrange(rgn_id); head(d_fix)
  
  names(d_fix) = n
  return(d_fix)
  
}


# make file 2012a: update nature2012 data with 2013 rgn_id

update_rgn_id_sov = function(nature2012data, nature2012filesave, nature2012_rgn_typ, column_order) {
  # column_order: should be value = column 1, the year if exists, then sector if exists. in format c(1,2,3).   
  
  library(reshape2)
  library(gdata)
  options(gsubfn.engine = "R") # otherwise, get X11 launching for sqldf package
  require(sqldf)
  
  ## read in  offical (by BB) lists with 2-letter OHI region codes
  rk = read.csv('/Volumes/data_edit/model/GL-NCEAS-OceanRegions_v2013a/manual_output/eez_rgn_2013master.csv')
  regionkey = rk
  
  # join based on which type of identifier
  if(nature2012_rgn_typ == 'country_id') {
    natureb = sqldf("SELECT b.rgn_id_2013, b.rgn_key_2013, a.*
                 FROM nature2012data AS a
                 LEFT OUTER JOIN (
                     SELECT DISTINCT rgn_id_2013, rgn_key_2013, rgn_key_2012
                     FROM regionkey 
                     ) AS b ON b.rgn_key_2012 = a.country_id") 
  } else {
    natureb = sqldf("SELECT b.rgn_id_2013, b.rgn_key_2013, a.*
                 FROM nature2012data AS a
                 LEFT OUTER JOIN (
                     SELECT DISTINCT rgn_id_2013, rgn_key_2013, region_id_2012
                     FROM regionkey 
                     ) AS b ON b.region_id_2012 = a.region_id") 
  }
  
  if(sum(is.na(natureb$rgn_id_2013) > 0)){
    natureb[is.na(natureb$rgn_id_2013),]
    print((sprintf('there are %d Nature2012 regions that were not matched with 2013 regions and were removed from all \'levels\': ', length(unique(natureb$country_id[is.na(natureb$rgn_id_2013)])))))
    print(unlist(unique(natureb$country_id[is.na(natureb$rgn_id_2013)])))
  }else{
    cat(print('everything is working properly, no unmatched OHI regions'))
  }
  
  naturec = natureb[!is.na(natureb[,1]),]
  naturec[,3] = NULL # country_id or region_id ## order is important here to not delete the second column twice
  naturec[,2] = NULL # rgn_key_2013 
  natured = naturec[,c(1,column_order)]
  names(natured)[1] = 'rgn_id'
  
  cat(print('2012a file saved. '))
  write.csv(natured, nature2012filesave, na = '', row.names=FALSE)
  
}


disaggregate = function(csv.in, csv.out, flds.id='region_id', fld.value=NA,
                        region_id.exclude=c(162),
                        weight.csv  = NA, weight.fld = NA){
  
  # load libraries
  #library(reshape2)
  #library(gdata)
  #options(gsubfn.engine = "R") # otherwise, get X11 launching for sqldf package
  #require(sqldf)
  library(plyr)
  
  # area-weighted example
  #   csv.in     = '/Volumes/local_edit/src/toolbox/data/global_2012_nature/layers/p_sp_alien.csv'
  #   csv.out    = '/Volumes/data_edit/model/GL-NCEAS-Resilience_v2013a/data/p_sp_alien_dis2012area.csv'
  #   weight.csv  = '/Volumes/data_edit/model/GL-NCEAS-OceanRegions_v2013a/data/rgn_area.csv'
  #   weight.fld = 'area_km2'
  #   flds.id='region_id'
  #   fld.value=NA
  
  # pop-weighted example  
  #totalpop   = '/Volumes/data_edit/model/GL-WorldBank-Statistics_v2012/data/rgn_wb_pop.csv'
  #coastalpop = '/Volumes/data_edit/model/GL-NCEAS-CoastalPopulation_v2013/data/rgn_popsum2013_inland25mi_complete.csv'
  
  # sovereignty example
  #   csv.in  = '/Volumes/local_edit/src/toolbox/data/global_2012_nature/layers/r_cites.csv'
  #   csv.out = '/Volumes/data_edit/model/GL-NCEAS-Resilience_v2013a/data/r_cites_test.csv'
  #   weight.csv  = NA
  #   weight.fld = NA
  #   flds.id='region_id'
  #   fld.value=NA
  # disaggregate(csv.in='/Volumes/local_edit/src/toolbox/data/global_2012_nature/layers/r_cites.csv', 
  #              csv.out= '/Volumes/data_edit/model/GL-NCEAS-Resilience_v2013a/data/r_cites_test.csv')
  
  
  # TODO: example with category/year for muliple flds.id
  # layer=rnk_np_product_weight
  # /usr/local/src/toolbox/data/global_2012_nature/layers/np_product_weight.csv
  
  # read in weights, if applicable
  if (!is.na(weight.csv)){
    weight = read.csv(weight.csv, na.strings='')
  }
  
  # get lookup files for translating Nature 2012 to 2013a regions and countries
  dir.lookups = '/Volumes/data_edit/model/GL-NCEAS-OceanRegions_v2013a/manual_output'
  cntry2013_country2012 = read.csv(file.path(dir.lookups, 'cntry2013_country2012.csv'), na='')
  rgn_cntry             = read.csv(file.path(dir.lookups, 'rgn_cntry.csv'), na='')
  rgn_eez_v2013a        = read.csv(file.path(dir.lookups, 'eez_rgn_2013master.csv'), na='')
  rgn2013_region2012    = read.csv(file.path(dir.lookups, 'rgn2013_region2012.csv'), na='')
  
  # read in nature2012 data and assign other default arguments
  a = read.csv(csv.in, na.strings=''); head(a)
  if (is.na(fld.value)){
    fld.value = names(a)[!names(a) %in% flds.id]
  }
  
  # check validity of arguments
  stopifnot(flds.id[1] %in% c('region_id','country_id'))
  stopifnot(c(flds.id, fld.value) %in% names(a))
  stopifnot(length(fld.value) == 1)
  
  # join based on which type of identifier
  if(flds.id[1] == 'region_id') {    
    
    head(rgn2013_region2012)
    m = merge(x=subset(a, !region_id %in% region_id.exclude), by.x='region_id', all.x=T,
              y=rgn2013_region2012, by.y='region_id_2012')
    head(m)
    
    # ensure no region_id's are unmatched
    # NOTE: if don't care about this region_id, add to region_id_exclude argument
    stopifnot(any(!is.na(m$rgn_id_2013)))
    
    # ensure rgn_id_2013 is not duplicated
    stopifnot(any(!duplicated(m$rgn_id_2013)))
    
    if (!is.na(weight.csv)){
      d = merge(x=m, by.x='rgn_id_2013', all.x=T,
                y=weight, by.y='rgn_id')
      
      ids = unique(d[,flds.id])
      # TODO: check with multiple fields in flds.id like with category and year
      for (i in 1:length(ids)){ # i=1
        di = subset(d, region_id==ids[i]); head(di)
        v = di[1, fld.value]
        wts = di[[weight.fld]] / sum(di[[weight.fld]])
        d[d$region_id %in% di$region_id, fld.value] = v * wts
        #subset(d, region_id==ids[i])
      }
      
    } else {
      d = m
    }    
    
    # need to order by other fields as well too
    
    # output 2013 csv
    # TODO: id.flds > 1
    write.csv(rename(d[,c('rgn_id_2013', fld.value)],
                     c('rgn_id_2013'='rgn_id')),
              csv.out, row.names=F, na='')
    
  } else if(flds.id[1] == 'country_id') {
    #     natureb = sqldf("SELECT b.rgn_id_2013, b.rgn_key_2013, a.*
    #                  FROM nature2012data AS a
    #                  LEFT OUTER JOIN (
    #                      SELECT DISTINCT rgn_id_2013, rgn_key_2013, rgn_key_2012
    #                      FROM regionkey 
    #                      ) AS b ON b.rgn_key_2012 = a.country_id") 
  }  
}
# disaggregate(csv.in='/Volumes/local_edit/src/toolbox/data/global_2012_nature/layers/r_cites.csv', 
#              csv.out= '/Volumes/data_edit/model/GL-NCEAS-Resilience_v2013a/data/r_cites_test.csv')
# 
# 
# fxn = "disaggregate(csv.in='/Volumes/local_edit/src/toolbox/data/global_2012_nature/layers/r_cites.csv', 
#              csv.out= '/Volumes/data_edit/model/GL-NCEAS-Resilience_v2013a/data/r_cites_test.csv')"
# cat(fxn)
# eval(parse(text=fxn))


save_pressure_layers_2012a_2013a = function(data, filesave2012a, filesave2013a) {
  
  data = na.omit(data)
  
  # just 3 years, so go through and take max of each time
  rgn_uni = unique(data$rgn_id)
  d_12 = data.frame(matrix(nrow=length(rgn_uni), ncol=dim(data)[2]))
  d_13 = data.frame(matrix(NA, length(rgn_uni), dim(data)[2]))
  names(d_12) = names(data) 
  names(d_13) = names(data) 
  
  for(i in 1:length(rgn_uni)){
    a = data[data$rgn_id == rgn_uni[i],]
    al = dim(a)[1]
    d_13[i,] = a[al,]
    if(al>=2){
      d_12[i,] = a[al-1,]
    }else{
      d_12[i,] = a[al,]
    }
  }
  
  d_12$year = NULL
  d_13$year = NULL
  
  write.csv(d_12, filesave2012a, na = '', row.names=FALSE)
  write.csv(d_13, filesave2013a, na = '', row.names=FALSE)
  
}

# by BB
temporal.gapfill = function(data, fld.id = 'rgn_id', fld.value = 'value', fld.year = 'year',
                            year.end = max(data[[fld.year]], na.rm=T), 
                            year.beg = year.end - 5,
                            years.window = 10, verbose=F){
  
  
  #  fld.id = 'rgn_id'; fld.year = 'year'; fld.value = 'value'; year.end = max(data[[fld.year]], na.rm=T); year.beg = year.end - 5; years.window = 10; verbose=F
  # arguments:
  #   fld.id       : 
  #   year.end     : maximum year to finish temporal gap filling, defaulting to the max year
  #   year.beg     : minimum year to begin temporal gap filling, defaulting to 5 years prior to the max year
  #   years.window : maximum range of years, centered as much as possible on current year
  #
  # value:
  #   This function returns the original data frame with additional gap-filled rows and columns specifying:
  #     whence         : 'original' or 'temporal.gapfill(%s)' with arguments used in original function call
  #     whence_details : details of gap filled linear model specific to that row of data
  #
  # Note that this methods is different than interpolating between the first previous and next available values in time. See approx() for that.
  # 
  # TODO: include measure of variation from lm()
  
  # ensure fields exist
  stopifnot(fld.id    %in% names(data))
  stopifnot(fld.year  %in% names(data))
  stopifnot(fld.value %in% names(data))
  
  # rename fields to generically handle the data
  d = plyr::rename(na.omit(data[, c(fld.id, fld.value, fld.year)]), 
                   setNames(c(         'id',   'value',   'year'  ),
                            c(fld.id, fld.value, fld.year)))
  
  # initialize data frame of filled data
  d.filled = d[0,]
  d.filled$whence_details = character()
  
  # check validity of parameters
  stopifnot(year.end >= max(d[[fld.year]], na.rm=T))
  
  # setup variables
  yrs.tofill = year.beg:year.end
  
  # loop through all ids
  for (i in unique(d$id)){ # i=5
    
    # identify missing years
    d.id = subset(d, id==i)
    yrs.missing =  yrs.tofill[!yrs.tofill %in% d.id$year]
    
    if (length(yrs.missing) > 0 & verbose) cat(sprintf('id %d: %d missing years\n', i, length(yrs.missing)))
    
    # loop through any missing years
    for (yr in yrs.missing){ # yr = 2006
      
      
      # get available window of data, centered as much as possible on current year
      yr.max = min(max(d.id$year), yr + years.window/2)
      yr.min = max(min(d.id$year), yr - years.window)  
      if (verbose) cat(sprintf('  yr:%d, yr.max:%d, yr.min:%d\n', yr, yr.max, yr.min))
      d.id.yrs = subset(d.id, year >= yr.min & year <= yr.max)      
      
      # skip if insufficient data to gap fill
      if (nrow(d.id.yrs) < 2){    # TODO: maybe flag this to gapfill
        next
      }
      
      # fit linear model
      mdl = lm(value ~ year, d.id.yrs)      
      
      # predict on given year (if only two points, suppress Warning message: "In qt((1 - level)/2, df) : NaNs produced")
      res = suppressWarnings(predict.lm(mdl, newdata=data.frame('year'=yr), interval=c('confidence')))
      
      # bind to filled data
      whence_details = sprintf('confidence interval of %g to %g from lm.predict() having annual trend %g on %d values spanning years %d to %d', 
                               res[,'lwr'], res[,'upr'], mdl[['coefficients']][['year']], nrow(d.id.yrs), min(d.id.yrs$year), max(d.id.yrs$year))      
      d.filled = rbind(d.filled,
                       data.frame(id=i, value=res[,'fit'], year=yr, 
                                  whence_details=whence_details))
    }
  }
  
  # bind to original with additional whence field
  # d$whence = 'original' # JS commented out bc causing errors Aug 27 2013 17h00
  # d.filled$whence = paste(deparse(sys.call()), collapse='') # gettting the call to the function
  
  d = plyr::rbind.fill(d, d.filled)
  d = d[order(d$id, d$year), ]
  d = plyr::rename(d, 
                   setNames(c(fld.id, fld.value, fld.year),
                            c('id'  , 'value',  'year')))
  return(d)
}

#JS
add_gapfill = function(cleandata, dirsave, layersave, s_island_val, 
                       dpath = 'src/LookupTables',   
                       rgn_georegions.csv = file.path(dpath, 'rgn_georegions_wide_2013b.csv'),
                       rgns.csv           = file.path(dpath, 'rgn_details.csv')) {
  
  # debug: dpath = 'src/LookupTables'; rgn_georegions.csv = file.path(dpath, 'rgn_georegions_wide_2013b.csv'); rgns.csv = file.path(dpath, 'rgn_details.csv')
  
  
  # setup ----
  print('-->>> add_gapfill.r substitutes UN georegions means for NA values')
  
  # load libraries
  library(dplyr)
  
  # read in lookup files
  gf = read.csv(rgn_georegions.csv); head(gf) # georegions file
  rf = read.csv(rgns.csv); head(rf) # rgns file
  
  #tidy cleandata
  cleandata$rgn_nam = NULL
  n = names(cleandata)
  value_nam = names(cleandata)[!names(cleandata) %in% c('rgn_id', 'year')]
  names(cleandata)[!names(cleandata) %in% c('rgn_id', 'year')] = 'value' # call this value for processing; revert back below 
  head(cleandata)
  
  
  # create lookup tables of average values for each UN georegions (r2, r1) ----
  # calculates georegional averages using cleandata >= 1 of the countries in that georegion are present. We exclude r0; we would want a different solution if that were necessary
  
  # calculate mean values of r2, r1 for each year using values from cleandata
  d_r2a = cleandata %.%
    left_join(gf, by='rgn_id') %.%
    group_by(r2, year); head(d_r2a)
  
  d_r2 = d_r2a %.%  # Have to split this chain so can save whence information below...
    dplyr::summarize(r2mean = mean(value, na.rm=T)) %.%  # if this gives errors check to make sure this is dplyr::summarize not plyr::summarize. See detach() above
    mutate(whence_choice = rep('r2')); head(d_r2)
  
  d_r1a = cleandata %.%
    left_join(gf, by='rgn_id') %.%
    group_by(r1, year); head(d_r1a)
  
  d_r1 = d_r1a %.%
    dplyr::summarize(r1mean = mean(value, na.rm=T)) %.%
    mutate(whence_choice = rep('r1')); head(d_r1)
  
  
  # work with the rgn_ids that must be gapfilled ----
  
  # identify which rgn_ids are missing from cleandata (using anti_join); then left_join to the UN georegions. 
  rgn_to_gapfill_tmp = rf %.%
    select(rgn_id, rgn_nam) %.%
    anti_join(cleandata, by='rgn_id') %.%
    left_join(gf %.% 
                select(rgn_id, r2, r1), 
              , by='rgn_id') %.%
    mutate(year = unique(cleandata$year)[1]) %.%  
    arrange(rgn_id); head(rgn_to_gapfill_tmp)
  
  # create rows in rgn_to_gapfill_tmp for each unique year
  ind = c((rgn_to_gapfill_tmp$rgn_id %in% 213) | (!rgn_to_gapfill_tmp$r2 %in% NA)) # removes open ocean and disputed 
  year_uni = as.data.frame(unique(cleandata$year)) 
  names(year_uni) = 'year'
  year_uni$year = as.integer(year_uni$year) 
  year_uni = year_uni %.%
    arrange(year)
  
  # must create this in two steps otherwise, otherwise years do not align with regions and duplicates are introduced
  rgn_to_gapfill_tmp2 = data.frame(rgn_id  = rep(rgn_to_gapfill_tmp$rgn_id[ind], dim(year_uni)[1]), 
                                   rgn_nam = rep(rgn_to_gapfill_tmp$rgn_nam[ind], dim(year_uni)[1]), 
                                   r2      = rep(rgn_to_gapfill_tmp$r2[ind], dim(year_uni)[1]),
                                   r1      = rep(rgn_to_gapfill_tmp$r1[ind], dim(year_uni)[1])) %.%
    arrange(rgn_id);
  rgn_to_gapfill = data.frame(rgn_to_gapfill_tmp2,  
                              year_uni)  
  ####
  
  #   rgn_to_gapfill = data.frame(rgn_id  = rep(rgn_to_gapfill_tmp$rgn_id[ind], dim(year_uni)[1]), 
  #                               rgn_nam = rep(rgn_to_gapfill_tmp$rgn_nam[ind], dim(year_uni)[1]), 
  #                               r2      = rep(rgn_to_gapfill_tmp$r2[ind], dim(year_uni)[1]),
  #                               r1      = rep(rgn_to_gapfill_tmp$r1[ind], dim(year_uni)[1]),
  #                               year    = year_uni)
  #   rgn_to_gapfill = arrange(rgn_to_gapfill, rgn_id, year); head(rgn_to_gapfill)
  
  
  ## gapfill data
  
  # join regions to be gapfilled with georegional averages for each year available  
  rgn_gapfilled = rgn_to_gapfill %.%
    left_join(d_r2, by=c('r2', 'year')) %.%
    # select(rgn_id:year, value=r2mean) %.% ## this was an attempt to rename and isn't working; lines 2 below do this instead
    mutate(whence_choice = rep('r2')); head(rgn_gapfilled) 
  rgn_gapfilled$value = rgn_gapfilled$r2mean; rgn_gapfilled$r2mean = NULL # hack because select isn't allowing
  
  # if no r2 georegional average, join with r1 georegional averages for each year available, and combine back with rgn_gapfilled  
  if(sum(is.na(rgn_gapfilled$value))>0){
    rgn_gapfilledb = rgn_gapfilled %.%
      filter(is.na(value)) %.% 
      select(rgn_id, rgn_nam, year, r1, r2) %.%
      left_join(d_r1, by=c('r1', 'year')) %.%
      select(rgn_id, rgn_nam, r1, r2, year, whence_choice, value=r1mean); head(rgn_gapfilledb)     
    
    # hardcode identifiers for southern islands
    rgn_gapfilledb$value[rgn_gapfilledb$r2 == 999] = s_island_val
    rgn_gapfilledb$whence_choice[rgn_gapfilledb$r2 == 999] = 'XSI'
    
    # combine rgn_gapfilled so it is a combination of r2 and either/and r1, xsi
    rgn_gapfilled = rbind(filter(rgn_gapfilled, !is.na(value)),
                          rgn_gapfilledb)      
  }
  
  # combine gapfilled data with original data; save ----
  
  # prepare to combine; add whencev01 columns
  rgn_gapfilled = rgn_gapfilled %.%
    mutate(whencev01 = rep('SG', length(rgn_gapfilled$year))) %.%
    arrange(rgn_id, year); head(rgn_gapfilled) 
  
  cleandata = cleandata %.%
    mutate(whencev01 = rep('OD')) %.%
    mutate(whence_choice = rep('OD')); head(cleandata)
  
  # combine finally
  findat = rbind(cleandata, 
                 select(rgn_gapfilled, rgn_id, year, value, whencev01, whence_choice)); head(findat)
  findat$rgn_id = as.numeric(findat$rgn_id)
  findat$year = as.numeric(findat$year)
  finaldata = findat %.%
    arrange(rgn_id, year)
  n2 = c(n, 'whencev01', 'whence_choice')
  names(finaldata) = n2; head(finaldata) # rename original header
  
  print('Final data layer saved: ')
  print(paste(layersave, '.csv', sep=''))
  write.csv(finaldata, file.path(dirsave, paste(layersave, '.csv', sep='')), na = '', row.names=FALSE)
  
  # whence bookkeeping ----
  
  # join each rgn_id that was gapfilled, by year, with all rgn_ids that had data
  # within the georegion (identified by whence_choice)
  
  # whence first with r2
  rgn_gapfilled_whence_r2 = rgn_gapfilled %.%
    left_join(d_r2a %.% 
                select(year, r2,
                       rgn_id_whence = rgn_id,
                       rgn_nam_whence = rgn_nam,
                       value_whence = value), 
              by=c('r2', 'year')) %.%
    select(rgn_id, rgn_nam, r2, r1, whence_choice, year, value, rgn_id_whence, rgn_nam_whence, value_whence)
  head(rgn_gapfilled_whence_r2,10)
  
  # whence then with r1
  rgn_gapfilled_whence_r1 = rgn_gapfilled_whence_r2 %.%
    filter(is.na(rgn_id_whence)) %.%
    select(rgn_id:value) %.%
    left_join(d_r1a %.% 
                select(year, r1,
                       rgn_id_whence = rgn_id,
                       rgn_nam_whence = rgn_nam,
                       value_whence = value), by=c('r1', 'year')) %.%
    select(rgn_id, rgn_nam, r2, r1, whence_choice, year, value, rgn_id_whence, rgn_nam_whence, value_whence)
  head(rgn_gapfilled_whence_r1)
  
  # combine r2 and r1
  rgn_gapfilled_whence = rgn_gapfilled_whence_r2 %.%
    filter(!is.na(rgn_id_whence)) %.%
    rbind(rgn_gapfilled_whence_r1) %.%
    arrange(rgn_id, year, rgn_id_whence) %.%
    plyr::rename(replace=c('value' = value_nam, 
                           'value_whence' = paste(value_nam, '_whence', sep=''))); head(rgn_gapfilled_whence,20)
  
  write.csv(rgn_gapfilled_whence, file.path(dirsave, paste(layersave, '_whencev01.csv', sep='')), na = '', row.names=FALSE)   
}


# JS  # sovereignty
add_gapfill_sov = function(cleaned_data, dirsave, layersave,
                           dpath = 'src/LookupTables',   
                           rgn_master.csv     = file.path(dpath, 'eez_rgn_2013master.csv')) {
  
  # debug:: dpath = 'src/LookupTables'; rgn_master.csv= file.path(dpath, 'eez_rgn_2013master.csv')
  
  
  # setup ----
  print('-->>> add_gapfill_sov.r gives children the parent values based on sovereignty')
  
  # load libraries
  library(dplyr)
  
  # read in lookup files
  rk = read.csv(rgn_master.csv); head(rk) # master file
  rkf = rk %.%
    filter(rgn_id_2013 < 255) # remove disputed and open ocean
  
  #tidy cleandata
  cleandata = cleaned_data
  n = names(cleandata)
  value_nam = names(cleandata)[!names(cleandata) %in% c('rgn_id', 'year')]
  names(cleandata)[!names(cleandata) %in% c('rgn_id', 'year')] = 'value';  head(cleandata)
  
  
  ## identify gaps and fill ----
  
  # join cleandata with master sovereignty list and identify regions to gapfill
  d_sov = cleandata %.%
    left_join(rkf %.%
                select(rgn_id = rgn_id_2013, 
                       rgn_nam = rgn_nam_2013, 
                       sov_id, sov_nam),
              by='rgn_id'); head(d_sov)
  
  
  # identify which rgn_ids are missing from cleandata (using anti_join); then left_join to the sovereign regions. 
  rgn_to_gapfill = rkf %.%
    select(rgn_id = rgn_id_2013, 
           rgn_nam = rgn_nam_2013, 
           sov_id, sov_nam) %.%
    anti_join(cleandata, by='rgn_id') %.%
    arrange(rgn_id); head(rgn_to_gapfill)
  
  
  # gapfill using d_sov created above
  rgn_gapfilled = rgn_to_gapfill %.%
    left_join(d_sov %.%
                select(value, year, sov_id, sov_nam),
              by=c('sov_id', 'sov_nam')); head(rgn_gapfilled) 
  
  
  # combine gapfilled data with original data; save ----
  
  
  # prepare to combine; add whencev01 columns
  rgn_gapfilled = rgn_gapfilled %.%
    mutate(whencev01 = rep('SCG', length(rgn_gapfilled$year))) %.%
    arrange(rgn_id, year); head(rgn_gapfilled) 
  
  cleandata = cleandata %.%
    mutate(whencev01 = rep('OD')) %.%
    mutate(whence_choice = rep('OD')); head(cleandata)
  
  # combine finally
  findat = rbind(cleandata, 
                 select(rgn_gapfilled, rgn_id, year, value, whencev01,
                        whence_choice = sov_nam)); head(findat)
  findat$rgn_id = as.numeric(findat$rgn_id)
  findat$year = as.numeric(findat$year)
  finaldata = findat %.%
    arrange(rgn_id, year)
  n2 = c(n, 'whencev01', 'whence_choice')
  names(finaldata) = n2; head(finaldata) # rename original header
  
  print('Final data layer saved: ')
  print(paste(layersave, '.csv', sep=''))
  write.csv(finaldata, file.path(dirsave, paste(layersave, '.csv', sep='')), na = '', row.names=FALSE)
  
  print('..')
  print('Check to see whether any further georegional gapfilling is required')
  
  # return(finaldata) # this is how it was done in the past
  
}
