# Data Prep functions to use for OHI cleaning and gapfilling 

# created fall 2013 by JStewart; updated March 2014 by JStewartLowndes
# location of original file on Neptune local_edit: /src/R/jstewart/ohi_clean_fxns.R

# add rgn_id
add_rgn_id = function(uidata, uifilesave) {
  # example: uidata     = '/Volumes/ohi/Work/2013Update/data/GL-FAO-Commodities/data/cleaned/GL-FAO-Commodities.csv'
  #          uifilesave = '/Volumes/ohi/Work/2013Update/data/GL-FAO-Commodities/data/cleaned/GL-FAO-Commodities-cleaned.csv'
  
  # add_ISO.r: use SQLite to add OHI region codes and save as new file (J. Stewart, B. Best Apr 2013) 
  #   read in user-specified data file that needs ISO codes (tbd)
  #   rename anything with accents (Cote d'Ivoire->Ivory Coast, Reunion, Republique)
  #   read in official list of OHI regions (countries) and ISO codes: eez_rgn_2013master.csv (this is BB's file based on BH's xls for Radical)
  #   read in supplementary list of OHI regions/ISO codes: rgn_eez_v2013a_synonyms.csv (this is JS's file based on BB's file with 2-letter OHI regionkeys. Also exists one saved from BH's Radical file with 3-digit codes)
  #   query user-specified file to the ISO codes and save as a new file
  #   
  #   Note: the beginnings of regions_eezSynonyms.xlsx came from here, included at the bottom
  
  print('-->>> add_rgn_id.r expects that the first two columns of the matrix will be country_name, value_units ')
  print('.')
  print('..')
  
  library(reshape2)
  library(gdata)
  options(gsubfn.engine = "R") # otherwise, get X11 launching for sqldf package
  require(sqldf)
  #   library(plyr)
  #   library(dplyr)
  
  dpath = '/Users/jstewart/github/ohiprep/src/LookupTables' # fix this with more portable code
  
  
  # remove accents
  col_num = grep('country', names(uidata), ignore.case = TRUE)
  names(uidata)[col_num] = 'country_id'
  uidata[,col_num] = gsub('^\'', '', uidata[,col_num]) # get rid of any errant quotes
  uidata[,col_num] = gsub('.+voire', 'Ivory Coast', uidata[,col_num]) # Ivory Coast
  uidata[,col_num] = gsub('.+union', 'Reunion', uidata[,col_num]) # Reunion
  uidata[,col_num] = gsub('.+publique du', 'Republic of', uidata[,col_num]) # Congo
  uidata[,col_num] = gsub('Cura.+', 'Curacao', uidata[,col_num]) # Curacao 
  uidata[,col_num] = gsub('Saint Barth.+', 'Saint Barthelemy', uidata[,col_num]) # Saint Barthelemy 
  uidata[,col_num] = gsub('.+Principe', 'Sao Tome and Principe', uidata[,col_num]) # Sao Tome and Principe
  
  
  ## read in more offical (by BB) and redundant (by JS) lists with 2-letter OHI region codes, combine into one data.frame 
  rk = read.csv(file.path(dpath, 'eez_rgn_2013master.csv'))
  rk2 = read.csv(file.path(dpath, 'rgn_eez_v2013a_synonyms.csv'))
  
  # manage official region_id data
  rk = rk[rk$rgn_id_2013 < 255,]# remove high seas and non-regions
  rk$rgn_typ = rep(NA, length(rk[,1]))
  rk$rgn_typ[!is.na(rk$rgn_id_2013)] = 'ohi_region'
  
  # manage synonym region_id data
  rkb = data.frame(rk$rgn_id_2013, rk$rgn_key_2013, rk$rgn_nam_2013, rk$region_id_2012, rk$rgn_typ) 
  rk2b = data.frame(rk2$rgn_id_2013, rk2$rgn_key_2013, rk2$rgn_nam_2013, rk2$region_id_2012, rk2$rgn_typ)
  
  # combine official and synonym region_id data
  names(rkb) = c('rgn_id_2013', 'rgn_key_2013', 'rgn_nam_2013', 'region_id_2012', 'rgn_typ'); names(rk2b) = c('rgn_id_2013', 'rgn_key_2013', 'rgn_nam_2013', 'region_id_2012', 'rgn_typ')
  regionkey = rbind(rkb, rk2b)
  
  
  
  
  ## sqlite: it isn't case sensitive so this should be fine if uidata is labeled Country or country. 
  uidata2 = sqldf("SELECT b.rgn_id_2013, a.*, b.rgn_typ
                 FROM uidata AS a
                 LEFT OUTER JOIN (
                     SELECT DISTINCT rgn_nam_2013, rgn_key_2013, rgn_id_2013, rgn_typ  
                     FROM regionkey 
                     ) AS b ON b.rgn_nam_2013 = a.country_id") 
  
  ## remove landlocked rows from datafiles, leaving only ohi regions and NA regions: need to assign those later.  
  unique(uidata2$rgn_typ)
  uidata3 = subset(uidata2, (rgn_typ == 'ohi_region' | is.na(rgn_typ))) # keep NAs because they need to be assigned later on
  #uidata3 = uidata3[uidata3$rgn_id < 255,] # Also removed disputed. for high-seas stuff, would want this to just say !=255 to exclude disputed areas
  
  # indicate which were removed
  print('These landlocked/largescale countries were removed:')
  RemovedMatrix = subset(uidata2, (rgn_typ == 'landlocked' | rgn_typ == 'largescale' | rgn_typ == 'disputed')) 
  RemovedCountry = data.frame(RemovedMatrix$country_id)
  print(unique(RemovedCountry))
  print('')
  
  # indicate which still need to be assigned:
  print('These non-landlocked countries were not matched with OHI rgn_id codes:')
  uidata3.na = subset(uidata3,  is.na(rgn_typ)) 
  print(unique(data.frame(uidata3.na$country_id)))
  
  print('TRUE if everything is working properly: ')
  print(dim(uidata3)[1] + dim(RemovedMatrix)[1] == dim(uidata2)[1]) # make sure this is TRUE
  
  uidata4 = uidata3
  
  ## save
  uidata4$rgn_typ <- NULL
  # uidata4$country_id <- NULL # we need this in there to ID which countries aren't matched. remove in add_gapfill.r
  names(uidata4)[c(1,2)] = c('rgn_id', 'rgn_nam')
  uidata4 = uidata4[order(uidata4$rgn_id),]
  
  
  print('Be sure to inspect saved .csv file for additional or missing rgn_ids.')
  write.csv(uidata4, uifilesave, na = '', row.names=FALSE)
  
  #  write.table(regionkey, '/Users/jstewart/Desktop/regionkeytest.txt', sep='\t', row.names=FALSE)
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
      if (nrow(d.id.yrs) < 2){
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


add_gapfill = function(cleandata, dirsave, layersave, s_island_val=NULL, 
                       dpath = '/Users/jstewart/github/ohiprep/src/LookupTables',   
                       rgn_georegions.csv = file.path(dpath, 'rgn_georegions_wide_2013b.csv'),
                       rgns.csv           = file.path(dpath, 'rgn_details.csv')) {
  
  # debug: cleaned_data=s; layersave=file.path(td, 'sanitation_gapfilled_2013b.csv'); s_island_val=NULL; dpath = '/Users/jstewart/github/ohiprep/src/LookupTables'; rgn_georegions.csv = file.path(dpath, 'rgn_georegions_wide_2013b.csv'); rgns.csv = file.path(dpath, 'rgn_details.csv')
  
  
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
  
  # create lookup tables of average values for each UN georegions (r2, r1) ----
  # calculates georegional averages using cleandata >= 1 of the countries in that georegion are present. We exclude r0; we would want a different solution if that were necessary
  
  # calculate mean values of r2, r1 for each year using values from cleandata
  d_r2a = cleandata %.%
    left_join(gf, by='rgn_id') %.%
    group_by(r2, year); head(d_r2a)
  #delete mutate(whence_choice = rep('r2')) # unfortunately need this here and just below because summarize() will remove it
  
  d_r2 = d_r2a %.%  # Have to split this chain so can save whence information below...
    summarize(r2mean = mean(value, na.rm=T))%.%  
    mutate(whence_choice = rep('r2')); head(d_r2)
  
  d_r1 = cleandata %.%
    left_join(gf, by='rgn_id') %.%
    group_by(r1, year) %.%
    summarize(r1mean = mean(value, na.rm=T)) %.%
    mutate(whence_choice = rep('r1'))
  
  
  # work with the rgn_ids that must be gapfilled ----
  
  # identify which rgn_ids are missing from cleandata (using anti_join); then left_join to the UN georegions  
  rgn_to_gapfill = rf %.%
    select(rgn_id, rgn_nam) %.%
    anti_join(cleandata, by='rgn_id') %.%
    left_join(gf %.% 
                select(rgn_id, r2, r1), 
              , by='rgn_id') %.%
    arrange(rgn_id); head(rgn_to_gapfill)
  
  # join regions to be gapfilled with georegional averages for each year available  
  rgn_gapfilled = rgn_to_gapfill %.%
    left_join(d_r2, by='r2') %.%
    # select(rgn_id:year, value=r2mean) %.% ## this was an attempt to rename and isn't working; lines 2 below do this instead
    mutate(whence_choice = rep('r2')); head(rgn_gapfilled) 
  rgn_gapfilled$value = rgn_gapfilled$r2mean; rgn_gapfilled$r2mean = NULL # hack because select isn't allowing
  
  # if no r2 georegional average, join with r1 georegional averages for each year available, and combine back with rgn_gapfilled  
  if(sum(is.na(rgn_gapfilled$value))>0){
    rgn_gapfilledb = rgn_gapfilled %.%
      filter(is.na(value), !is.na(r2)) %.% # also removes open ocean and disputed 
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
    mutate(whencev01 = rep('SG', length(rgn_gapfilled$year))); head(rgn_gapfilled) 
  
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
  
  # join each rgn_id that was gapfilled, by year, with all rgn_ids that had data within the georegion (identified by whence_choice)
  rgn_gapfilled_whence = rgn_gapfilled %.%
    left_join(d_r2a %.%
                select(year, r2,
                       rgn_id_whence = rgn_id,
                       rgn_nam_whence = rgn_nam,
                       value_whence = value), 
              by=c('r2', 'year')) %.%
    arrange(rgn_id, year, rgn_id_whence) %.%
    select(rgn_id, rgn_nam, r2, r1, whence_choice, year, value, rgn_id_whence, rgn_nam_whence, value_whence) ; head(rgn_gapfilled_whence,20)
  rgn_gapfilled_whence = rename(rgn_gapfilled_whence, replace=c('value' = value_nam, 'value_whence' = paste(value_nam, '_whence', sep='')))

  write.csv(rgn_gapfilled_whence, file.path(dirsave, paste(layersave, '_whencev01.csv', sep='')), na = '', row.names=FALSE)   
}


# JS
add_gapfill_singleyear = function(cleaned_data, layersave, s_island_val=NULL,
                                  rgn_georegions.csv = '/Volumes/data_edit/model/GL-NCEAS-OceanRegions_v2013a/manual_output/rgn_georegions_wide_2013b.csv',
                                  rgns.csv           = '/Volumes/data_edit/model/GL-NCEAS-OceanRegions_v2013a/data/rgn_details.csv') {
  # use SQLite to add UN gapfilling regions and save as new file (J. Stewart, Aug 2013) 
  
  
  print('-->>> add_gapfill.r substitutes UN georegions means for NA values')
  print('.')
  print('..')
  
  library(reshape2)
  library(gdata)
  library(plyr)
  options(gsubfn.engine = "R") # otherwise, get X11 launching for sqldf package
  require(sqldf)
  
  
  # FIRST, average each UN r2 georegion and each UN r1 georegion
  # This will provide an r2 average if at least one of the countries in that region are present.
  
  # read in files
  gf = read.csv(rgn_georegions.csv)
  
  #tidy cleaned_data
  cleandata = cleaned_data
  cleandata$rgn_nam = NULL
  n = names(cleandata)
  names(cleandata)[2] = 'value' # BB: what if year is the second column? this breaks.
  
  # join r2 to cleandata
  clean_r2 = sqldf("SELECT a.*, b.r2, b.r1, b.r0
                 FROM cleandata AS a
                 LEFT OUTER JOIN (
                     SELECT rgn_id, r2, r1, r0
                     FROM gf
                     ) AS b ON b.rgn_id = a.rgn_id") 
  
  # transpose then recast, calculating mean value of r2 grouped by year. And for r1
  r2mean_long = dcast(clean_r2, r2 ~ r0, fun.aggregate = mean, na.rm=T) # mean(clean_r2$value[clean_r2$r2 == 5]) # quick check
  names(r2mean_long)[2] = 'r2mean'
  
  r1mean_long = dcast(clean_r2, r1 ~ r0, fun.aggregate = mean, na.rm=T) 
  names(r1mean_long)[2] = 'r1mean'
  
  r0mean_long = data.frame(clean_r2$r0, mean(clean_r2$value))   
  names(r0mean_long) = c('r0', 'r0mean')
  r0mean_long = r0mean_long[1,]
  
  # SECOND, use the master OHI region list (to identify which rgn_ids are missing) and join to the UN georegions so they can then be joined to cleandata to see how to gapfill. 
  
  # read in master OHI list with 2letter code; region ids
  rk = read.csv('/Volumes/data_edit/model/GL-NCEAS-OceanRegions_v2013a/manual_output/eez_rgn_2013master.csv')
  rk = rk[order(rk$rgn_id),]
  rkx = rk[rk$rgn_id < 255,]# remove high seas and non-regions
  rk_uni = unique(rkx)
  rgn_tofill = sqldf("SELECT a.rgn_id_2013, a.rgn_nam_2013
                 FROM rk_uni AS a
                 LEFT OUTER JOIN (
                     SELECT DISTINCT rgn_id
                     FROM cleandata
                     ) AS b ON b.rgn_id = a.rgn_id_2013
                  WHERE b.rgn_id IS null") 
  
  # read in gap-filling UN region document and join to rgn_tofill
  rgn_gf = sqldf("SELECT a.rgn_id_2013, a.rgn_nam_2013, b.r2, b.r1
                 FROM rgn_tofill AS a
                 LEFT OUTER JOIN (
                     SELECT rgn_id, r2, r1
                     FROM gf
                     ) AS b ON b.rgn_id = a.rgn_id_2013") 
  
  # THIRD, join r2s and gapfill
  
  trixtmp = r2mean_long
  rgn_gf_r2 = sqldf("SELECT a.*, b.r2mean
                          FROM rgn_gf AS a
                          LEFT OUTER JOIN (
                          SELECT r2, r2mean
                          FROM trixtmp
                          ) AS b ON b.r2 = a.r2") 
  
  # fix a few NAs by hand
  rgn_gf_r2$r2mean[rgn_gf_r2$r2 == 999] = s_island_val # sub in the southern island regions
  rgn_gf_r2 <- rgn_gf_r2[rgn_gf_r2$rgn_id != 213,] # remove Antarctica
  
  # identify when rgn_gf_r2 is na (=no data for that r2 region)
  idx = which(is.na(rgn_gf_r2$r2mean)) 
  
  if(length(idx) > 0) {
    trixtmp2 = r1mean_long
    rgn_gf_r2r1 = sqldf("SELECT a.*, b.r1mean
                 FROM rgn_gf_r2 AS a
                 LEFT OUTER JOIN (
                     SELECT r1, r1mean
                     FROM trixtmp2
                     ) AS b ON b.r1 = a.r1") 
  }
  
  # switch out r1mean where r2mean=NA
  rgn_gf_combo = rgn_gf_r2r1
  rgn_gf_combo$r21mean = rgn_gf_combo$r2mean
  rdex = which((is.na(rgn_gf_combo$r21mean) & rgn_gf_combo$r2 != 999))
  rgn_gf_combo$r21mean[rdex] = rgn_gf_combo$r1mean[rdex]
  #r0dex = which(rgn_gf_combo$r21mean == NA & rgn_gf_combo$r2 != 999)
  rgn_gf_combo = data.frame(cbind(rgn_gf_combo$rgn_id_2013, rgn_gf_combo$r21mean))  
  names(rgn_gf_combo) = c('rgn_id', 'r21mean')
  
  rgn_gf_r2_all = rgn_gf_combo
  
  
  #TODO: logic to fill for the world
  
  # prep to combine, account for category column
  clean_r2b = clean_r2
  clean_r2b$r2 = NULL; clean_r2b$r1 = NULL; clean_r2b$r0 = NULL
  
  col_diff = dim(clean_r2b)[2] - dim(rgn_gf_r2_all)[2]
  if(col_diff != 0) {
    if(col_diff == 1) {
      rgn_gf_r2_all$units = rep(clean_r2b$units[1], dim(rgn_gf_r2_all)[1])
    } else {
      print('problem with too many data columns')
    }
  }
  
  names(clean_r2b) = n
  names(rgn_gf_r2_all) = n
  
  # combine finally:
  findat = rbind(clean_r2b, rgn_gf_r2_all)
  finaldata = findat[order(findat$rgn_id),]
  
  
  
  print('Final data layer saved: ')
  print(layersave)
  write.csv(finaldata, layersave, na = '', row.names=FALSE)
}


# JS
add_gapfill_sov = function(cleaned_data) {
  # sovereignty
  # use SQLite to add UN gapfilling regions and save as new file (J. Stewart, Aug 2013) 
  
  
  print('-->>> add_gapfill_sov.r gives children the parent values based on sovereignty')
  print('.')
  print('..')
  
  library(reshape2)
  library(gdata)
  library(plyr)
  options(gsubfn.engine = "R") # otherwise, get X11 launching for sqldf package
  require(sqldf)
  
  cleandata = cleaned_data
  n = names(cleaned_data)
  names(cleandata)[3] = 'value'
  
  # SECOND, use the master OHI region list (to identify which rgn_ids are missing) and join to the sovereignty so they can then be joined to cleandata to see how to gapfill. 
  
  # read in master OHI list with 2letter code; region ids
  rk = read.csv('/Volumes/data_edit/model/GL-NCEAS-OceanRegions_v2013a/manual_output/eez_rgn_2013master.csv')
  rk = rk[order(rk$rgn_id_2013),]
  rkx = rk[rk$rgn_id_2013 < 255,]# remove high seas and non-regions
  rk_uni = unique(rkx)
  rgn_tofill = sqldf("SELECT a.rgn_id_2013, a.rgn_nam_2013, a.sov_id, a.sov_nam
                 FROM rk_uni AS a
                 LEFT OUTER JOIN (
                     SELECT DISTINCT rgn_id
                     FROM cleandata
                     ) AS b ON b.rgn_id = a.rgn_id_2013
                  WHERE b.rgn_id IS null") 
  
  # THIRD, join and gapfill
  # do this for every year: 
  
  yrtrix = unique(cleandata$year)
  rgn_gf_sov_all = matrix(nrow=0, ncol=0)
  for(yr in yrtrix) {
    trixtmp = cleandata[cleandata$year == yr,]
    
    rgn_gf_sov = sqldf("SELECT a.*, b.value, b.year
                          FROM rgn_tofill AS a
                          LEFT OUTER JOIN (
                          SELECT value, rgn_id, year
                          FROM trixtmp
                          ) AS b ON b.rgn_id = a.sov_id") 
    
    
    rgn_gf_sov_all = rbind(rgn_gf_sov_all, rgn_gf_sov)
    
  }
  
  rgn_gf_sov_all = rgn_gf_sov_all[order(rgn_gf_sov_all$rgn_id, rgn_gf_sov_all$year),]
  
  cleaned_data_sov = rgn_gf_sov_all[,c(1,2,dim(rgn_gf_sov_all)[2]-1, dim(rgn_gf_sov_all)[2])]
  names(cleaned_data_sov) = n
  
  return(cleaned_data_sov)
  
}
