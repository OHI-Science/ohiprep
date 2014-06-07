# data_prep.R

# Prepare FAO commodities data for Natural Products goal. 
# By JSLowndes Jun2014; File was originally clean_FAOcommodities.r:(by JStewart Apr2013)

#   read in individual files
#   remove Totals row
#   remove/translate FAO data codes (F, ..., -, 0 0)
#   add identifier column
#   concatenate data from each file into a single file
#   run add_rgn_id.r (function by J. Stewart, B. Best)
#   gafilling: rules like for Mariculture: gapfilling category = TP.
#   save single files for each commodity 


# setup ----


# load libraries
library(reshape2)
library(dplyr)
# devtools::install_github('ohi-science/ohicore') # may require uninstall and reinstall
library(ohicore)  

# from get paths configuration based on host machine name
dir_root = file.path('/Users', Sys.info()[['user']], 'github/ohiprep')
source(file.path(dir_root, 'src/R/common.R')) # set dir_neptune_data
# Otherwise, presume that scripts are always working from your default ohiprep folder

dir_d = 'Global/FAO-Commodities_v2011'
setwd(file.path(dir_root, dir_d, 'raw'))

# get functions
source(file.path(dir_root, 'src/R/ohi_clean_fxns.R'))

# read in and process files ----
for (f in list.files(pattern=glob2rx('*.csv'))){ # f="FAO_raw_commodities_quant_1950_2011.csv"
  
  d = read.csv(f, check.names=F,strip.white=TRUE); head(d)
  
  # clean up and melt
  d.1 = d
  d.1 <- d.1[d.1[,1] != "Totals",] # remove Totals line
  d.1 <- d.1[d.1[,1] != "Yugoslavia SFR",] 
  
  d.m = melt(data=d.1, id.vars=names(d)[1:3], variable.name='year')
  names(d.m) = c('country','commodity','trade','year','value'); head(d.m)
  
  # remove FAO indicators
  # d.m$value = trim(d.m$value) # not necessary with strip.white=T in read.csv() above
  d.m$value = sub(' F', '', d.m$value, fixed=T) # FAO denotes with F when they have estimated the value using best available data
  d.m$value = sub('0 0', '0.1', d.m$value, fixed=T)# FAO denotes something as '0 0' when it is > 0 but < 1/2 of a unit. 
  d.m$value = sub('-', '0', d.m$value, fixed=T) # FAO's 0.
  d.m$value = sub('...', 'NA', d.m$value, fixed=T)
  d.m$value = as.numeric(d.m$value); head(d.m)
  
  # OR...
  #     d.m %.%
  #       mutate(
  #         value = sub(' F', '', value, fixed=T), # FAO denotes with F when they have estimated the value using best available data
  #         value = sub('0 0', '0.1', value, fixed=T),# FAO denotes something as '0 0' when it is > 0 but < 1/2 of a unit. 
  #         value = sub('-', '0', value, fixed=T), # FAO's 0.
  #         value = sub('...', 'NA', value, fixed=T),
  #         value = as.numeric(value)); head(d.m)
  
  
  ## filter data; add category column 
  d.m = d.m %.%
    filter(trade == 'Export') %.%
    select(-trade) %.%
    mutate(category = commodity, 
           year = as.numeric(as.character(factor(year)))); head(d.m) # search in R_inferno.pdf for "shame on you."
  
  ## select all NP subcategories by category
  cat_com = list(
    spg=c(
      'Natural sponges nei',
      'Natural sponges other than raw',
      'Natural sponges raw' 
    ),
    oil=c(
      'Alaska pollack oil, nei',  # none
      'Anchoveta oil',            # none
      'Capelin oil',
      'Clupeoid oils, nei',
      'Cod liver oil', 
      'Fish body oils, nei', 
      'Fish liver oils, nei',
      'Gadoid liver oils, nei',   # none
      'Hake liver oil',
      'Halibuts, liver oils', 
      'Herring oil', 
      'Jack mackerel oil',        # none
      'Menhaden oil',
      'Pilchard oil',             # none
      'Redfish oil', 
      'Sardine oil',
      'Shark liver oil',
      'Shark oil',
      'Squid oil' 
    ),
    swd=c(
      'Agar agar in powder',  
      'Agar agar in strips', 
      'Agar agar nei',
      'Carrageen (Chondrus crispus)',
      'Green laver', 
      'Hizikia fusiforme (brown algae)', 
      'Kelp',
      'Kelp meal',
      'Laver, dry',
      'Laver, nei', 
      'Other brown algae (laminaria, eisenia/ecklonia)', 
      'Other edible seaweeds', 
      'Other inedible seaweeds',
      'Other red algae', 
      'Other seaweeds and aquatic plants and products thereof', 
      'Undaria pinnafitida (brown algae)'
    ),
    orn=c(
      'Ornamental saltwater fish',
      'Ornamental fish nei'
    ), 
    crl=c(
      'Coral and the like' 
      ## note: 'Miscellaneous corals and shells' not included here; see shells below
    ),
    shl=c(
      'Abalone shells',  
      'Miscellaneous corals and shells', 
      'Mother of pearl shells',
      'Oyster shells',
      'Sea snail shells', 
      'Shells nei', 
      'Trochus shells'
    ))
  
  ## create categories from subcategories
  
  # sponges
  d.spg = d.m %.%  
    filter( commodity %in% cat_com[['spg']] ) %.%
    mutate(category = 'spg');  head(d.spg)
  unique(d.spg$commodity)
  summary(d.spg)
  
  # fish oil
  d.oil = d.m %.%  
    filter( commodity %in% cat_com[['oil']] ) %.%
    mutate(category = 'oil');  head(d.oil)
  unique(d.oil$commodity)
  summary(d.oil, maxsum = 30) # TODO: figure out how to report that there are categories not represented
  
  # seaweed and plants
  d.swd = d.m %.%  
    filter( commodity %in% cat_com[['swd']] ) %.%
    mutate(category = 'swd');  head(d.swd)
  unique(d.swd$commodity)
  summary(d.swd, maxsum = 20)
  
  # ornamental fish
  d.orn = d.m %.%  
    filter( commodity %in% cat_com[['orn']] ) %.%
    mutate(category = 'orn');  head(d.orn)
  unique(d.orn$commodity)
  summary(d.orn)
  
  # corals
  d.crl = d.m %.%  
    filter( commodity %in% cat_com[['crl']] ) %.%# 
    mutate(category = 'crl');  head(d.crl)
  unique(d.crl$commodity)
  summary(d.crl)
  
  # shells
  d.shl = d.m %.%  
    filter(  commodity %in% cat_com[['shl']] ) %.%# 
    mutate(category = 'shl');  head(d.shl)
  unique(d.shl$commodity)
  summary(d.shl, maxsum = 10)
  
  
  ## combine all categories back together
  m = rbind_list(d.spg, d.oil, d.swd, d.orn, d.crl, d.shl); head(m) # %.%
  #     write.csv('../tmp/m.csv', row.names=F, na='')
  #   # FUNK restart
  #   library(dplyr)
  #   library(reshape2)
  #   
  #   wd = 'Global/FAO-Commodities_v2011'
  #   m = read.csv(file.path(wd, '/tmp/m.csv'), na.strings='')
  
  m = m %.%
    group_by(country, category, year) %.%
    summarize(value = sum(value, na.rm=T)) #ERROR:::: JSL, June 6 2014 at 5:55pm: run as is, this sums as that single value again...
  
  ## clean up Netherlands Antilles: break into its 6 regions
  sort(unique(m$country)) # Netherlands Antilles
  ant_cntry = c('Aruba', 'Bonaire', 'Curacao', 'Saba', 'Sint-Maarten', 'Sint-Eustasius') # correct 6
  m_ant = filter(m, country=='Netherlands Antilles')
  m_ant_exp = m_ant %.%
    as.data.frame() %.%
    mutate(
      value = value/6,
      'Aruba' = value,
      'Bonaire' = value,
      'Curacao' = value,
      'Saba' = value,
      'Sint-Maarten' = value,
      'Sint-Eustasius' = value) %.%
    select(-value, -country) %.%
    melt(id=c('category','year'), variable.name='country')
  
  #   list.files('../ohicore/inst/extdata/layers.Global2013.www2013')
  #   read.csv('../ohicore/inst/extdata/layers.Global2013.www2013/rgn_labels.csv') %.%
  #   arrange(label)
  #   head()
  # check for duplicates: m[ duplicated(m[,c('country','category','year')]), ]
  
  # rbind Netherlands Antilles regions with original data, arrange
  m2 = m %.%
    filter(country !='Netherlands Antilles') %.%
    rbind(m_ant_exp) %.%
    arrange(country, value, year, category); head(m2) # prepare for the order add_rgn_id.r expects
  m2$year = as.numeric(as.character(factor(d.m2$year))) 
  summary(d.m2)
  
  # layer-specific further processing for quantity and value
  a = strsplit(f, '_', fixed=FALSE)
  layer = unlist(a)[4]
  b = strsplit(dir_d, '/', fixed=FALSE)
  
  if(layer == 'quant'){
    d.final = d.m2 %.%
      select(country, year, 
             tonnes = value, 
             category); head(d.final)
  } else if (layer == 'value'){
    d.m2$value = d.m2$value * 1000 # FAO reports in 'thousands of US dollars'
    d.final = d.m2 %.%
      select(country, year, 
             USD = value, 
             category); head(d.final)
  }
  
  # save file
  filesave = file.path(dir_root, dir_d, 'data', paste(unlist(b)[2], '_', layer, '-cleaned.csv', sep=''))
  add_rgn_id(d.final, filesave, dpath = file.path(dir_root,'src/LookupTables'))
}


## -----
## ATTENTION 2014a::
## JSL has not updated this gapfilling code for 2014a; this approach needs to be tidied (below was a hack). The only gapfilling that occurs is: if value (tonnes/USD) !=NA for the max(year)-1 but value == NA for the max(year), copy the value from max(year)-1 forward to max(year); this gives wiggle room for regions still in production but not able to report by the FAO deadline...



## gapfilling 2013a::, category TP ----
# as was done in Mariculture. read it in, melt, recast and aggregate by sum, and then remelt and save as individual commodities.
setwd(file.path(dir_root, dir_d, 'data'))
for (f in list.files(pattern=glob2rx('*cleaned.csv'))){ # f="FAO-Commodities_v2011_quant-cleaned.csv"
  
  o = read.csv(f); head(o) 
  
  create rows in rgn_to_gapfill_tmp for each unique year
  ind = !rgn_to_gapfill_tmp$r2 %in% NA
  year_uni = as.data.frame(unique(cleandata$year))
  names(year_uni) = 'year'
  year_uni$year = as.numeric(year_uni$year)
  
  rgn_to_gapfill = data.frame(rgn_id=rep(rgn_to_gapfill_tmp$rgn_id[ind], dim(year_uni)[1]), 
                              rgn_nam=rep(rgn_to_gapfill_tmp$rgn_nam[ind], dim(year_uni)[1]), 
                              r2=rep(rgn_to_gapfill_tmp$r2[ind], dim(year_uni)[1]),
                              r1=rep(rgn_to_gapfill_tmp$r1[ind], dim(year_uni)[1]),
                              year=unique(cleandata$year))
  rgn_to_gapfill = arrange(rgn_to_gapfill, rgn_id, year); head(rgn_to_gapfill)
  
  
  
  # process by hand: assign all Netherlands Antilles children 1/6 of total and save as GL-FAO-AllCombined_v2009-rgn-processed.csv
  
  
  ## gapfilling, category TP, as was done in Mariculture. read it in, melt, recast and aggregate by sum, and then remelt and save as individual commodities. 
  
  # read in file that had a few modifications by hand (see the README.md)
  p = read.csv(paste(dir1, 'data/', 'GL-FAO-AllCombined_v2009-rgn-processed.csv', sep=''))
  p.1 = p
  
  yrt_nadex = which(is.na(p.1$X2009)) # ID where most recent year t are NAs
  yrtminus1_nadex = which(is.na(p.1$X2008)) # ID where year t-1 are NAs
  p.1$X2008[yrtminus1_nadex] = 0 # first fill most recent year NAs with 0 temporarily
  p.1$X2009[yrt_nadex] = p.1$X2008[yrt_nadex] # then replace t NAs with the t-1 values
  p.1$X2008[yrtminus1_nadex] = NA # then change the 2010 value back to NA (probably won't matter for status, but just in case. 
  p.1$gapfilled = rep(0, dim(p.1)[1])
  p.1$gapfilled[yrt_nadex] = 1
  
  p.1end = dim(p.1)[2]
  p.m = melt(data=p.1, id.vars=names(p.1)[c(1:4,p.1end)], variable.name='year')
  p.m$year = sub('X', '', p.m$year, fixed=F) # fix years: remove R's X
  p.m$year = as.numeric(as.character(p.m$year))
  
  # get ready to cast, melt and save as data layer for each layer
  # p.m$layer = gsub('OrnamentalFish', 'orn', p.m$layer) 
  # p.m$layer = gsub('Seaweeds', 'swd', p.m$layer) 
  # p.m$layer = gsub('Shells', 'shl', p.m$layer) 
  # p.m$layer = gsub('Sponges', 'spg', p.m$layer) 
  # p.m$layer = gsub('Coral', 'crl', p.m$layer) 
  # p.m$layer = gsub('FishOil', 'oil', p.m$layer) 
  
  layer_uni = unique(p.m$layer)
  layernames = sprintf('rgn_fao_%s.csv', tolower(layer_uni))
  
  for(i in 1:length(layer_uni)) {
    p.mi = p.m[p.m$layer == layer_uni[i],]
    
    # transpose with sum aggregate function to sum over the commodity subcategory
    p.ti = dcast(p.mi, rgn_id + rgn_nam + layer + gapfilled ~ year, fun.aggregate = sum) 
    
    # remelt
    p.mi2 = melt(data=p.ti, id.vars=names(p.ti)[1:4], variable.name='year')
    
    # prep and save
    p.mi3 = p.mi2[c(1,6,5,4)]
    p.mi3 = p.mi3[order(p.mi3$rgn_id, p.mi3$year),]
    
    layersave = paste(dir1, 'data/', layernames[i], sep='') 
    write.csv(p.mi3, layersave, na = '', row.names=FALSE)
  }
  
  
  
  
  
  