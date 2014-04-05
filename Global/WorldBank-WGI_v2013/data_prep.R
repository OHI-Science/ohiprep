# data_prep.R. 
# Add rgn_ids for World Bank WGI (World Governance Indicators)
# Previously had been named clean_WGI.r (by JStewart May2013). This script created by JStewartLowndes Mar2014.

# gapfilling: sovereignty (parent-children) gapfilling with add_gapfill_sov.r


# setup ----

# load libraries
library(reshape2)
library(gdata)
library(dplyr)

# from get paths configuration based on host machine name
source('src/R/common.R') # set dir_neptune_data
# Otherwise, presume that scripts are always working from your default ohiprep folder
dir_d = 'Global/WorldBank-WGI_v2013'

# get functions
source('src/R/ohi_clean_fxns.R')


# **  it takes about 10 mins to make GL-WorldBank-WGI_v2011-cleaned.csv using add_rgn_id.r
## calculations from raw data to add_rgn_id.r ----

filein = 'wgidataset.xlsx'
wgisheetNames = gsub(" ", "", sheetNames(file.path(dir_d, 'raw', filein)))
wgisheets = 2:7 # the six WGI indicators, each on a separate sheet
headerID = 'Country/Territory'

# read in two header rows, collapse into one header title, read in the rest of the data with those header names.
d.all =  matrix(nrow=0, ncol=0)
for (s in wgisheets){ # s=2
  rm(d, d.m, d.c)
  
  hdr = read.xls(file.path(dir_d, 'raw', filein), sheet=s, blank.lines.skip=F, skip=13, nrows=2, header=F) 
  hdr = apply(hdr, 2, function(x) paste(rev(x),collapse='.'))
  hdr[1:2] = gsub('\\.','',hdr[1:2]) # clean two first column headers
  hdr = gsub('\\-','',hdr) # 
  hdr = gsub('.+ank','PercRank',hdr) # make this consistent: P-Rank and Rank were used in different sheets but are same value. 
  
  d = read.xls(file.path(dir_d, 'raw', filein), sheet=s, blank.lines.skip=F, skip=15, header=F, col.names=hdr)
  
  # melt data
  d.m = melt(data=d, id.vars=names(d)[1:2], variable.name='stat')
  names(d.m) = c('country','countryID','stat','value');head(d.m)
  
  # change NAs
  d.m$value[d.m$value == '#N/A'] = NA 
  
  # split stat (the combined header variable created above)
  v = strsplit(as.character(d.m$stat), '\\.') 
  d.m[['year']] = sapply(v, function(x) x[2])
  d.m[['metric']] = sapply(v, function(x) x[1])
  
  # add WGI indicator
  d.m$WGI = rep.int(wgisheetNames[s], length(d.m$value))
  #head(d.m)  
  
  
   # Redo capitalization: capitalize just the first letter: function from R help page for sapply
  capwords <- function(s, strict = FALSE) {
    cap <- function(s) paste(toupper(substring(s, 1, 1)),
                  {s <- substring(s, 2); if(strict) tolower(s) else s},
                             sep = "", collapse = " " )
    sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
  }
  
  d.m$country = tolower(d.m$country)
  d.m$country = sapply(d.m$country, function(x) capwords(x))
  d.m$country = as.character(lapply(as.character(d.m$country), function(x) capwords(x)))
  
  d.m$country = gsub('And ', 'and ', d.m$country, fixed=TRUE) # and a few to do by hand
  d.m$country = gsub('Sar', 'SAR', d.m$country, fixed=TRUE)
  d.m$country = gsub('Fyr', 'FYR', d.m$country, fixed=TRUE)
  d.m$country = gsub('Rb', 'RB', d.m$country, fixed=TRUE)
  d.m$country = gsub('Pdr', 'PDR', d.m$country, fixed=TRUE)
  d.m$country = gsub('u.s.', 'U.S.', d.m$country, fixed=TRUE)
  
  # cast
  d.c = dcast(d.m, country + WGI + year ~ metric) 
  # or by year: dcast(d.m, country + WGI + metric ~ year)
  names(d.c) = c("country","WGI","year","Estimate","Lower","NumSrc","P","StdErr","Upper")
  
  
  # concatenate f files
  d.all = rbind(d.all, d.c)
  
}

# prepare as add_rgn_id expects--d.all2 would be final, but all 6 indicators are separate
d.all2 = d.all[c(1,4,3,2)]
names(d.all2)[c(2,4)] = c('score', 'category')

# calculate average score

# transpose with mean aggregate function to average over 6 indicator subcategories
d.all2$score = as.numeric(d.all2$score)
d.t = dcast(d.all2, value.var="score", country ~ year, fun.aggregate = mean, na.rm = T) 
  
# remelt for final
d.m2 = melt(data=d.t, id.vars=names(d.all)[1], variable.name='year')
d.m3 = d.m2[order(d.m2$country, d.m2$year),]
names(d.m3)[3] = 'score'

# get it in order add_rgn_id expects
d.m4 = d.m3[c(1,3,2)]
d.m4 <- d.m4[d.m4[,1] != "Jersey, Channel Islands",] # only data for 2011; needs to be gapfilled

# partition Netherland Antilles
ind = d.m4$country %in% c('Netherlands Antilles (former)')
d.m5 = rbind(d.m4[!ind,],
  data.frame(country=c('Sint Maarten', 'Curacao', 'Bonaire', 'Saba', 'Sint Eustasius'), # Aruba reported separately
            score=rep(d.m4$score[ind], 5),
            year=rep(d.m4$year[ind], 5)))

## run add_rgn_id and save ----
uifilesave = file.path(dir_d, 'raw', 'GL-WorldBank-WGI_v2011-cleaned.csv')
add_rgn_id(d.m5, uifilesave)

# rescaling ----

cleaned_layer = read.csv(uifilesave)

rng = c(-2.5, 2.5)
cleaned_layer = within(cleaned_layer,{
  score = (score - rng[1]) / (rng[2] - rng[1])})


## check for duplicate regions, sum them ----

# explore; identify dups
dup = cleaned_layer[duplicated(cleaned_layer[,c('rgn_id', 'year')]),]; head(dup)
dup_ids = unique(dup$rgn_id) # 116, 209
filter(cleaned_layer, rgn_id == 116, year == 1996)
filter(cleaned_layer, rgn_id == 209, year == 1996)

# sum duplicates
cleaned_layer_nodup = sum_duplicates(cleaned_layer, dup_ids); head(cleaned_layer_nodup)

# confirm no more dups
filter(cleaned_layer_nodup, rgn_id == 116, year == 1996)
filter(cleaned_layer_nodup, rgn_id == 209, year == 1996)


## gapfilling ----

# temporal gapfilling with temporal.gapfill.r
cleaned_layert_tmp = temporal.gapfill(cleaned_layer_nodup, 
                                      fld.id = 'rgn_id', 
                                      fld.value = names(cleaned_layer_nodup)[3], 
                                      fld.year = 'year', verbose=F); head(cleaned_layert_tmp) 
cleaned_layert = cleaned_layert_tmp; cleaned_layert$whence = NULL; cleaned_layert$whence_details = NULL; head(cleaned_layert) 
cleaned_layert = cleaned_layert %.% 
  select(rgn_id, year, score)
  
# sovereignty (parent-children) gapfilling with add_gapfill_sov.r 
dirsave = file.path(dir_d, 'data')
layersave = 'rgn_wb_wgi_2014a'
add_gapfill_sov(cleaned_layert, dirsave, layersave)

# no further gapfilling required

# calculate inverse file and save ----
cleaned_data_sov =  read.csv(file.path(dir_d, 'data', paste(layersave, '.csv', sep=''))); head(cleaned_data_sov) 
cleaned_data_sov_inverse = cleaned_data_sov %.%
  mutate(score_inverse = (1-score)) %.%
  select(rgn_id, year,
         score = score_inverse, 
         whencev01, whence_choice); head(cleaned_data_sov_inverse)

write.csv(cleaned_data_sov_inverse, 
          file.path(dirsave, paste(layersave, '_inverse.csv', sep='')), na = '', row.names=FALSE)

## --- fin

