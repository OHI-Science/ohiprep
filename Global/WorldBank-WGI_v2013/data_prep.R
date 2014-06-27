# data_prep.R. 
# Add rgn_ids for World Bank WGI (World Governance Indicators)
# Previously had been named clean_WGI.r (by JStewart May2013). This script created by JStewartLowndes Mar2014.

# gapfilling: sovereignty (parent-children) gapfilling with add_gapfill_sov.r


# setup ----

# load libraries
library(gdata)
library(tools)
library(ohicore) # devtools::install_github('ohi-science/ohicore') # may require uninstall and reinstall

# get paths.  NOTE: Default path should be ohiprep root directory.
source('src/R/common.R') # set dir_neptune_data
source('src/R/ohi_clean_fxns.R') # has functions: cbind_rgn(), sum_na()
dir_d = 'Global/WorldBank-WGI_v2013'


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
  
  d = read.xls(file.path(dir_d, 'raw', filein), sheet=s, blank.lines.skip=F, skip=15, header=F, col.names=hdr); head(d)
  
  # melt data
  d.m = melt(data=d, id.vars=names(d)[1:2], variable.name='stat')
  names(d.m) = c('country','countryID','stat','value');head(d.m)
  
  # change NAs
  d.m$value[d.m$value == '#N/A'] = NA 
  
  # split stat (the combined header variable created above) and add WGI indicator
  v = strsplit(as.character(d.m$stat), '\\.') 
  d.m = d.m %.%
    mutate(year   = sapply(v, function(x) x[2]),
           metric = sapply(v, function(x) x[1]), 
           WGI    = wgisheetNames[s]); head(d.m)
  
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

# prepare to add rgn_ids: d.all2 would be final, but must combine the 6 separate indicators 
d.all2 = d.all %.%
  select(country,
         score = Estimate,
         year,
         category = WGI)

# calculate average score ----

# transpose with mean aggregate function to average over 6 indicator subcategories
d.all2$score = as.numeric(d.all2$score)
d.t = dcast(d.all2, value.var="score", country ~ year, fun.aggregate = mean, na.rm = T) 

# remelt for final
d.m2 = melt(data=d.t, id.vars=names(d.all)[1], variable.name='year') %.%
  select(country, 
         score = value,
         year) %.%
  arrange(country, year)

# rescale
rng = c(-2.5, 2.5)
d.m2 = within(d.m2,{
  score = (score - rng[1]) / (rng[2] - rng[1])})

# d.m2 <- d.m2[d.m2[,1] != "Jersey, Channel Islands",] # only data for 2011; needs to be gapfilled # don't remove this, but keep an eye on it

# partition Netherland Antilles
ind = d.m2$country %in% c('Netherlands Antilles (former)')
d.m3 = rbind(d.m2[!ind,],
             data.frame(country=c('Sint Maarten', 'Curacao', 'Bonaire', 'Saba', 'Sint Eustasius'), # Aruba reported separately
                        score=rep(d.m2$score[ind], 5),
                        year=rep(d.m2$year[ind], 5)))



##  add rgn_id  ----
m_d = name_to_rgn(d.m3, fld_name='country', flds_unique=c('country','year'), fld_value='score', add_rgn_name=T); head(m_d)


## georegional gapfilling with gapfill_georegions.r ----

# read in lookups
georegions = read.csv('../ohicore/inst/extdata/layers.Global2013.www2013/rgn_georegions_long_2013b.csv', na.strings='') %.%
  dcast(rgn_id ~ level, value.var='georgn_id')

georegion_labels = read.csv('../ohicore/inst/extdata/layers.Global2013.www2013/rgn_georegions_labels_long_2013b.csv') %.%    
  mutate(level_label = sprintf('%s_label', level)) %.%
  dcast(rgn_id ~ level_label, value.var='label') %.%
  left_join(
    read.csv('../ohicore/inst/extdata/layers.Global2013.www2013/rgn_labels.csv') %.%
      select(rgn_id, v_label=label),
    by='rgn_id') %.%
  arrange(r0_label, r1_label, r2_label, v_label); head(georegion_labels)

# gapfill_georegions
layersave = file.path(dir_d, 'data', 'rgn_wb_wgi_2014a.csv')
attrsave  = file.path(dir_d, 'data', 'rgn_wb_wgi_2014a_attr.csv')

# library(devtools); load_all('../ohicore')
d_g_a = gapfill_georegions(
  data = m_d %.%
    filter(!rgn_id %in% c(213,255)) %.%
    select(rgn_id, year, score),
  fld_id = 'rgn_id',
  georegions = georegions,
  georegion_labels = georegion_labels,
  r0_to_NA = TRUE, 
  attributes_csv = (attrsave)) # don't chain gapfill_georegions or will lose head(attr(d_g_a, 'gapfill_georegions')) ability

# investigate attribute tables
head(attr(d_g_a, 'gapfill_georegions'))  # or to open in excel: system(sprintf('open %s', attrsave))

# save gapfilled layer
d_g = d_g_a %.%
  select(rgn_id, year, score) %.%
  arrange(rgn_id, year); head(d_g)

write.csv(d_g, layersave, na = '', row.names=FALSE)

# calculate inverse file and save ----
d_g_inverse = d_g %.%
  mutate(score_inverse = (1-score)) %.%
  select(rgn_id, year,
         score = score_inverse); head(d_g_inverse)

write.csv(d_g_inverse, file.path(paste(file_path_sans_ext(layersave), '_inverse.csv', sep='')), na = '', row.names=FALSE)



## check for duplicate regions, sum them ----

# explore; identify dups
dup = m_d[duplicated(m_d[,c('rgn_id', 'year')]),]; head(dup)
dup_ids = unique(dup$rgn_id) # 116, 209
filter(m_d, rgn_id == 116, year == 1996)
filter(m_d, rgn_id == 209, year == 1996)

# sum duplicates
cleaned_layer_nodup = sum_duplicates(m_d, dup_ids); head(cleaned_layer_nodup)

# confirm no more dups
filter(cleaned_layer_nodup, rgn_id == 116, year == 1996)
filter(cleaned_layer_nodup, rgn_id == 209, year == 1996)

cleaned_layer_nodup$year = as.numeric(as.character(cleaned_layer_nodup$year))

## gapfilling ----

# temporal gapfilling with temporal.gapfill.r
cleaned_layert_tmp = temporal.gapfill(cleaned_layer_nodup, 
                                      fld.id = 'rgn_id', 
                                      fld.value = names(cleaned_layer_nodup)[3], 
                                      fld.year = 'year', verbose=F); head(cleaned_layert_tmp) 
ct = cleaned_layert_tmp; ct$whence = NULL; ct$whence_details = NULL; head(ct) 
ct = ct %.% 
  select(rgn_id, year, score)


# test
a = read.csv('/Users/jstewart/github/ohiprep/Global/WorldBank-WGI_v2013/data/rgn_wb_wgi_2014a.csv')
dup = a[duplicated(a[,c('rgn_id', 'year')]),]; head(dup)

# explore; identify dups
dup = ct[duplicated(ct[,c('rgn_id', 'year')]),]; head(dup)
dup_ids = unique(dup$rgn_id) # 116, 209
filter(m_d, rgn_id == 116, year == 1996)
filter(m_d, rgn_id == 209, year == 1996)


# sovereignty (parent-children) gapfilling with add_gapfill_sov.r 
dirsave = file.path(dir_d, 'data')
layersave = 'rgn_wb_wgi_2014a'
add_gapfill_sov(ct, dirsave, layersave)

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

