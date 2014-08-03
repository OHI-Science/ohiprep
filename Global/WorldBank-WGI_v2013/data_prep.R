# data_prep.R. 
# Add rgn_ids for World Bank WGI (World Governance Indicators)
# Previously had been named clean_WGI.r (by JStewart May2013). This script created by JStewartLowndes Mar2014.

# gapfilling: sovereignty (parent-children) gapfilling with gapfill_georegions: flag sovereign


# setup ----

# load libraries
library(gdata)
library(tools)
library(ohicore) # devtools::install_github('ohi-science/ohicore') # may require uninstall and reinstall

# get paths.  NOTE: Default path should be ohiprep root directory.
source('../ohiprep/src/R/common.R') # set dir_neptune_data
source('../ohiprep/src/R/ohi_clean_fxns.R') # has functions: cbind_rgn(), sum_na()
dir_d = '../ohiprep/Global/WorldBank-WGI_v2013'


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

## save tmp file ----
#so can just load this instead of running the whole thing
tmpsave = file.path(dir_d, 'tmp', 'rgn_wb_wgi_2014a_tmp.csv')
# write.csv(d.all, tmpsave, na = '', row.names=F)
d.all = read.csv(tmpsave); head(d.all)

# prepare to add rgn_ids: d.all2 would be final, but must combine the 6 separate indicators 
d.all2 = d.all %.%
  select(country,
         score = Estimate,
         year,
         category = WGI); head(d.all2)

# calculate average score and rescale ----

d.m = d.all2 %.%
  group_by(country, year) %.%
  summarize(score = mean(score, na.rm=T)); head(d.m); summary(d.m)

rng = c(-2.5, 2.5)
d.m2 = within(d.m,{
  score = (score - rng[1]) / (rng[2] - rng[1])}); head(d.m2); summary(d.m2)

# partition Netherland Antilles
ind = d.m2$country %in% c('Netherlands Antilles (former)')
d.m3 = rbind(d.m2[!ind,],
             data.frame(country=c('Sint Maarten', 'Curacao', 'Bonaire', 'Saba', 'Sint Eustasius'), # Aruba reported separately
                        score=rep(d.m2$score[ind], 5),
                        year=rep(d.m2$year[ind], 5)))

##  add rgn_id  ----
m_d = name_to_rgn(d.m3, fld_name='country', flds_unique=c('country','year'), 
                  fld_value='score', add_rgn_name=T, collapse_fxn = mean); head(m_d); summary(m_d) 
# m_d[duplicated(m_d[, c('rgn_id', 'year')]),] 


## sovereign gapfilling with gapfill_georegions.r ----
# use gapfill_georegions: lookup table that has sov_ids and weight the 'parent' country with 1, others with 0

# read in lookups
sovregions = read.csv('../ohiprep/src/LookupTables/eez_rgn_2013master.csv', na.strings='') %.% 
  select(rgn_id = rgn_id_2013,
         r2 = sov_id) %.%     # r2 is actually rgn_ids of sovereign regions
  group_by(rgn_id) %.%                       # remove duplicated countrys from this rgn_id list                    
  summarize(r2 = mean(r2, na.rm=T)) %.% # duplicates always have the same sov_id (r2 value)
  mutate(r1 = r2, 
         r0 = r2,
         fld_wt = as.integer(rgn_id == r2)) %.%  # flag the 'parent' rgn_id with 1, others with 0 
  filter(rgn_id < 255, rgn_id != 213); head(sovregions)

# join fld_wt weighting to m_d
m_d = m_d %.% 
  left_join(sovregions %.%
              select(rgn_id, fld_wt),
            by = 'rgn_id'); head(m_d)

# gapfill_georegions
attrsave  = file.path(dir_d, 'data', 'rgn_wb_wgi_2014a_attr.csv')

# library(devtools); load_all('../ohicore')
# source('../ohicore/R/gapfill_georegions.R')
d_g_a = gapfill_georegions(
  data = m_d %.%
    filter(!rgn_id %in% c(213,255)) %.%
    select(rgn_id, year, score, fld_wt),
  fld_id = c('rgn_id'),
  fld_weight = 'fld_wt',
  georegions = sovregions %.%
    select(-fld_wt),
  r0_to_NA = TRUE, 
  attributes_csv = (attrsave)) 

# investigate attribute tables
head(attr(d_g_a, 'gapfill_georegions'))  # or to open in excel: system(sprintf('open %s', attrsave))

## save for scenarios separately----
d_g = d_g_a %.%
  select(rgn_id, year, score) %.%
  arrange(rgn_id, year); head(d_g) # d_g[duplicated(d_g[, c('rgn_id', 'year')]),] 

# for each scenario separately
scenarios = list('2012a'=2010,
                 '2013a'=2011,
                 '2014a'=2012)

for (scen in names(scenarios)){ # scen = names(scenarios)[1]
  
  yr = scenarios[[scen]]
  cat(sprintf('\nScenario %s using year == %d\n', scen, yr))
  
  ## save gapfilled layer
  d_g_yr = d_g %>%
    filter(year == yr) %>% # only keep scenario year
    select(rgn_id, score); summary(d_g_yr)
  
  layersave = file.path(dir_d, 'data', sprintf('rgn_wb_wgi_%s.csv', scen))
  write.csv(d_g_yr, layersave, na = '', row.names=FALSE)
  
  
  # calculate inverse file and save ----
  d_g_inverse = d_g_yr %.%
    mutate(score_inverse = (1-score)) %.%
    select(rgn_id,
           score = score_inverse); summary(d_g_inverse)
  
  layersave2 = file.path(paste(file_path_sans_ext(layersave), '_inverse.csv', sep=''))
  write.csv(d_g_inverse, layersave2, na = '', row.names=FALSE)
  
}





## --- fin

