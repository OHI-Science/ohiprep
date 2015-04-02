# data_prep.R. 
# Prep World Bank WGI (World Governance Indicators) data using World Bank Development Indicators API
# J. Lowndes: lowndes@nceas.ucsb.edu, April 2015
# See also ohiprep/Global/WorldBank-WGI_v2013 for previous analyses without the API. 

# Steps involve:
# Access WGI data from 
# Add rgn_ids  
# gapfilling: sovereignty (parent-children) gapfilling with gapfill_georegions: flag sovereign


# setup ----
library(ohicore) #devtools::install_github('ohi-science/ohicore@dev')
library(WDI) # install.packages('WDI')


# check website to see what years are available: http://info.worldbank.org/governance/wgi/index.aspx#home
yr_start = 1996
yr_end   = 2013


## access data ----

# identify the six indicators
# WDIsearch('violence')# general search
key_voice = WDI(
  WDIsearch('Voice and Accountability: Estimate', field='name')['indicator'],
  country='all',start = yr_start, end=yr_end)

key_polst = WDI(
  WDIsearch('Political Stability and Absence of Violence/Terrorism: Estimate', field='name')['indicator'],
  country='all',start = yr_start, end=yr_end)

key_gvtef = WDI(
  WDIsearch('Government Effectiveness: Estimate', field='name')['indicator'],
  country='all',start = yr_start, end=yr_end)

key_regqt = WDI(
  WDIsearch('Regulatory Quality: Estimate', field='name')['indicator'],
  country='all',start = yr_start, end=yr_end)

key_rolaw = WDI(
  WDIsearch('Rule of Law: Estimate', field='name')['indicator'],
  country='all',start = yr_start, end=yr_end)

key_corrp = WDI(
  WDIsearch('Control of Corruption: Estimate', field='name')['indicator'],
  country='all',start = yr_start, end=yr_end)


## combine all indicators ----
d = key_voice %>% 
  select(country, year, VA.EST) %>%
  left_join(key_polst %>% select(-iso2c), by=(c('country', 'year'))) %>%
  left_join(key_gvtef %>% select(-iso2c), by=(c('country', 'year'))) %>%
  left_join(key_regqt %>% select(-iso2c), by=(c('country', 'year'))) %>%
  left_join(key_rolaw %>% select(-iso2c), by=(c('country', 'year'))) %>%
  left_join(key_corrp %>% select(-iso2c), by=(c('country', 'year'))); head(d); summary(d); sapply(d, class)  

d_wgi = d %>%
  group_by(country, year) %>%
  mutate(score = sum(VA.EST, PV.EST, GE.EST, RQ.EST, RL.EST, CC.EST, na.rm=T)/6); head(d_wgi); summary(d_wgi)
# make sure this matches last year's calculations:
# summary(read.csv('~/github/ohiprep/Global/WorldBank-WGI_v2013/raw/GL-WorldBank-WGI_v2011-cleaned.csv'))

## JSL come back here Apr 1
rng = c(-2.5, 2.5)
d.m2 = within(d.m,{
  score = (score - rng[1]) / (rng[2] - rng[1])}); head(d.m2); summary(d.m2)

    
    

##  add rgn_id using name_to_rgn ----
m_d = name_to_rgn(d_wgi, fld_name='country', flds_unique=c('country','year'), 
                  fld_value='score', add_rgn_name=T, collapse_fxn = mean); head(m_d); summary(m_d) 



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

