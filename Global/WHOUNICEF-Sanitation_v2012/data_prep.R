# data_prep.r. 
# reformat and add rgn_ids to WHO-UNICEF JMP (Joint Monitoring Programme) data
# previously clean_WHOUNICEF.r:
#   name_to_rgn
#   georegional gapfilling with gapfill_georegions.r


# Nature 2012 SOM p. 51: 'Access to improved sanitation facilities is defined as the percentage of the population within a country with at least adequate access to excreta disposal facilities that can effectively prevent human, animal, and insect contact with excreta. These data are a country-wide average (not specific to the coastal region) and the most recent year available is 2008. Percentages (0-100) for each country were rescaled to 0-1 based on a maximum target of 100% of the population with access to improved sanitation, and a minimum value of 0'

# compare http://www.wssinfo.org/data-estimates/tables/ (226 unique countries' data (coastal and non-coastal))
# with Quandl, which has 452 individual datasets for each country:
# http://www.quandl.com/WHO_UNICEF?keyword=%20&page=4&code=WHO_UNICEF&source_ids=439. But with Quandl, you have to read them in separately.

# load libraries
library(zoo) # for na.locf: Last Observation Carried Forward
library(ohicore) # devtools::install_github('ohi-science/ohicore') # may require uninstall and reinstall

# get paths
source('src/R/common.R') # set dir_neptune_data
source('src/R/ohi_clean_fxns.R') # has functions: cbind_rgn(), sum_na()
dir_d = 'Global/WHOUNICEF-Sanitation_v2012' 

## read in files, clean up data ----

d = read.csv(file.path(dir_d, 'raw', 'wssinfo_data.csv'), na.strings=c(NA,''), skip=3); head(d) # note: na.strings argument important for na.locf to work
d = d %>%
  select(country = Country,
         year    = Year,
         population = x1000,
         improved_tot_count = Total.Improved..x1000. ,
         improved_tot_perc  = Total.Improved.... ,
         unimproved_tot_count = Total.Unimproved..x1000. ,
         unimproved_tot_perc  = Total.Unimproved.... ,
         prop2010PopThatGainedAccessSince1995 = X) %>%
  mutate(population           = population           * 1000,
         improved_tot_count   = improved_tot_count   * 1000,
         unimproved_tot_count = unimproved_tot_count * 1000,
         country = na.locf(country),                               # use function from zoo package to fill down
         prop2010PopThatGainedAccessSince1995 = na.locf(prop2010PopThatGainedAccessSince1995)); head(d)

# select and scale percent of population with access to improved sanitation
d.1 = d %>%
  select(country, year,
         percent = improved_tot_perc) %>%
  mutate(prop_access = percent/100); summary(d.1); head(d.1)


# add rgn_id: country to rgn_id ----
r = name_to_rgn(d.1, fld_name='country', flds_unique=c('country','year'), fld_value='prop_access', add_rgn_name=T); head(r) 

# a check:
a = r %>%
  filter(year >=2004) %>%
  select(rgn_id, prop_access) %>%
  group_by(rgn_id) %>%
  mutate(count = n())
head(a); summary(a)

## georegional gapfilling with gapfill_georegions.r ----
georegions = read.csv('../ohi-global/eez2013/layers/rgn_georegions.csv', na.strings='') %.%
  dcast(rgn_id ~ level, value.var='georgn_id')

georegion_labels = read.csv('../ohi-global/eez2013/layers/rgn_georegion_labels.csv') %.%    
  mutate(level_label = sprintf('%s_label', level)) %.%
  dcast(rgn_id ~ level_label, value.var='label') %.%
  left_join(
    read.csv('../ohi-global/eez2013/layers/rgn_labels.csv') %.%
      select(rgn_id, v_label=label),
    by='rgn_id') %.%
  arrange(r0_label, r1_label, r2_label, v_label); head(georegion_labels)


layersave = file.path(dir_d, 'data', 'rgn_jmp_san_2014a.csv')
attrsave  = file.path(dir_d, 'data', 'rgn_jmp_san_2014a_attr.csv')

# library(devtools); load_all('../ohicore')
# source('../ohicore/R/gapfill_georegions.R')
r_g_a = gapfill_georegions(
  data = r %.%
    filter(!rgn_id %in% c(213,255)) %.%
    select(rgn_id, year, prop_access),
  fld_id = 'rgn_id',
  georegions = georegions,
  georegion_labels = georegion_labels,
  r0_to_NA = TRUE, 
  attributes_csv = attrsave) # don't chain 

# investigate attribute tables
head(attr(r_g_a, 'gapfill_georegions'))  # or to open in excel: system(sprintf('open %s', attrsave))

## save ----
r_g = r_g_a %.%
  select(rgn_id, year, prop_access) %.%
  arrange(rgn_id, year); head(r_g)

write.csv(r_g, layersave, na = '', row.names=FALSE)


## rescaling, different from 2013a style ----
# based from neptune_data:model/GL-NCEAS-Pressures_v2013a/model_pathogens.R

popn_density_file = read.csv(file.path(dir_neptune_data, 'model/GL-NCEAS-CoastalPopulation_v2013/data', 
                                       'rgn_popsum_area_density_2005to2015_inland25mi.csv')); head(popn_density_file) 

# explore skew of population density ----
popn_density = popn_density_file %>%  
  mutate(
    pop_per_km2_lin     = pop_per_km2 / max(pop_per_km2),
    pop_per_km2_log     = ifelse(pop_per_km2!=0, log(pop_per_km2), 0),
    pop_per_km2_log_lin = ( pop_per_km2_log - min(pop_per_km2_log) ) / ( max(pop_per_km2_log) - min(pop_per_km2_log) )) %>%
  select(rgn_id, year, pop_per_km2, pop_per_km2_lin, pop_per_km2_log_lin) %>%
  arrange(desc(pop_per_km2)); head(popn_density)


# for each scenario, divide by population in current year
scenario = c('2014' = 0,
             '2013' = 1,
             '2012' = 2)

# for each scenario
for (i in 1:length(names(scenario))) { # i=1
  
  yr_max_san = max(r_g$year) - as.numeric(as.character(factor(scenario[i])))
  yr_min_san = yr_max_san - 4   # inclusive: this is 5 years
  yr_max_pop = as.numeric(as.character(factor(names(scenario)[i])))
  yr_min_pop = yr_max_pop - 4
  
  
  ## calculate access status for 5 years and get trend ----
   # use a linear model since there is enough data
  san_pop_mdl = r_g %>%
    filter(year <= yr_max_san & year >= yr_min_san, #  
           !is.na(prop_access)) %>%  
    mutate(yr_id = year - yr_min_san+1) %>%
    select(rgn_id, yr_id, prop_access) %>%
    left_join(popn_density %>%
                filter(year <= yr_max_pop & year >= yr_min_pop) %>%
                mutate(yr_id = year - yr_min_pop+1) %>%
                select(rgn_id, yr_id, pop_per_km2), 
              by=c('rgn_id', 'yr_id')) %>%
    mutate(prop_x_pop = prop_access * pop_per_km2,
           prop_x_pop_log = log(prop_x_pop+1)) %>%
  group_by(rgn_id) %>%
    do(
      mdl = lm(prop_x_pop_log ~ yr_id+2000, data=.)) %>% # come back here: getting an error
    summarize(
      rgn_id = rgn_id, 
      year_ix0  = coef(mdl)['(Intercept)'],
      year_coef = coef(mdl)['year']) %.%
    mutate(
      trend_tmp = year_coef / (year_coef * yr_max_san + year_ix0) * 5, # save these as separate steps to check it's working
      trend_min = pmin(trend_tmp, 1, na.rm = T),
      trend_max = pmax(trend_min, -1)) %>% 
    select(rgn_id, 
           trend = trend_max); head(san_pop_mdl); summary(san_pop_mdl)
  
  ,                     # log is important because the skew was high otherwise
  prop_x_pop_rescaled = prop_x_pop_log / max(prop_x_pop_log, na.rm=T)); head(san_pop); summary(san_pop) 

  
     
  
  
  # save access trend 
  
  
  ## save pressure score per scenario ----
  # this is 1 - status from the most recent year 
  san_pop = r_g %>%
    filter(year == yr_max_san) %>%
    select(rgn_id, prop_access) %>%
    left_join(popn_density %>%
                filter(year == yr_max_pop) %>%
                select(rgn_id, pop_per_km2), 
              by=c('rgn_id')) %>%
    mutate(prop_x_pop = prop_access * pop_per_km2,
           prop_x_pop_log = log(prop_x_pop+1),                     # log is important because the skew was high otherwise
           prop_x_pop_rescaled = prop_x_pop_log / max(prop_x_pop_log, na.rm=T)); head(san_pop); summary(san_pop)  
  # similar approach to what was done in 2013a: in model_pathogens.R
  #    for (scen in names(scenarios)){ # scen = names(scenarios)[1]
  #     yr.san = scenarios[[scen]][['sanitation']]
  #     yr.pop = scenarios[[scen]][['population']]
  #     d = rename(subset(m, year==yr.san), setNames('popsum_inland25mi', sprintf('popsum%d_inland25mi', yr.pop))); head(d)
  #     d = within(d, {
  #       popn_density           = popsum_inland25mi / area_inland25mi_km2
  #       #popn_density_log       = ifelse(popn_density!=0, log(popn_density), 0)
  #       popn_density_log       = log(popn_density+1)
  #       popn_density_log_lin   = ( popn_density_log - min(popn_density_log) ) / ( max(popn_density_log) - min(popn_density_log) )
  #       pct_without_sanitation = (100 - pct_sanitation_access)
  #       score_raw              = pct_without_sanitation / 100 * popn_density_log_lin
  #       pressure_score         = score_raw/(max(score_raw)*1.10)
  #     })
  
  
  ## save as pressure layer ----
  sp = san_pop %>%
    select(rgn_id, 
           pressure_score = prop_x_pop_rescaled)
  stopifnot(anyDuplicated(sp[,c('rgn_id')]) == 0)
  
  
  csv = file.path(dir_d, 'data', sprintf('po_pathogens_popdensity25km_%sa.csv', names(scenario)[i])) 
                                #sprintf('po_pathogens_sanitation%d_popninland25km%d.csv', yr_max_san, yr_max_pop))
  write.csv(sp, csv, row.names=F, na='')
  
  
  
   ## calculate trend per scenario this is 1-pressure, and calculate the pressure ----
 
  
  
    select(rgn_id, prop_access) %>%
    left_join(popn_density %>%
                filter(year == yr_max_pop) %>%
                select(rgn_id, pop_per_km2), 
              by=c('rgn_id')) %>%
    mutate(prop_x_pop = prop_access * pop_per_km2,
           prop_x_pop_log = log(prop_x_pop+1),                     # log is important because the skew was high otherwise
           prop_x_pop_rescaled = prop_x_pop_log / max(prop_x_pop_log, na.rm=T)); head(san_pop); summary(san_pop)  
  
  
  
   d_mdl = dn2 %>%
      filter(!is.na(prop_access)) %>%
      group_by(rgn_id) %>%
      do(
        mdl = lm(tonnes ~ year, data=.)) %>%
      summarize(
        rgn_id = rgn_id, 
        year_ix0  = coef(mdl)['(Intercept)'],
        year_coef = coef(mdl)['year']) %.%
      mutate(
        trend_tmp = year_coef / (year_coef * yr_min + year_ix0) * 5, # save these as separate steps to check it's working
        trend_min = pmin(trend_tmp, 1, na.rm = T),
        trend_max = pmax(trend_min, -1)) %>% 
      select(rgn_id, 
             trend.score = trend_max); head(d_mdl)
  
  
  
}

# these are the southern islands which can't be gap-filled, so applying 100





}



# calculate trend from two files ----
d = merge(rename(read.csv('data/po_pathogens_sanitation2008_popninland25km2008.csv', na.strings=''),
                 c('pressure_score' = 'pressure_score_2008')),
          rename(read.csv('data/po_pathogens_sanitation2011_popninland25km2013.csv', na.strings=''),
                 c('pressure_score' = 'pressure_score_2011'))); head(d)
# get 5 year trend from this 3 year period
d = within(d, {
  trend = (pressure_score_2011 - pressure_score_2008) / 3 * 5}); head(d)
write.csv(d[,c('rgn_id','trend')], 'data/po_pathogens_trend_sanitation2008to2011_popninland25km2008to2013.csv', row.names=F, na='')



## test with Quandl ----
# for now, Quandl (2010) doesn't have the up-to-date information that the JMP website does (2012).
library(devtools)
install_github('R-package','quandl')
library(Quandl)
Quandl.auth("JULES32")

qs = Quandl.search(query = "WHO / UNICEF Joint Monitoring Program for Water Supply and Sanitation", silent = FALSE)

# in the future, pull the names of each country (x below) from the qs list, and loop through to pull them all and make a data.frame. 
# to make the loop, either pull from qs above or download the 'complete list' of data that match this search: http://www.quandl.com/WHO_UNICEF?keyword=%20&page=4&code=WHO_UNICEF&source_ids=439
x = 'WHO_UNICEF/WATERSAN_ABS_DENMARK'
JMP_san = Quandl(x); summary(JMP_san)

# --- fin ---


