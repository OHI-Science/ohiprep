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
library(stringr)
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
# d.1[duplicated(d.1[,c('country', 'year')]),]

# add rgn_id: country to rgn_id ----
r = name_to_rgn(d.1, fld_name='country', flds_unique=c('country','year'), fld_value='prop_access', collapse_fxn = mean, add_rgn_name=T); head(r); summary(r) 
    # must use mean, otherwise prop_access > 1


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


layersave = file.path(dir_d, 'data', 'rgn_jmp_san_2014a_raw_prop_access.csv')
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


## save raw proportion with access ----
r_g = r_g_a %.%
  select(rgn_id, year, prop_access) %.%
  arrange(rgn_id, year); head(r_g)
write.csv(r_g, layersave, na = '', row.names=FALSE)
# r_g = read.csv('ohiprep/Global/WHOUNICEF-Sanitation_v2012/data/rgn_jmp_san_2014a_raw_prop_access.csv')

## model the trend and rescale, different from 2013a style ----
# based from neptune_data:model/GL-NCEAS-Pressures_v2013a/model_pathogens.R

popn_density_file = read.csv(file.path(dir_neptune_data, 'model/GL-NCEAS-CoastalPopulation_v2013/data', 
                                       'rgn_popsum_area_density_2005to2015_inland25mi.csv')); head(popn_density_file) 

# explore skew of population density ----
popn_density = popn_density_file %>%  
  filter(pop_per_km2 != 0) %>% # remove any unpopulated regions (rgn_id 149, 158)
  mutate(
    pop_per_km2_lin     = pop_per_km2 / max(pop_per_km2, na.rm=T),
    pop_per_km2_log     = ifelse(pop_per_km2!=0, log(pop_per_km2), 0),
    pop_per_km2_log_lin = ( pop_per_km2_log - min(pop_per_km2_log) ) / ( max(pop_per_km2_log, na.rm=T) - min(pop_per_km2_log) )) %>%
  select(rgn_id, year, pop_per_km2, pop_per_km2_lin, pop_per_km2_log_lin) %>%
  arrange(desc(pop_per_km2)); head(popn_density)

# to add any missing regions as NA
rgns = read.csv('src/LookupTables/eez_rgn_2013master.csv') %.%
  select(rgn_id = rgn_id_2013,
         rgn_name = rgn_nam_2013)  %.%
  filter(rgn_id < 255) %.%
  arrange(rgn_id); head(rgns)


## calculate by scenario ----

# identify  years for each scenario and overall. 'san' = 'sanitation'
maxyear_all_san = max(r_g$year, na.rm=T)
scenario_maxyear_san = c('eez2014' = maxyear_all_san,
                         'eez2013' = maxyear_all_san - 1,
                         'eez2012' = maxyear_all_san - 2)
minyear_all_san = scenario_maxyear_san[length(scenario_maxyear_san)]

for (i in 1:length(names(scenario_maxyear_san))) { # i=2
  
  maxyear_san = scenario_maxyear_san[i]
  minyear_san = maxyear_san - 4
  
  maxyear_pop = as.numeric(as.character(str_extract(names(scenario_maxyear_san)[i], "\\d{4}")))
  minyear_pop = maxyear_pop - 4
  
  san_pop = r_g %>%  # sanitation-population (san_pop)
    filter(year >= minyear_san & year <= maxyear_san, #  
           !is.na(prop_access)) %>%  
    mutate(yr_id = year - minyear_san+1,  # +1 just so it's not 0 as an identifier
           include_prev_scenario = year >= minyear_all_san) %>% 
    select(rgn_id, yr_id, prop_access, include_prev_scenario) %>%
    left_join(popn_density %>%
                filter(year >= minyear_pop & year <= maxyear_pop) %>%
                mutate(yr_id = year - minyear_pop+1) %>%
                select(rgn_id, yr_id, pop_per_km2), 
              by=c('rgn_id', 'yr_id')) %>%
    mutate(propWO_x_pop     = (1 - prop_access) * pop_per_km2, # this is the number of 'POOPERS'
           propWO_x_pop_log = log(propWO_x_pop+1))   # log is important because the skew was high otherwise
  head(san_pop); summary(san_pop) 

  
  ## save as pressure layer: only scenario's recent year, but rescaled, including previous scenarios ----

  # rescale with all previous scenarios, to 110%:  Neptune: model/GL-NCEAS-CleanWatersPressures/pathogens/sanitation-population-combo/model.R
  sp_press = san_pop %>%
    filter(include_prev_scenario == T) %>% 
    mutate(pressure_score = propWO_x_pop_log / (max(propWO_x_pop_log, na.rm=T) * 1.1)) %>% # number of POOPERS, RESCALED
    select(-include_prev_scenario)
  
#   # investigate Benin[99]
#   r %>% filter(year == 2012) %>% # eez2013
#     arrange(prop_access) %>% head(10) 
#   sp_press %>%
#     arrange(desc(propWO_x_pop)) %>% head(15)
    
  # combine with any missing regions as 0
  sp_pressure = rbind(sp_press %>%
                        filter(yr_id == max(yr_id, na.rm=T)) %>% # capture only the most recent year
                        select(rgn_id, pressure_score), 
                      rgns %>%
                        anti_join(san_pop, by = 'rgn_id') %>% 
                        mutate(pressure_score = 0) %>%
                        select(-rgn_name)) %>%
    arrange(rgn_id); head(sp_pressure); summary(sp_pressure)
  stopifnot(anyDuplicated(sp_pressure[,c('rgn_id')]) == 0)  

  csv_p = file.path(dir_d, 'data', sprintf('po_pathogens_popdensity25km_%sa.csv', 
                                           str_extract(names(scenario_maxyear_san)[i], '\\d{4}'))) # sprintf('po_pathogens_sanitation%d_popninland25km%d.csv', yr_max_san, yr_max_pop)) # previous, longer name
  write.csv(sp_pressure, csv_p, row.names=F, na='')
  
  
  ## model 5 year trend ----
  # trend needs to be calculated with status, which is NON-POOPERS, so must re-invert.
  # use a linear model since there is enough data. See below at the end for approach used in 2013a
  sp_mdl = san_pop %>%
    mutate(prop_x_pop = prop_access * pop_per_km2,
           prop_x_pop_log = log(prop_x_pop+1)) %>%  # log is important because the skew was high otherwise)
    select(rgn_id, yr_id, prop_x_pop_log) %>%
    filter(!is.na(prop_x_pop_log)) %>% # lm can't handle NAs!!
    group_by(rgn_id) %>%
    do(
      mdl = lm(prop_x_pop_log ~ yr_id, data=.)) %>% 
    summarize(
      rgn_id = rgn_id, 
      year_ix0  = coef(mdl)['(Intercept)'],
      year_coef = coef(mdl)['yr_id']) %.%
    mutate(
      trend_tmp = year_coef / (year_coef * maxyear_san + year_ix0) * 5, # save as separate steps to check it's working
      trend_min = pmin(trend_tmp, 1, na.rm = T),
      trend_max = pmax(trend_min, -1)) %>% 
    select(rgn_id, 
           trend = trend_max); head(sp_mdl); summary(sp_mdl)
  
  # save 5-year trend 
  # add any missing regions as 0
  sp_mdl_fin = rbind(sp_mdl, 
                     rgns %>%
                       anti_join(sp_mdl, by = 'rgn_id') %>%
                       mutate(trend = 0) %>%
                       select(-rgn_name)) %>%
    arrange(rgn_id); head(sp_mdl_fin); summary(sp_mdl_fin)
  stopifnot(anyDuplicated(sp_mdl_fin[,c('rgn_id')]) == 0)
  
  csv_t = file.path(dir_d, 'data', sprintf('pathogens_popdensity25km_trend_%sa.csv', 
                                           str_extract(names(scenario_maxyear_san)[i], '\\d{4}')))  # sprintf('po_pathogens_trend_sanitation%d_popninland25km%d.csv', yr_max_san, yr_max_pop)) # previous, longer name
  write.csv(sp_mdl_fin, csv_t, row.names=F, na='')
  
}


# pressures:

# 2014 pressures scores are rescaled to maximum 'poopers' since eez2012 (2010-2012):
# Benin in 2012
# 
# 2013 pressures scores are rescaled to maximum 'poopers' since eez2012 (2010-2011):
# Benin in 2011
# 
# 2012 pressures scores are rescaled to maximum 'poopers' since eez2012 (2010-2010):
# Benin in 2010




## test with Quandl ----
# for now, Quandl (2010) doesn't have the up-to-date information that the JMP website does (2012).
# library(devtools)
# install_github('R-package','quandl')
# library(Quandl)
# Quandl.auth("JULES32")
# 
# qs = Quandl.search(query = "WHO / UNICEF Joint Monitoring Program for Water Supply and Sanitation", silent = FALSE)
# 
# # in the future, pull the names of each country (x below) from the qs list, and loop through to pull them all and make a data.frame. 
# # to make the loop, either pull from qs above or download the 'complete list' of data that match this search: http://www.quandl.com/WHO_UNICEF?keyword=%20&page=4&code=WHO_UNICEF&source_ids=439
# x = 'WHO_UNICEF/WATERSAN_ABS_DENMARK'
# JMP_san = Quandl(x); summary(JMP_san)


## 2013a approach: model_pathogens.R ----

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


# calculate trend from two files ----
# d = merge(rename(read.csv('data/po_pathogens_sanitation2008_popninland25km2008.csv', na.strings=''),
#                  c('pressure_score' = 'pressure_score_2008')),
#           rename(read.csv('data/po_pathogens_sanitation2011_popninland25km2013.csv', na.strings=''),
#                  c('pressure_score' = 'pressure_score_2011'))); head(d)
# # get 5 year trend from this 3 year period
# d = within(d, {
#   trend = (pressure_score_2011 - pressure_score_2008) / 3 * 5}); head(d)
# write.csv(d[,c('rgn_id','trend')], 'data/po_pathogens_trend_sanitation2008to2011_popninland25km2008to2013.csv', row.names=F, na='')





# --- fin ---


