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
  mutate(percent = percent/100); summary(d.1); head(d.1)


# add rgn_id: country to rgn_id ----
r = name_to_rgn(d.1, fld_name='country', flds_unique=c('country','year'), fld_value='percent', add_rgn_name=T); head(r) 


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
    select(rgn_id, year, percent),
  fld_id = 'rgn_id',
  georegions = georegions,
  georegion_labels = georegion_labels,
  r0_to_NA = TRUE, 
  attributes_csv = attrsave) # don't chain 

# investigate attribute tables
head(attr(r_g_a, 'gapfill_georegions'))  # or to open in excel: system(sprintf('open %s', attrsave))

## save ----
r_g = r_g_a %.%
  select(rgn_id, year, percent) %.%
  arrange(rgn_id, year); head(r_g)

write.csv(r_g, layersave, na = '', row.names=FALSE)

## test with Quandl ----
# for now, Quandl (2010) doesn't have the up-to-date information that the JMP website does (2012).
library(devtools)
install_github('R-package','quandl')
library(Quandl)
Quandl.auth("JULES32")

qs = Quandl.search(query = "WHO / UNICEF Joint Monitoring Program for Water Supply and Sanitation", silent = FALSE)

# in the future, pull the names of each country (x below) from the qs list, and loop through to pull them all and make a data.frame. 
x = 'WHO_UNICEF/WATERSAN_ABS_DENMARK'
JMP_san = Quandl(x); summary(JMP_san)

# --- fin ---


