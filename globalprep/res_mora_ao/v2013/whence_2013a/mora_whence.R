# track georegional gapfilling of ao_need layer, which is Mora_S4. 

# see /Volumes/data_edit/model/GL-NCEAS-Resilience_Mora/README.txt
# and /Volumes/data_edit/model/GL-NCEAS-Resilience_Mora/model.sql

## setup ---
# load libraries
source('src/R/common.R') # set dir_neptune_data; load reshape2, plyr, dplyr
# devtools::install_github('ohi-science/ohicore') # may require uninstall and reinstall
library(stringr)
library(ggplot2)
library(ohicore)  # for github/ohicore/R/gapfill_georegions.R
m_path = file.path(dir_neptune_data, 'model/GL-NCEAS-Resilience_Mora')

# read in file
m = read.csv(file.path(dir_neptune_data, 'model/GL-NCEAS-Resilience_Mora/data/global_af_oaf_mora.csv'), na.strings=''); head(m)

## translate from cntry2012 to rgn_2013 ----
l = read.csv(file.path(dir_neptune_data, 'model/GL-NCEAS-OceanRegions_v2013a/manual_output/rgn2013_to_country2012.csv'), na.strings=''); head(l)

m2 = m %.%
  select(country_id = ISO3166,
         value = VALUE) %.%
  left_join(l %.%
              select(rgn_id, country_id), 
            by = 'country_id'); head(m2)
filter(m2, is.na(rgn_id)) # a check

# fix a few by hand
m2$rgn_id[m2$country_id == 'ABW'] = 250 # Aruba
m2$rgn_id[m2$country_id == 'BRN'] = 247 # Brunei
m2$rgn_id[m2$country_id == 'CUW'] = 244 # Curacao
m2$rgn_id[m2$country_id == 'HKG'] = 209 # Hong Kong --check for duplicates
m2$rgn_id[m2$country_id == 'MNE'] = 186 # Montenegro

# removed regions not used in 2013a
m3 = m2 %.%
  filter(!country_id %in% c('Bornholm', 'NVM', 'SRB', 'Tuvulau'))

# remove duplicates -- purpose here is to check which regions were missing
filter(m3, is.na(rgn_id)) # a check
i_dup = duplicated(m3$rgn_id)
m4 = m3[!i_dup,]


## identify where gapfilling occured ----
# for whence 2013a purposesonly, not for values (which were calculated for 2012n and disaggregated to 2012a region)

# gapfill_georegions--for whence only, not for values (which were calculated for 2012n and disaggregated to 2012a regions
georegions = read.csv('../ohicore/inst/extdata/layers.Global2013.www2013/rgn_georegions_long_2013b.csv', na.strings='') %.%
  dcast(rgn_id ~ level, value.var='georgn_id'); head(georegions)

m_gf = georegions %.% # absent from m
  anti_join(m4, by= 'rgn_id') %.%
  select(rgn_id) %.%
  mutate(whencev01 = 'SG'); head(m_gf)

m5 = m4 %.%
  select(rgn_id) %.%
  mutate(whencev01 = 'OD'); head(m5)

mfin = rbind(m5, m_gf) %.%
  arrange(rgn_id); head(mfin)

layersave = file.path(m_path, 'whence_2013a/mora_whence.csv')
write.csv(mfin, layersave, na = '', row.names=FALSE)


# --- fin


