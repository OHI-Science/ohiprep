# track georegional gapfilling of a CBD resilience layers:
# 'r_alien_species_Nature2012disaggregated.csv'
# 'r_tourism_2013a.csv'
# 'r_water_2013a.csv'

# files will be in GL-NCEAS-Resilience_v2013a/whence_2013a; see also model/GL-CBD-Survey


## setup ---
# load libraries
source('src/R/common.R') # set dir_neptune_data; load reshape2, plyr, dplyr
# devtools::install_github('ohi-science/ohicore') # may require uninstall and reinstall
library(stringr)
library(ggplot2)
library(ohicore)  # for github/ohicore/R/gapfill_georegions.R
fdir = 'model/GL-NCEAS-Resilience_v2013a'

# to translate from cntry2012 to rgn_2013 
l = read.csv(file.path(dir_neptune_data, 'model/GL-NCEAS-OceanRegions_v2013a/manual_output/rgn2013_to_country2012.csv'), na.strings=''); head(l)

# to track gapfilling for whence 2013a purposes only, not for values (which were calculated for 2012n and disaggregated to 2012a region)
georegions = read.csv('../ohicore/inst/extdata/layers.Global2013.www2013/rgn_georegions_long_2013b.csv', na.strings='') %.%
  dcast(rgn_id ~ level, value.var='georgn_id'); head(georegions)


# read in file ----
f = read.csv(file.path(dir_neptune_data, 'model/GL-CBD-Survey/tmp/cbd_data.csv')); head(f)
layer = c('water', 'tourism', 'aliens')

for (i in 1:length(layer)) { # i=1
  
  if        (layer[i] == 'water') {        # listed in README file: /Volumes/data_edit/model/GL-CBD-Survey/README.txt
    questions = c('153d', '153f') 
    
  } else if (layer[i] == 'tourism') {
    questions = c('79x', '80x', '82x')
    
  } else if (layer[i] == 'aliens') {
    questions = c('160b', '160c', '160d', '160e') 
  }
  
  
  w = f %.%  # 
    filter(QUESTION %in% questions) %.%
    select(country_id = ISO3166, answer = ANSWER) %.%
    left_join(l %.%
                select(rgn_id, country_id), 
              by = 'country_id'); head(w)
  w$rgn_id[w$country_id == 'BRN'] = 247 # Brunei    # fix a few by hand
  w$rgn_id[w$country_id == 'MNE'] = 186 # Montenegro
 
  w2 = w[duplicated(w[,'rgn_id']),] %.%         # just need to track presence/absence here
    filter(!is.na(rgn_id)) %.% # remove others: land-locked
    mutate(whencev01 = 'OD') %.%
    select(rgn_id, whencev01)
  
  w_gf = georegions %.% # absent from t2: identify as gapfilled
    anti_join(w2, by= 'rgn_id') %.%
    select(rgn_id) %.%
    mutate(whencev01 = 'SG'); head(w_gf)
  
  wfin = rbind(w2, w_gf) %.% 
    arrange(rgn_id); head(wfin)
  
  layersave = file.path(dir_neptune_data, fdir, 'whence_2013a', paste('CBD', layer[i], '_whence.csv', sep=''))
  write.csv(wfin, layersave, na = '', row.names=FALSE)
  
}




# --- fin