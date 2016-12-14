# Cleaning layer files for detailed TR function by removing duplicates and assigning common column names (ie rgn_id_2013 to rgn_id).
# Initially got these warnings from CheckLayers()
#     Warning messages:
#       Rows duplicated...
#         tr_jobs_tourism: 100
#         tr_jobs_total: 66
#         tr_unemployment: 56

source('src/R/common.R') # set dir_neptune_data depending on platform
library(dplyr)

# E = Ed / (L- (L*U))
# Xtr = E * (V/P) * S 
#
# V = Coastal population;   /Volumes/data_edit/model/GL-NCEAS-CoastalPopulation_v2013/data/rgn_popsum2013_inland25mi_complete.csv
#  using: mar_coastalpopn_inland25mi
p_in  = file.path(dir_neptune_data, 'model/GL-NCEAS-CoastalPopulation_v2013/data/rgn_popsum2013_inland25mi_complete.csv')
p_out = file.path(dirname(p_in), 'rgn_popsum2013_inland25mi_complete_lyr.csv')
read.csv(p_in, na.strings='') %.%
  select(rgn_id, popn=rgn_popsum2013_inland25mi) %.%
  arrange(rgn_id) %.%
  write.csv(p_out, na='', row.names=F)
head(read.csv(p_out, na.strings=''))

# P = Total population;     /Volumes/data_edit/model/GL-WorldBank-Statistics_v2012/data/rgn_wb_pop.csv
#  using: rny_le_popn (note same file for 2012 and 2013)

# S = Sustainability index
# needs rgn_id, not rgn_id_2013. since all values only for year 2013, remove year which suggest like all other TR values it varies by year.
p_in  = file.path(dir_neptune_data, 'model/GL-WEF-Economics_v2013/data/rgn_wef_ttci_2013a.csv')
p_out = file.path(dirname(p_in), 'rgn_wef_ttci_2013a_lyr.csv')
read.csv(p_in, na.strings='') %.%
  select(rgn_id=rgn_id_2013, score) %.%
  arrange(rgn_id) %.%
  write.csv(p_out, na='', row.names=F)  
head(read.csv(p_out, na.strings=''))

# Ed = Direct employment in tourism: ** this has not been gapfilled. We thought it would make more sense to do at the status level.
# /Volumes/data_edit/model/GL-WTTC-Tourism_v2011/data/rgn_wttc_empd.csv
# using: rgn_wttc_empd_2013a.csv
# averaging duplicates
p_in  = file.path(dir_neptune_data, 'model/GL-WTTC-Tourism_v2011/data/rgn_wttc_empd_2013a.csv')
p_out = file.path(dirname(p_in), 'rgn_wttc_empd_2013a_avgduplicates.csv')
d = read.csv(p_in, na.strings='') %.%
  select(rgn_id, year, val=count) %.%
  group_by(rgn_id, year) %.%
  summarize(
    val_n   = n(),
    val_min = min(val),
    val_max = max(val),
    val_dif = max(val) - min(val),
    val_avg = mean(val, na.rm=T)) %.%
  arrange(desc(val_n), rgn_id, year, val_avg)
d %.%
  filter(val_n > 1) %.%
  group_by(rgn_id) %.%
  summarize(
    duplicates = sum(val_n),
    years  = length(year),
    year_min = min(year),
    year_max = max(year))
#   rgn_id duplicates years year_min year_max
# 1    116         50    25     1988     2012
# 2    140         50    25     1988     2012
# 3    209         75    25     1988     2012
d %.%
  select(rgn_id, year, count=val_avg) %.%
  as.data.frame() %.%
  write.csv(p_out, na='', row.names=F)  

# L = Total labor force:    /Volumes/data_edit/model/GL-WorldBank-Statistics_v2012/data/rgn_wb_lab.csv
# using: rgn_wb_lab_2013a.csv. 2013: max(year)=2011; 2012: max(year)=2010
# averaging duplicates
p_in  = file.path(dir_neptune_data, 'model/GL-WorldBank-Statistics_v2012/data/rgn_wb_lab_2013a.csv')
p_out = file.path(dirname(p_in), 'rgn_wb_lab_2013a_avgduplicates.csv')
d = read.csv(p_in, na.strings='') %.%
  select(rgn_id, year, val=count) %.%
  group_by(rgn_id, year) %.%
  summarize(
    val_n   = n(),
    val_min = min(val),
    val_max = max(val),
    val_dif = max(val) - min(val),
    val_avg = mean(val, na.rm=T)) %.%
  arrange(desc(val_n), rgn_id, year, val_avg)
d %.%
  filter(val_n > 1) %.%
  group_by(rgn_id) %.%
  summarize(
    duplicates = sum(val_n),
    years  = length(year),
    year_min = min(year),
    year_max = max(year))
#   rgn_id duplicates years year_min year_max
# 1    116         44    22     1990     2011
# 2    209         66    22     1990     2011
d %.%
  select(rgn_id, year, count=val_avg) %.%
  as.data.frame() %.%
  write.csv(p_out, na='', row.names=F)  

# U = Unemployment: /Volumes/data_edit/model/GL-WorldBank-Statistics_v2012/data/rgn_wb_uem.csv
# using: rgn_wb_uem_2013a.csv. 2013: max(year)=2011; 2012: max(year)=2010
# averaging duplicates
p_in  = file.path(dir_neptune_data, 'model/GL-WorldBank-Statistics_v2012/data/rgn_wb_uem_2013a.csv')
p_out = file.path(dirname(p_in), 'rgn_wb_uem_2013a_avgduplicates.csv')
d = read.csv(p_in, na.strings='') %.%
  select(rgn_id, year, val=percent) %.%
  group_by(rgn_id, year) %.%
  summarize(
    val_n   = n(),
    val_min = min(val),
    val_max = max(val),
    val_dif = max(val) - min(val),
    val_avg = mean(val, na.rm=T)) %.%
  arrange(desc(val_n), rgn_id, year, val_avg)
d %.%
  filter(val_n > 1) %.%
  group_by(rgn_id) %.%
  summarize(
    duplicates = sum(val_n),
    years  = length(year),
    year_min = min(year),
    year_max = max(year))
#   rgn_id duplicates years year_min year_max
# 1     13          6     3     1984     2000
# 2    209         85    32     1980     2011
d %.%
  select(rgn_id, year, percent=val_avg) %.%
  as.data.frame() %.%
  write.csv(p_out, na='', row.names=F)    