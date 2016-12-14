library(dplyr)

source('src/R/common.R')

m = read.csv('G:/ohiprep/Global/NCEAS-Regions_v2014/manual_output/sp_rgn_manual.csv', na.strings='')
z = read.csv('C:/tmp/Global/NCEAS-Regions_v2014/eez.csv', na.strings='')

head(m)
head(z)

names(m)
names(z)

m %.%
  filter(sp_type=='land') %.%
  select(sp_type, sp_name, sp_key, sp_id) %.%
  merge(
    z %.%
      select(Country, ID, EEZ_ID),
    by.x='sp_name', by.y='Country', all=T) %.%
  filter(sp_id!=EEZ_ID)
#    sp_name sp_type sp_key sp_id  ID EEZ_ID
# 1 Colombia    land    COL   132 132    259
# 2 Colombia    land    COL   132 132    258
# 3 Svalbard    land    NOR   223 223    253

z %.%
  filter(ISO_3digit=='COL') %.%
  select(ID, EEZ, Country, Sov_ID, EEZ_ID)
#    ID                                             EEZ  Country Sov_ID EEZ_ID
# 1 132 Colombian Exclusive Economic Zone (QuitasueÃ±o) Colombia    132    258
# 2 132     Colombian Exclusive Economic Zone (Serrara) Colombia    132    259
# 3 132               Colombian Exclusive Economic Zone Colombia    132    132
  