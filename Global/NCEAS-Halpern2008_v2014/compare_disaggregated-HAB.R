source('src/R/common.R')
rgns = read.csv('Global/NCEAS-Regions_v2014/data/rgn_gcs_data.csv') %>%
  head()

d = read.csv('Global/NCEAS-Halpern2008_v2014/data/ecosystems_coral_reef.csv')