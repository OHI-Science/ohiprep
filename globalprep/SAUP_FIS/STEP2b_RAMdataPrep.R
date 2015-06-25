#######################################################
## Preparing the RAM data for B/Bmsy values
#######################################################

ram <- read.csv('globalprep/SAUP_FIS/raw/RLSADBv3_timeseries_values_views.csv') %>%
  select(stocklong, year, bbmsy=B.Bmsytouse) %>%
  filter(!is.na(bbmsy)) %>%
  filter(year >= 2005)

unique(ram$stocklong)
tmp <- ram %>%
  select(stocklong, year) %>%
  unique()

ram_summary <- ram %>%
  group_by(stocklong) %>%
  summarize(minYear = min(year),
            maxYear = max(year))
hist(ram_summary$maxYear, main="last year b/bmsy data", ylab="stock", xlab="Year")


##### RAM conversion
convert <- read.csv('globalprep/SAUP_FIS/raw/MatchedPairs.csv')
setdiff(convert$stocklong, ram$stocklong) ## All stocks in the covert file match RAM stocks (good!)
intersect(convert$stocklong, ram$stocklong)

setdiff(ram$stocklong, convert$stocklong)

ram_saup <- ram %>%
  left_join(convert, by='stocklong') %>%
  filter(!is.na(EEZID))


###### Merge with OHI regions
region <- read.csv(file.path(dir_neptune_data, 'git-annex/globalprep/SAUP_data_2015/tmp/new_saup_to_rgn_v2.csv'))

ram_saup_ohi <- ram_saup %>%
  left_join(region, by=c('EEZID' = 'saup_id'))

# Also need to identify the FAO region - need to use catch data to do this:
catch <- read.csv(file.path(dir_neptune_data, 'git-annex/globalprep/SAUP_data_2015/tmp/Catch_Value_11062015_summary.csv')) 

saup_fao <- catch %>%
  select(saup_id = EEZID, fao_id = FAOAreaID) %>%
  unique() %>%
  filter(saup_id > 0)

saup_fao[duplicated(saup_fao$saup_id), ]
