##############################################
## preparing mean catch data for the toolbox
## MRF June 18 2015
###############################################

source('../ohiprep/src/R/common.R') # set dir_neptune_data

## get catch data
data <- read.csv(file.path(dir_neptune_data, 'git-annex/globalprep/SAUP_data_2015/tmp/Catch_Value_11062015_summary.csv')) 

## SAUP to OHI region data
region <- read.csv(file.path(dir_neptune_data, 'git-annex/globalprep/SAUP_data_2015/tmp/new_saup_to_rgn_v2.csv'))
dups <- region[duplicated(region$saup_id), ]
region[region$saup_id %in% dups$saup_id, ] #duplicates, 
###### cause the sample size of the following merge to increase, but this is ok.  
###### They end up getting the same score.  Some SAUP regions have lower resolution than OHI regions.

species <- read.csv(file.path(dir_neptune_data, 'git-annex/globalprep/SAUP_data_2015/raw/ohi_taxon.csv'))

#######################################################################
### filter data to relevant years, convert SAUP regions to OHI regions
#######################################################################
catch <- data %>%
  filter(Year >= 1980) %>%
  filter(EEZID != 0) %>%
  left_join(region, by=c("EEZID"="saup_id")) %>%
  select(region_id=ohi_id_2013, FAO_id=FAOAreaID, TaxonKey, Year, catch) %>%
  filter(!is.na(region_id)) %>%
  group_by(region_id, FAO_id, TaxonKey, Year) %>%
  summarize(catch=sum(catch)) %>%
  ungroup() %>%
  arrange(region_id, FAO_id, TaxonKey, Year) %>%
  mutate(region_fao_taxon = paste(region_id, FAO_id, TaxonKey, sep="_")) %>%  
  select(region_fao_taxon, region_id, FAO_id, TaxonKey, Year, catch)

##########################################################################  
### Filling in missing years with zero values after first catch
  ## Determine first year of catch data (no true zeros in data)
first_report <- catch %>%
  group_by(region_fao_taxon) %>%
  summarize(first_catch_yr = min(Year)) %>%
  ungroup()
  
  ## Expand data and fill in all missing years with zero values
catch_wide <- spread(catch, Year, catch) %>%
catch_wide[is.na(catch_wide)] <- 0
  
  ## Gather data and get rid of years prior to the first reported year of catch:
catch_zero_gapfill <- gather(catch_wide, "Year", "Catch", 5:35)
catch_zero_gapfill <- catch_zero_gapfill %>%
  left_join(first_report) %>%
  mutate(Year = as.numeric(as.character(Year))) %>%
  mutate(include = ifelse(Year >= first_catch_yr, "include", "cut")) %>%
  filter(include=="include")

##########################################################################  
### Merge with Species name and formatting for toolbox

setdiff(catch_zero_gapfill$TaxonKey, species$taxonkey)  # looks like all the catch Taxa are in the database
setdiff(species$taxonkey, catch_zero_gapfill$TaxonKey)  # ask Katie about this, I think she updated....
sum(duplicated(species$scientific.name))

catch_zero_gapfill_taxa <- catch_zero_gapfill %>%
  left_join(species, by=c('TaxonKey'='taxonkey')) %>%
  mutate(fao_ohi_id = paste(FAO_id, region_id, sep="_")) %>%
  mutate(taxon_name_key = paste(scientific.name, TaxonKey, sep="_")) %>%
  select(fao_ohi_id, taxon_name_key, year=Year, catch=Catch)

##########################################################################
### Average catch across years

catch_zero_gapfill_taxa_meanCatch <- catch_zero_gapfill_taxa %>%
  group_by(fao_ohi_id, taxon_name_key) %>%
  mutate(mean_catch = mean(catch)) %>%
  filter(mean_catch > 0) %>%
  select(fao_ohi_id, taxon_name_key, year, mean_catch)

# check 
data.frame(filter(catch_zero_gapfill_taxa_meanCatch, fao_ohi_id=="57_1" & taxon_name_key=='Holothuroidea_290012'))

write.csv(catch_zero_gapfill_taxa_meanCatch, 
          file.path(dir_neptune_data, 'git-annex/globalprep/SAUP_data_2015/data/mean_catch.csv'),
          row.names=FALSE)
