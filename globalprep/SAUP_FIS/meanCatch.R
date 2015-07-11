##############################################
## preparing mean catch data for the toolbox
## MRF June 18 2015
###############################################

library(dplyr)
source('../ohiprep/src/R/common.R') # set dir_neptune_data


## get catch data
data <- read.csv(file.path(dir_neptune_data, 'git-annex/globalprep/SAUP_data_2015/tmp/Catch_Value_11062015_summary.csv')) 
# eez_fao <- data %>%
#   filter(EEZID != 0) %>%
#   dplyr::select(EEZID, FAOAreaID, TaxonKey) %>%
#   unique()
# write.csv(eez_fao, "globalprep/SAUP_FIS/tmp/eez_fao_taxa_combos.csv", row.names=FALSE)


## SAUP to OHI region data
region <- read.csv("src/LookupTables/new_saup_to_ohi_rgn.csv")
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
  group_by(EEZID, FAOAreaID, TaxonKey, Year) %>%
  summarize(catch=sum(catch)) %>%
  ungroup() %>%
  mutate(saup_fao_taxon = paste(EEZID, FAOAreaID, TaxonKey, sep="_"))

##########################################################################  
### Filling in missing years with zero values after first catch
  ## Determine first year of catch data (no true zeros in data)
first_report <- catch %>%
  group_by(saup_fao_taxon) %>%
  summarize(first_catch_yr = min(Year)) %>%
  ungroup()
  
  ## Expand data and fill in all missing years with zero values
catch_wide <- spread(catch, Year, catch)
catch_wide[is.na(catch_wide)] <- 0
  
  ## Gather data and get rid of years prior to the first reported year of catch:
catch_zero_gapfill <- gather(catch_wide, "Year", "Catch", 5:35)
catch_zero_gapfill <- catch_zero_gapfill %>%
  left_join(first_report) %>%
  mutate(Year = as.numeric(as.character(Year))) %>%
  mutate(include = ifelse(Year >= first_catch_yr, "include", "cut")) %>%
  filter(include=="include")

## Save these data for weighting the b/bmsy values when multiple saup regions within an OHI region
write.csv(catch_zero_gapfill, 'globalprep/SAUP_FIS/tmp/saup_fao_catch.csv', row.names=FALSE)
## calculate mean catch at saup level for calculating RAMdataprep
catch_saup_fao_mean <- catch_zero_gapfill %>%
  dplyr::select(EEZID, FAOAreaID, TaxonKey, Year, Catch) %>%
  group_by(EEZID, FAOAreaID, TaxonKey) %>%
  mutate(mean_catch=mean(Catch, na.rm=TRUE)) %>%
  select(EEZID, FAOAreaID, TaxonKey, Year, mean_catch) %>%
  ungroup(); head(catch_saup_fao_mean)

data.frame(filter(catch_saup_fao_mean, EEZID==8 & FAOAreaID==37 & TaxonKey==100039)) #should all be the same
data.frame(filter(catch_saup_fao_mean, EEZID==50 & FAOAreaID==57 & TaxonKey==600142)) #should all be the same
data.frame(filter(catch_saup_fao_mean, EEZID==36 & FAOAreaID==57 & TaxonKey==600142)) #should all be the same


write.csv(catch_saup_fao_mean, 'globalprep/SAUP_FIS/tmp/mean_catch_saup_fao.csv', row.names=FALSE)
data.frame(filter(RAM_b_bmsy_ohi_rgn_catch, EEZID==50 & FAOAreaID==57 & Taxonid==600142))

##############################################################
## converting SAUP regions to OHI regions:
catch_ohi_fao <- catch_zero_gapfill %>%
  left_join(region, by=c("EEZID"="saup_id")) %>%
  dplyr::select(region_id=ohi_id_2013, FAO_id=FAOAreaID, TaxonKey, Year, Catch) %>%
  filter(!is.na(region_id)) %>%
  group_by(region_id, FAO_id, TaxonKey, Year) %>%
  summarize(catch=sum(Catch)) %>%
  arrange(region_id, FAO_id, TaxonKey, Year) %>%
  ungroup()

##########################################################################  
### Merge with Species name and formatting for toolbox

setdiff(catch_zero_gapfill$TaxonKey, species$taxonkey)  # looks like all the catch Taxa are in the database
setdiff(species$taxonkey, catch_zero_gapfill$TaxonKey)  # ask Katie about this, I think she updated....
sum(duplicated(species$scientific.name))

catch_ohi_fao_taxa <- catch_ohi_fao %>%
  left_join(species, by=c('TaxonKey'='taxonkey')) %>%
  mutate(fao_ohi_id = paste(FAO_id, region_id, sep="_")) %>%
  mutate(taxon_name_key = paste(scientific.name, TaxonKey, sep="_")) %>%
  select(fao_ohi_id, taxon_name_key, year=Year, catch)



##########################################################################
### Average catch across years

ohi_fao_taxa_meanCatch <- catch_ohi_fao_taxa %>%
  group_by(fao_ohi_id, taxon_name_key) %>%
  mutate(mean_catch = mean(catch)) %>%
  filter(mean_catch > 0) %>%
  select(fao_ohi_id, taxon_name_key, year, mean_catch) %>%
  ungroup()

# check 
data.frame(filter(ohi_fao_taxa_meanCatch, fao_ohi_id=="57_1" & taxon_name_key=='Holothuroidea_290012'))
data.frame(filter(catch_ohi_fao_taxa, fao_ohi_id=="57_1" & taxon_name_key=='Holothuroidea_290012'))

# save
write.csv(ohi_fao_taxa_meanCatch, 
          'globalprep/SAUP_FIS/data/mean_catch.csv',
          row.names=FALSE)
