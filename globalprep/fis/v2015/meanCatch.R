##############################################
## preparing mean catch data for the toolbox
## MRF June 18 2015
###############################################

source('../ohiprep/src/R/common.R') # set dir_neptune_data

#--------------------------
# getting data
# --------------------------

## get catch data and convert region 910 (South Onarky Islands) to 0 (no corresponding ohi region)
data <- read.csv(file.path(dir_neptune_data, 'git-annex/globalprep/SAUP_FIS_data/v2015/tmp/Catch_v16072015_summary.csv')) 
data <- data %>%
  mutate(EEZID = ifelse(EEZID==910, 0, EEZID)) %>%
  group_by(EEZID, FAOAreaID, TaxonKey, Year) %>%
  summarize(catch = sum(catch)) %>%
  ungroup()

## SAUP to OHI region data
region <- read.csv("src/LookupTables/new_saup_to_ohi_rgn.csv")
dups <- region[duplicated(region$saup_id), ]
region[region$saup_id %in% dups$saup_id, ] #duplicates, 
###### dups occur because some SAUP regions have lower resolution than OHI regions.
###### This causes the sample size of the following merge to increase, but this is ok.  
###### They end up getting the same score.  

species <- read.csv(file.path(dir_neptune_data, 
                              'git-annex/globalprep/SAUP_FIS_data/v2015/raw/ohi_taxon.csv'))

#######################################################################
### filter data to relevant years for calculating mean catch
#######################################################################
catch <- data %>%
  filter(Year >= 1980) %>%
  mutate(saup_fao_taxon = paste(EEZID, FAOAreaID, TaxonKey, sep="_")) %>%
  ungroup()

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
catch_zero_gapfill <- gather(catch_wide, "Year", "Catch", 5:dim(catch_wide)[2])
catch_zero_gapfill <- catch_zero_gapfill %>%
  left_join(first_report) %>%
  mutate(Year = as.numeric(as.character(Year))) %>%
  mutate(include = ifelse(Year >= first_catch_yr, "include", "cut")) %>%
  filter(include=="include")

## calculate mean catch at saup level for calculating RAMdataprep
## These data used to weight the RAM b/bmsy values when multiple saup regions within an OHI region

catch_saup_fao_mean <- catch_zero_gapfill %>%
  dplyr::select(EEZID, FAOAreaID, TaxonKey, Year, Catch) %>%
  group_by(EEZID, FAOAreaID, TaxonKey) %>%
  mutate(mean_catch=mean(Catch, na.rm=TRUE)) %>%
  select(EEZID, FAOAreaID, TaxonKey, Year, mean_catch) %>%
  ungroup(); head(catch_saup_fao_mean)

# some checks to make sure things look right:
data.frame(filter(catch_saup_fao_mean, EEZID==8 & FAOAreaID==37 & TaxonKey==100039)) #should all be the same
data.frame(filter(catch_zero_gapfill, EEZID==8 & FAOAreaID==37 & TaxonKey==100039))

data.frame(filter(catch_saup_fao_mean, EEZID==50 & FAOAreaID==57 & TaxonKey==600142)) #should all be the same
data.frame(filter(catch_zero_gapfill, EEZID==50 & FAOAreaID==57 & TaxonKey==600142))

data.frame(filter(catch_saup_fao_mean, EEZID==36 & FAOAreaID==57 & TaxonKey==600142)) #should all be the same
data.frame(filter(catch_zero_gapfill, EEZID==36 & FAOAreaID==57 & TaxonKey==600142))

write.csv(catch_saup_fao_mean, 'globalprep/SAUP_FIS/v2015/tmp/mean_catch_saup_fao.csv', row.names=FALSE)

##############################################################
## converting SAUP regions to OHI regions:
catch_ohi_fao <- catch_zero_gapfill %>%
  left_join(region, by=c("EEZID"="saup_id")) %>%   #N increases here due to SAUP regions that correspond to multiple OHI regions
  mutate(ohi_id_2013 = ifelse(is.na(ohi_id_2013), 0, ohi_id_2013)) %>%  # All NA values are EEZID=0
  dplyr::select(region_id=ohi_id_2013, FAO_id=FAOAreaID, TaxonKey, Year, Catch) %>%
  group_by(region_id, FAO_id, TaxonKey, Year) %>%
  summarize(catch=sum(Catch)) %>%  # N decreases due to OHI regions that are comprised of multiple SAUP regions
  arrange(region_id, FAO_id, TaxonKey, Year) %>%
  ungroup()

## check: these three regions should have the same data:
data.frame(filter(catch_ohi_fao, region_id %in% c(33, 34, 35) & TaxonKey ==100038))

##########################################################################  
### Merge with Species name and formatting for toolbox

setdiff(catch_zero_gapfill$TaxonKey, species$taxonkey)  # some unaccounted for Taxon -but not big players...will ignore
# [1] 600478 612081 600472 607082 607108
# data[data$TaxonKey == 612081, ]
# catch[catch$TaxonKey %in% c(600478, 612081, 600472, 607082, 607108), ]
setdiff(species$taxonkey, catch_zero_gapfill$TaxonKey)  # a couple taxa not in the catch data
sum(duplicated(species$taxonkey))
sum(duplicated(species$scientific.name))
species[duplicated(species$scientific.name), ]  ## use names only for descriptive purposes...or edit species data to make names unique

catch_ohi_fao_taxa <- catch_ohi_fao %>%
  left_join(species, by=c('TaxonKey'='taxonkey')) 

#################################
###### EEZ data ###############
################################

eez <-  catch_ohi_fao_taxa %>%
  filter(region_id != 0) %>%
  mutate(fao_ohi_id = paste(FAO_id, region_id, sep="_")) %>%
  mutate(taxon_name_key = paste(scientific.name, TaxonKey, sep="_")) %>%
  select(fao_ohi_id, taxon_name_key, year=Year, catch)
### save these data for calculating FP weightings
write.csv(eez, "globalprep/SAUP_FIS/v2015/data/FP_fis_data.csv", row.names=FALSE)


### Average catch across years
eez_meanCatch <- eez %>%
  group_by(fao_ohi_id, taxon_name_key) %>%
  mutate(mean_catch = mean(catch, na.rm=TRUE)) %>%
  filter(mean_catch > 0) %>%
  select(fao_ohi_id, taxon_name_key, year, mean_catch) %>%
  ungroup()

# check 
data.frame(filter(eez_meanCatch, fao_ohi_id=="57_1" & taxon_name_key=='Holothuroidea_290012'))
data.frame(filter(catch_ohi_fao_taxa, FAO_id=="57" & TaxonKey=="290012" & region_id==1))


# filter to analysis years and save final data
eez_meanCatch <- eez_meanCatch %>%
  filter(year>=2005)

write.csv(eez_meanCatch, 
          'globalprep/SAUP_FIS/v2015/data/mean_catch.csv',
          row.names=FALSE)


#################################
###### HS data ###############
################################

hs <-  catch_ohi_fao_taxa %>%
  filter(region_id == 0) %>%
  mutate(fao_ohi_id = paste(FAO_id, region_id, sep="_")) %>%
  mutate(taxon_name_key = paste(scientific.name, TaxonKey, sep="_")) %>%
  select(fao_ohi_id, taxon_name_key, year=Year, catch)

### Average catch across years
hs_meanCatch <- hs %>%
  group_by(fao_ohi_id, taxon_name_key) %>%
  mutate(mean_catch = mean(catch, na.rm=TRUE)) %>%
  filter(mean_catch > 0) %>%
  select(fao_ohi_id, taxon_name_key, year, mean_catch) %>%
  ungroup()

# filter to analysis years and save final data
hs_meanCatch <- hs_meanCatch %>%
  filter(year>=2005)

write.csv(hs_meanCatch, 
          'globalprep/SAUP_FIS/v2015/data/mean_high_seas_catch.csv',
          row.names=FALSE)
