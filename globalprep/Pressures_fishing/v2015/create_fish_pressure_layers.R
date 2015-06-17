# Creating fisheries pressure layers with new SAUP data

# June 2015

# Jamie Afflerbach

rm(list=ls())

library(raster)
library(dplyr)



dir_N = c('Windows' = '//neptune.nceas.ucsb.edu/data_edit',
          'Darwin'  = '/Volumes/data_edit',
          'Linux'   = '/var/data/ohi')[[ Sys.info()[['sysname']] ]]

setwd(file.path(dir_N,'git-annex/globalprep/Pressures_fishing'))

# File paths

saup_pressures = file.path(dir_N,'git-annex/Global/SAUP-FishCatchByGearType_Halpern2008/data')

# Data used for the commercial fishing pressures layers in OHI 2013. 
#These are derivatives and updates to the 2008 data 
ohi_2013  = file.path(dir_N,'model/GL-NCEAS-Pressures_CommercialFisheries_v2013a')

# new SAUP data
saup_2015 = file.path(dir_N,'git-annex/globalprep/SAUP_data_2015')

# Directory where the updates to the 2008 data were done for 2013 ohi
saup_update = file.path(dir_N, 'model/GL-SAUP-FisheriesCatchData_v2013')


#--------------------------------------------------------------------------------------

# New SAUP data

saup_all = read.csv(file.path(saup_2015,'tmp/Catch_Value_11062015_summary.csv'))

#-----------------------------------------------------------------------------

# new eezs

saup_2015_eez = read.csv('v2015/ohi_eezs_plus_old_saup_ids.csv')

#-----------------------------------------------------------------------------


# filter out taxonkey not used previously

saup_taxon_exclude = read.csv(file.path(saup_update,'tmp/global_srcdata_ss_saup_excluded_stock.csv'))

# look at what these taxons are

# are these taxon keys the same?

ohi_2015_taxons = read.csv(file.path(saup_2015,'raw/ohi_taxon.csv'))

filter(ohi_2015_taxons,taxonkey%in%saup_taxon_exclude$stock_id)

#   X taxonkey                     scientific.name           common.name
#1  1   100000                      Marine animals        Marine animals
#2  3   100025     Miscellaneous diadromous fishes     Diadromous fishes
#3  8   100039        Marine fishes not identified         Marine fishes
#4 12   100047    Miscellaneous marine crustaceans    Marine crustaceans
#5 14   100058       Miscellaneous marine molluscs       Marine molluscs
#6 15   100077 Miscellaneous aquatic invertebrates Aquatic invertebrates
#7 16   100139        Marine fishes not identified             Finfishes
#8 20   100239        Marine fishes not identified          Groundfishes
#9 23   100339        Marine fishes not identified        Pelagic fishes

# filter our taxons and aggregate catch per year/region - add in the old SAUP IDs
saup_data = saup_all%>% 
            filter(!TaxonKey %in% saup_taxon_exclude$stock_id)%>%
             mutate(id      = ifelse(EEZID==0,FAOAreaID+1000,EEZID),
                    id_type = ifelse(id>1000,'fao','eez'))%>% 
              group_by(Year,id_type,id)%>%
               summarize(catch=sum(catch))%>%
              mutate(old_saup_id = as.integer(ifelse(id_type=='eez',saup_2015_eez$old_saup_id[match(id,saup_2015_eez$EEZID)],id)))%>%
              select(new_id = id, Year, id_type,catch,old_saup_id)


# now we have the total catch per year per id.


#-----------------------------------------------------------------------------


# Calculate change in catch since the period 1999-2003

# Here I am going to use the data from the old SAUP dataset for the years 1999-2003 to calculate the change
# in catch. This should be more accurate than using the new dataset catches for 1999-2003 since they are likely
# much different.

# bring in old data
change_old = read.csv(file.path(saup_update,'data/pct_chg_saup_2009to2011_vs_1999to2003.csv'))

# The catch in yrs1999to2003 is what we want to compare to.

# Now aggregate data for periods 

saup_06_10 = saup_data%>%
            filter(Year>2005 & Year < 2011)%>%
            group_by(id_type,old_saup_id)%>%
             summarize(avg_catch_2006to2010 = mean(catch))%>%
      mutate(yrs_1999to2003 = change_old$yrs1999to2003[match(old_saup_id,change_old$id)],
             pct_chg = ((avg_catch_2006to2010-yrs_1999to2003)/yrs_1999to2003)*100)%>%
              as.data.frame()

saup_05_09 = saup_data%>%
  filter(Year>2004 & Year < 2010)%>%
  group_by(id_type,old_saup_id)%>%
  summarize(avg_catch_2005to2009 = mean(catch))%>%
  mutate(yrs_1999to2003 = change_old$yrs1999to2003[match(old_saup_id,change_old$id)],
         pct_chg = ((avg_catch_2005to2009-yrs_1999to2003)/yrs_1999to2003)*100)%>%
  as.data.frame()

saup_04_08 = saup_data%>%
  filter(Year>2003 & Year < 2009)%>%
  group_by(id_type,old_saup_id)%>%
  summarize(avg_catch_2004to2008 = mean(catch))%>%
  mutate(yrs_1999to2003 = change_old$yrs1999to2003[match(old_saup_id,change_old$id)],
         pct_chg = ((avg_catch_2004to2008-yrs_1999to2003)/yrs_1999to2003)*100)%>%
  as.data.frame()

saup_03_07 = saup_data%>%
  filter(Year>2002 & Year < 2008)%>%
  group_by(id_type,old_saup_id)%>%
  summarize(avg_catch_2003to2007 = mean(catch))%>%
  mutate(yrs_1999to2003 = change_old$yrs1999to2003[match(old_saup_id,change_old$id)],
         pct_chg = ((avg_catch_2003to2007-yrs_1999to2003)/yrs_1999to2003)*100)%>%
  as.data.frame()

#----------------------------------------------------------------------

# bring all together (not sure this is necessary...)

catch_chg = saup_06_10%>%
              inner_join(saup_05_09)%>%
              inner_join(saup_04_08)%>%
              inner_join(saup_03_07)%>%
            mutate(yrs_1999to2003 = change_old$yrs1999to2003[match(old_saup_id,change_old$id)])

# look at rows where yrs_1999to2003 are NA

missing = as.data.frame(filter(catch_chg,is.na(yrs_1999to2003)))

# 2 regions that don't have a match between old and new - these actually make sense and are ok.



#-------------------------------------------------------------------
#create change rasters for each period of years (4)

#bring in old SAUP region raster

old_saup_eez = raster(file.path(dir_N,'model/GL-NCEAS-Pressures_CommercialFisheries_v2013a/tmp/saup_fao_mol.tif'))

# need to substitue values of regions with percent change

# 6-16-2015: NEED TO RUN THIS! Maybe try on neptune??

rcl = saup_06_10%>%
        select(is=old_saup_id,becomes=pct_chg)

chg_ras_06_10 = reclassify(old_saup_eez,rcl,filename='v2015/pct_change_rasters/pct_chg_1999to2003_2006to2010.tif',progress='text')

