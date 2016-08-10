library(dplyr)
library(raster)


dir_M             <- c('Windows' = '//mazu.nceas.ucsb.edu/ohi',
                       'Darwin'  = '/Volumes/ohi',    ### connect (cmd-K) to smb://mazu/ohi
                       'Linux'   = '/home/shares/ohi')[[ Sys.info()[['sysname']] ]]

saup_pressures = file.path(dir_M,'git-annex/Global/SAUP-FishCatchByGearType_Halpern2008/data')

saup_update = file.path(dir_M, 'model/GL-SAUP-FisheriesCatchData_v2013')

# Data used for the commercial fishing pressures layers in OHI 2013. 
#These are derivatives and updates to the 2008 data 
ohi_2013  = file.path(dir_M,'model/GL-NCEAS-Pressures_CommercialFisheries_v2013a')

saup_2015 = file.path(dir_M,'git-annex/globalprep/SAUP_data_2015')


# using demersal destructive as demo

dem_d_orig = raster(file.path(saup_orig,'catch_dem_d_gcs.tif'))

#original catch data from SAUP (1950-2011). Remember the data from 2006-2011 is just
#based on FAO catch (I THINK!?) Or Kristin Kleisner projected it forward without rasterizing

orig_catch = read.csv(file.path(saup_update,'tmp/saup_stocks-catch_by_saup-eez.csv'))

# New catch from SAUP (1950-2010)
new_catch = read.csv(file.path(saup_2015,'raw/ohi_main.csv'))


# Previous data used to update fishing pressures

chg = read.csv(file.path(saup_update,'data/pct_chg_saup_2009to2011_vs_1999to2003.csv'))

#ohi_eezs

eezs = read.csv(file.path(saup_2015,'raw/ohi_eez.csv'))

#---------------------------------------------------------------------

# Look at Taxons previously ignored when aggregating catch. The taxons were:

saup_taxon_exclude = read.csv(file.path(saup_update,'tmp/global_srcdata_ss_saup_excluded_stock.csv'))

taxon_names = orig_catch%>%
                dplyr::select(Taxonkey,TaxonName,CommonName)%>%
                filter(Taxonkey %in% saup_taxon_exclude$stock_id)%>%
                  unique()
#>stock_id
#1    100000
#2    100011  *This taxonkey doesn't show up in the dataset
#3    100025
#4    100039
#5    100047
#6    100058
#7    100077  * This taxonkey doesn't show up in the dataset
#8    100139
#9    100239
#10   100339
           


#> taxon_names
#Taxonkey                        TaxonName         CommonName
#1       100000                   Marine animals     Marine animals
#2       100339     Marine fishes not identified     Pelagic fishes
#5       100139     Marine fishes not identified          Finfishes
#7       100058    Miscellaneous marine molluscs    Marine molluscs
#6324    100239     Marine fishes not identified       Groundfishes
#6986    100047 Miscellaneous marine crustaceans Marine crustaceans
#7109    100025  Miscellaneous diadromous fishes  Diadromous fishes
#14979   100039     Marine fishes not identified      Marine fishes


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

#--------------------------------------------------------------------------



#aggregate new catch in same way previous one was done

# (1) Aggregate the new catch data so that we get total catch per year per eez. This completely ignores taxon type. IN pressures it is not
# taxon specific since we dont have the information about gear type specific to species


new_catch_agg = new_catch%>%
                 filter(!TaxonKey %in% saup_taxon_exclude$stock_id)%>%
                  mutate(id = ifelse(EEZID==0,FAOAreaID+1000,EEZID),
                         id_type = ifelse(id>1000,'fao','eez'))%>% #as of now (6/7/15 we have no FAO data)
                   group_by(Year,id_type,id)%>%
                    summarize(catch=sum(CatchAmount))%>%
                    as.data.frame()

# now we have the total catch per year per id. 

#---------------------------------------------------

#(2) Want to calculate change. Lets look at 1999 to 2003, then also look at the previous 1999 to 2003

# bring in old data
all_old = read.csv(file.path(saup_update,'data/catch-tons_per_saup-or-fao_year.csv'))

change_old = read.csv(file.path(saup_update,'data/pct_chg_saup_2009to2011_vs_1999to2003.csv'))


# get mean tons/year across 1999-2003 per id

new_catch_1999to2003 = new_catch_agg%>%
                        filter(Year>1998 & Year<2004)%>%
                        group_by(id_type,id)%>%
                        summarize(avg.catch_1999to2003_new = mean(catch))%>%
                        as.data.frame()%>%
                        mutate(avg.catch_1999to2003_old = change_old$yrs1999to2003[match(id,change_old$id)],
                              diff = avg.catch_1999to2003_new-avg.catch_1999to2003_old)%>%
                          filter(diff!='NA')

#we get 207 id comparisons

plot(new_catch_1999to2003$avg.catch_1999to2003_new~new_catch_1999to2003$avg.catch_1999to2003_old,pty='s',
     xlab='Old Avg Catch (tons)',ylab='New Avg Catch (tons)',main='Average Catch per region (eez/fao)\n1999 to 2003')
abline(0,1)

# look at them logged
plot(log(new_catch_1999to2003$avg.catch_1999to2003_new+1)~log(new_catch_1999to2003$avg.catch_1999to2003_old+1),pty='s',
     xlab='Old Avg Catch (tons)',ylab='New Avg Catch (tons)',main='Average Catch per region (eez/fao)\n1999 to 2003')
abline(0,1)

#NOTE there are some ids that have been retired and added so here we are only looking at matching ones
# i believe these do not change

# Unfortunately we can't look at percent change differences, although I'm not sure that would help us
# anyway. This dataset only goes to 2010 where our previous dataset went to 2011 (combining both FAO
# and SAUP data)

#-------------------------------------------------------

# I've still been wondering why we chose these sets of years to compare (1999 - 2003 and 2008 to 2011)
# Going to go ahead and look at 1999 to 2003 and 2007 to 2010, as well as 2005 to 2010.


#remember old data is saved as 'chg

#aggregate catch between 1999to2003 and 2008to2010 and 2006 to 2010


# 2006-2010

old_catch_2006to2010 = all_old%>%
                        filter(year > 2005 & year < 2011)%>%
                        group_by(id)%>%
                        summarize(catch = mean(catch_tons))


new_chg_2006to2010 = new_catch_agg%>%
  filter(Year %in% c(2006,2007,2008,2009,2010))%>%
  group_by(id_type,id)%>%
  summarize(avg.catch_2006to2010_new = mean(catch))%>%
  as.data.frame()%>%
  mutate(avg.catch_2006to2010_old = old_catch_2006to2010$catch[match(id,old_catch_2006to2010$id)],
         diff = avg.catch_2006to2010_new-avg.catch_2006to2010_old)%>%
  filter(diff!='NA')


# 2008-2010

old_catch_2008to2010 = all_old%>%
  filter(year > 2007 & year < 2011)%>%
  group_by(id)%>%
  summarize(catch = mean(catch_tons))



new_chg_2008to2010 = as.data.frame(new_catch_agg)%>%
        filter(Year %in% c(2008,2009,2010))%>%
  group_by(id_type,id)%>%
  summarize(avg.catch_2008to2010_new = mean(catch))%>%
  mutate(avg.catch_2008to2010_old = old_catch_2008to2010$catch[match(id,old_catch_2008to2010$id)],
         diff = avg.catch_2008to2010_new-avg.catch_2008to2010_old)%>%
  filter(diff!='NA')


#look across all groups of years, including old (1999 to 2003)

all = new_catch_1999to2003%>%
        rename(catch_1999to2003_NEW = avg.catch_1999to2003_new)%>%
          mutate(catch_2008to2010_NEW = new_chg_2008to2010$avg.catch_2008to2010_new[match(id,new_chg_2008to2010$id)],
                catch_2006to2010_NEW = new_chg_2006to2010$avg.catch_2006to2010_new[match(id,new_chg_2006to2010$id)],
                catch_1999to2003_OLD = chg$yrs1999to2003[match(id,chg$id)])%>%
        dplyr::select(id_type,id,catch_1999to2003_NEW,catch_1999to2003_OLD,catch_2008to2010_NEW,catch_2006to2010_NEW)%>%
        mutate(pct.chg_2008to2010_NEW = ((catch_2008to2010_NEW-catch_1999to2003_NEW)/catch_1999to2003_NEW)*100,
               pct.chg_2006to2010_NEW = ((catch_2006to2010_NEW-catch_1999to2003_NEW)/catch_1999to2003_NEW)*100,
               pct.chg_2009to2011_OLD = chg$pct_chg[match(id,chg$id)]) # remember this was calculated using different data for these years...

# look at the average catch from 2008 to 2010 and 2006 to 2010

x = all$catch_2008to2010_NEW
y = all$catch_2006to2010_NEW

plot(x~y,ylab='Average Catch (tons) 2008-2010',xlab='Average Catch (tons) 2006-2010')
abline(0,1)

# SUPER CLOSE - maybe doesnt matter what one we choose? Let's look at pct change now

x2 = all$pct.chg_2008to2010_NEW
y2 = all$pct.chg_2006to2010_NEW
plot(x2~y2,ylab='Percent Change in Catch 2008-2010',xlab='Percent Change in Catch 2006-2010')
abline(0,1)

# remove the large outlier to get a better picture
t = filter(all,pct.chg_2008to2010_NEW<2000)

x2 = t$pct.chg_2008to2010_NEW
y2 = t$pct.chg_2006to2010_NEW
plot(x2~y2,ylab='Percent Change in Catch 2008-2010',xlab='Percent Change in Catch 2006-2010')
abline(0,1)

# lets log it to look?

plot(log(x2)~log(y2),ylab='Percent Change in Catch 2008-2010',xlab='Percent Change in Catch 2006-2010')
abline(0,1)

#very close so I don't think it matters what number of years we choose

# now look at the differences if we use the most recent data most recetn 3 years (2008-2010) and 
#the old data which looked at percent change 2009to 2011

x3 = all$pct.chg_2009to2011_OLD
y3 = all$pct.chg_2006to2010_NEW


plot(x3~y3,ylab='Previous % change (2009to2011 old data)',xlab='% Change with new data (2006-2010)')
abline(0,1)


# remove outlier to get better picture

m = filter(all,pct.chg_2006to2010_NEW<500)

x3 = m$pct.chg_2009to2011_OLD
y3 = m$pct.chg_2006to2010_NEW

plot(x3~y3,ylab='Previous % change (2009to2011 old data)',xlab='% Change with new data (2006-2010)')
abline(0,1)


#look at log?

plot(log(x3)~log(y3),ylab='Previous % change (2009to2011 old data)',xlab='% Change with new data (2006-2010)')
abline(0,1)

# Lot of NA's because negative numbers in old data.


#-----------------------------------------------------------

# Rasterizing percent change

pct_chg = raster(file.path(ohi_2013,'tmp/saup_pct_chg_mol.tif'))
# need the shape file

saup_rgns = 
