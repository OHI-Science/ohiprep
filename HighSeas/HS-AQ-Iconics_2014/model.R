########################################################################################
## GOAL: Iconics analysis
## for HS and AQ
## Date: Mar 21 2014
## MRF
########################################################################################
library(sp)
library(rgdal)
library(raster)
library(stringr)
rm(list = ls())

source('../ohiprep/src/R/common.R')

dir_curr = '../ohiprep/Global/HS-AQ-Iconics_2014'

# ICO status/trend calculations:
# Primarily uses data from SPP calculation with supplemental tables added to ID iconic species.
# (see R script in that file for more information about these data)
cells <- read.csv(file.path(dir_neptune_data, "model/GL-HS-AQ-SpeciesRichness_v2014/tmp/am_cells_rgn_proportions.csv"))
cells_spp <- read.csv(file.path(dir_neptune_data, "model/GL-HS-AQ-SpeciesRichness_v2014/tmp/am_cells_spp.csv"))
spp <- read.csv(file.path(dir_neptune_data, "model/GL-HS-AQ-SpeciesRichness_v2014/raw/spp.csv"))
am_cells <- read.csv(file.path(dir_neptune_data, "model/GL-NCEAS-SpeciesDiversity_v2013a/tmp/am_cells_data.csv")) #for cell areas (and other abiotic conditions)


# Identify Iconic species ----
icons <- read.csv(file.path(dir_neptune_data, "model/GL-NCEAS-IconicSpecies_v2013/data/ico_spp_extinction_status.csv"))

icons$sciname <- str_trim(icons$sciname)
icons$sciname[icons$sciname=="Kogia simus"] <- "Kogia sima"
icons$sciname[icons$sciname=="Cephalorhynchus hectori maui"] <- "Cephalorhynchus hectori"
setdiff(icons$sciname, spp$sciname)
length(unique(icons$sciname)) #N=78 taxa

# AQ: Katie sent an excel file with iconic species.  Seeing how many of those match the species in the IUCN list:
AQ_icons <- read.csv(file.path(dir_curr, "raw/AQ_IconicSpecies.csv"))
setdiff(AQ_icons$Scientific.name, icons$sciname) # N=23 that were not already on the list
setdiff(AQ_icons$Scientific.name, spp$sciname) # only two species have no IUCN data
length(intersect(AQ_icons$Scientific.name, spp$sciname)) #N=28 taxa with IUCN data

AQ_iconSp <- AQ_icons %>%
  select("sciname" = Scientific.name)

# prepare icon species list:
iconSp <- icons %>%
  select(sciname) %>%
  rbind(AQ_iconSp) %>%
  unique()
dim(iconSp) 

### save master list to new folder:
write.csv(iconSp, "Antarctica/AQ_ICO/raw/ICO_master.csv", row.names=FALSE)

# subset species lists to include only iconics ---- 
length(intersect(spp$sciname, iconSp$sciname)) #N=99: looks correct
spp <- spp[spp$sciname %in% iconSp$sciname, ]
cells_spp <- cells_spp[cells_spp$sid %in% spp$sid, ]
length(unique(cells_spp$sid))
setdiff(cells_spp$sid, spp$sid)
setdiff(spp$sid, cells_spp$sid)
spp[spp$sid == 60217, ] # for some reason this species is not in the spatial data
dim(spp)

## Summarize species data for AQ and HS for data check ----
#tmp <- readOGR(dsn="N:\\model\\GL-HS-AQ-PressuresSummary_v2013\\GL-NCEAS-Regions_v2014\\data", layer="eez_ccmlar_fao_gcs")
AQ_regions <- read.csv("../ohiprep/Antarctica/Other_v2014/rgn_labels_ccamlr.csv")
HS_regions <- read.csv("../ohiprep/HighSeas/HS_other_v2014/rgn_labels_fao.csv")

cells_AQ <- cells[cells$sp_id %in% AQ_regions$sp_id, ]
table(cells_AQ$sp_id)

cells_HS <- cells %>%
  filter(sp_id %in% HS_regions$sp_id) %>%
  left_join(HS_regions) %>%
  select(LOICZID, proportionArea, rgn_id)  
table(cells_HS$rgn_id)

cells_HS_arctic <- cells_HS %>%
  filter(rgn_id %in% '260') 
table(cells_HS_arctic$rgn_id)

cells_spp_AQ <- unique(cells_spp$sid[cells_spp$LOICZID %in% cells_AQ$LOICZID])
cells_spp_HS <- unique(cells_spp$sid[cells_spp$LOICZID %in% cells_HS$LOICZID])
cells_spp_HS_arctic <- unique(cells_spp$sid[cells_spp$LOICZID %in% cells_HS_arctic$LOICZID])


spp_AQ <- spp[spp$sid %in% cells_spp_AQ, ]
spp_HS <- spp[spp$sid %in% cells_spp_HS, ]
spp_HS_arctic <- spp[spp$sid %in% cells_spp_HS_arctic, ]

#write.csv(spp_AQ, "data\\Antartica_icons.csv", row.names=FALSE)
#write.csv(spp_HS, "data\\HighSeas_icons.csv", row.names=FALSE)


### Master file with cell information: 
am_cells <- am_cells %.%
  select(LOICZID, CellArea)

setdiff(cells$LOICZID, am_cells$LOICZID) # should be 0
setdiff(am_cells$LOICZID, cells$LOICZID) # These are cells that fell outside a rgn polygon
am_cells$LOICZID <- as.numeric(am_cells$LOICZID)

cells <- cells %.%
  left_join(am_cells, by="LOICZID")
cells$rgn_area <- cells$CellArea*cells$proportionArea

# create lookup table of weights for extinction risk categories and population trends
category <- data.frame(category=c("LC", "NT", "VU", "EN", "CR", "EX"), 
                       catScore =c(0, 0.2, 0.4, 0.6, 0.8, 1))
popn_trend <- data.frame(popn_trend=c("Decreasing", "Stable", "Increasing"), 
                         trendScore=c(-0.5, 0, 0.5))

spp <- spp %.%
  select(sid, category, popn_trend)%.%
  left_join(category) %.%
  left_join(popn_trend) 


# For each cell: calculate average weight and count for category and trend scores 
scoreData <- cells_spp %.%
  group_by(LOICZID) %.%
  inner_join(spp) %.% 
  summarise(category_linear_cnt = length(category[!(category %in% "DD")]), #cut data defficients
            category_linear_avg = mean(catScore, na.rm=TRUE),
            popn_trend_linear_cnt = sum(!is.na(trendScore)),
            popn_trend_linear_avg = mean(trendScore, na.rm=TRUE))
# rescale lower end of the biodiversity goal to be 0 when 75% species are extinct, a level comparable to the five documented mass extinctions

scoreData$category_linear_score <- ((1 - scoreData$category_linear_avg) - 0.25) / 0.75 * 100
scoreData$LOICZID <- as.numeric(scoreData$LOICZID)

cells <- cells %.%
  left_join(scoreData, by="LOICZID")

# calculate area-weighted regional scores ----
regionScores <- cells %.%
  group_by(sp_id) %.%
  summarise(rgn_spp_score_2014=sum(category_linear_score*rgn_area, na.rm=TRUE)/sum(rgn_area, na.rm=TRUE),
            rgn_spp_trend_2014=sum(popn_trend_linear_avg*rgn_area, na.rm=TRUE)/sum(rgn_area, na.rm=TRUE))
regionScores$sp_id <- as.integer(regionScores$sp_id)



# organize and write to csv:

AQ_scores  <- AQ_regions %.%
  select(sp_id) %.%
  left_join(regionScores) %.%
  select(sp_id, score=rgn_spp_score_2014)
write.csv(AQ_scores, file.path(dir_curr, "data/rgn_ico_score_2014_AQ.csv"), row.names=FALSE)

AQ_trend  <- AQ_regions %.%
  select(sp_id) %.%
  left_join(regionScores) %.%
  select(sp_id, score=rgn_spp_trend_2014)
write.csv(AQ_trend, file.path(dir_curr, "data/rgn_ico_trend_2014_AQ.csv"), row.names=FALSE)


HS_scores  <- HS_regions %.%
  select(sp_id, rgn_id) %.%
  left_join(regionScores) %.%
  select(rgn_id, score=rgn_spp_score_2014)
write.csv(HS_scores, file.path(dir_curr, "data/rgn_ico_score_2014_HS.csv"), row.names=FALSE)

HS_trend  <- HS_regions %.%
  select(sp_id, rgn_id) %.%
  left_join(regionScores) %.%
  select(rgn_id, score=rgn_spp_trend_2014)
write.csv(HS_trend, file.path(dir_curr, "data/rgn_ico_trend_2014_HS.csv"), row.names=FALSE)


####################################################### 
### side:  exploring data:
#######################################################

 spp2 <- read.csv(file.path(data, "raw/spp.csv")) %>%
   select(sid, sciname, category, popn_trend)
 scoreData2 <- cells_spp %>%
   inner_join(spp2)
 
 #Antarctica data:
 AQ_regions <- read.csv("Antarctica/Other_v2014/rgn_labels_ccamlr.csv") 
 cells2 <- cells %>%
   left_join(scoreData2, by="LOICZID") %>%
   filter(sp_id %in% AQ_regions$sp_id)
 
 species <- cells2 %>%
   select(sciname, category, popn_trend) %>%
   filter(category != "DD")
 species <- unique(species)
 
 write.csv(species, file.path(dir_curr, "data_explore/AQ_ICO_species_status.csv"), row.names=FALSE)
 
 # High Seas data:
 HS_regions <- read.csv("HighSeas/HS_other_v2014/rgn_labels_fao.csv")
 cells2 <- cells %>%
   left_join(scoreData2, by="LOICZID") %>%
   filter(sp_id %in% HS_regions$sp_id)
 
 species <- cells2 %>%
   select(sp_id, sciname, category, popn_trend) %>%
   filter(category != "DD") %>%
   left_join(HS_regions, by='sp_id') %>%
   select(rgn_id, label, sciname, category, popn_trend)
 
 speciesCells <- species %>%
  group_by(rgn_id, label, sciname, category, popn_trend) %>%
  summarize(Ncells = length(rgn_id))
  
 speciesCells[speciesCells$rgn_id==262,]
 
 write.csv(speciesCells, file.path(dir_curr, "data_explore/HS_ICO_species_regions.csv"), row.names=FALSE)
 
 data <- read.csv("../ohi-global/global2014/scores_2014_2014-09-09.csv") %>%
   filter(region_type == "fao",
          goal == "SPP") %>%
   arrange(dimension, region_id) 

