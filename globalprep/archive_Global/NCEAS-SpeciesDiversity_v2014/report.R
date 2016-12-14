library(raster)
library(maps)
library(RColorBrewer)

# TODO: run for 2013. All of below is OLD from 2012.

# set working directory
wd = 'D:/best/docs/data/model/GL-NCEAS-SpeciesDiversity/ohi_spp/data'
setwd(wd)

# read in data files
spp = read.csv('spp.csv')
tic = Sys.time()
cells_spp = read.csv('cells_spp.csv')
print(Sys.time() - tic) # Time difference of 1.382429 mins

# get cell id raster
r = raster('cells.tif') # in geographic coordinate system
names(r) = 'cid'
cid = values(r)

# plot individual species ----

# get a wide ranging endangered species: N Atlantic Right Whale
#   see IUCN map for comparison: http://maps.iucnredlist.org/map.html?id=41712
sp = subset(spp, scientific=='Eubalaena glacialis')

# get cells for that species
tic = Sys.time()
cells_sp = subset(cells_spp, sid==sp$sid)
r.sp = calc(r, fun=function(cid) {cid %in% cells_sp$cid})
print(Sys.time() - tic) # Time difference of

# plot
tic = Sys.time()
png('../plot_sp_rwhale.png')
plot(r.sp, col=c('lightblue','red'))
map('world',col='gray95',fill=T,border='gray80',add=T)
dev.off()
print(Sys.time() - tic) # Time difference of

# calculate and plot diversity ----

# species.csv field descriptions
#   sid:          unique species identifier
#   scientific:   Genus species scientific name
#   src_distn:    source of distribution, either IUCN RedList or Sea Around Us Project (aka AquaMaps) 
#                 distribution based on a 0.4 threshold 
#                 (see Ready et al 2010 Predicting the distributions of marine organisms at the global scale)
#   status:       IUCN extinction risk status: CR (critically endangered), EN (endangered), VU (vulnerable), NT (near threatened), LC (least concern),
#                 DD (data deficient), NE (not evaluated)
#   popn_trend:   population trend: decreasing, increasing, stable or unknown
#   troph:        trophic index ranging 2 to 5
#   kingdom/phylum/class/order/family: taxonomy
#   iucn_src_shp: source shapefile of IUCN range distribution
#   iucn_src_xls: source table for extinction risk and/or trophic status, 
#                 one of: Hagfish_export_RL.xlsx, Marine_spp_for_OHI.xlsx, SeaSnake_export_RL.xlsx, Tuna_Billfishes_TaxaList.xlsx
#   saup_sid:     Sea Around Us Project (aka AquaMaps) unique identifier
#   troph_se:     trophic index standard error
#   troph_src:    source of trophic index, either Fishbase or SeaLifeBase 
#   iucn_src:     group based on IUCN source file
#   grp:          group based on taxonomy


# show counts of species by status
table(spp$status, useNA='ifany')
#   CR   DD   EN   LC   NE   NT   VU <NA> 
#   43  636   80 1450 9779  346  366  140

addmargins(table(spp[,c('grp','status')], useNA='ifany'))
#                         status
# grp                         CR    DD    EN    LC    NE    NT    VU  <NA>   Sum
#   angelfish                  0     2     1    80     4     2     1     0    90
#   butterflyfish              0     8     0   117     1     3     0     0   129
#   corals                     5   140    25   298    54   177   202     0   901
#   groupers                   4    54     5    96   133    23    14     1   330
#   hagfishes                  1    29     2    35     2     2     6     1    78
#   mangroves                  2     3     3    44     0     6     5     3    66
#   marine_mammals             3    47    10    40     4     7    12     4   127
#   other                      1     0     0     5  2472     4     2     1  2485
#   other_fish                 5    34     5    33  6972    11    19    88  7167
#   parrotfish                 0     1     0    11    68     0     2    30   112
#   reptiles                   5    23     3    38     1     4     4     1    79
#   seagrasses                 0     9     3    48     0     5     7     1    73
#   sharks_rays_and_skates    14   203    20   172    21    95    71     0   596
#   tunas_and_billfishes       2     9     2    33    40     3     4     8   101
#   wrasses                    1    74     1   400     7     4    17     2   506
#   Sum                       43   636    80  1450  9779   346   366   140 12840

addmargins(table(spp[,c('src_distn','status')], useNA='ifany'))
#          status
# src_distn    CR    DD    EN    LC    NE    NT    VU  <NA>   Sum
#      IUCN    18   382    50  1178   114   230   266    48  2286
#      SAUP    25   254    30   272  9665   116   100    92 10554
#      Sum     43   636    80  1450  9779   346   366   140 12840

addmargins(table(spp[,c('grp','src_distn')], useNA='ifany'))
#                         src_distn
# grp                       IUCN  SAUP   Sum
#   angelfish                 86     4    90
#   butterflyfish            128     1   129
#   corals                   843    58   901
#   groupers                 160   170   330
#   hagfishes                 76     2    78
#   mangroves                 66     0    66
#   marine_mammals           121     6   127
#   other                      0  2485  2485
#   other_fish                13  7154  7167
#   parrotfish               106     6   112
#   reptiles                  70     9    79
#   seagrasses                73     0    73
#   sharks_rays_and_skates     0   596   596
#   tunas_and_billfishes      52    49   101
#   wrasses                  492    14   506
#   Sum                     2286 10554 12840

# get diversity data based on count of species per cell, limiting to species with a valid IUCN status
tic = Sys.time()
d = sqldf(c("CREATE INDEX idx_spp ON spp(sid)",
          "CREATE INDEX idx_cells_spp ON cells_spp(cid, sid)",
          "SELECT cid, COUNT(sid) AS cnt_spp_validstatus
          FROM cells_spp JOIN spp USING (sid) 
          WHERE status IN ('CR','EN','VU','NT','LC')
          GROUP BY cid"), dbname=tempfile())
print(Sys.time() - tic) # Time difference of 21.80176 mins
write.csv(d, 'spp_cnt_validstatus.csv', row.names=F)

# get cells for that species
d = read.csv('spp_cnt_validstatus.csv')
rownames(d) = d$cid
tic = Sys.time()
r.d = calc(r, fun=function(cid) {d[as.character(cid),'cnt_spp_validstatus']})
print(Sys.time() - tic) # Time difference of 2.533762 mins

# output raster
writeRaster(r.d, 'spp_cnt_validstatus.tif', datatype='INT4U', overwrite=T)

# plot
cols = rev(colorRampPalette(brewer.pal(11, 'Spectral'))(255)) # rainbow color scheme
png('../plot_cnt_spp_validstatus.png', width=1500, height=800)
plot(r.d, col=cols)
map('world',col='gray95',fill=T,border='gray80',add=T)
dev.off()