# libraries
library(raster)
library(dplyr)
library(RColorBrewer)
library(maps)

# paths
source('src/R/common.R') # get dir_neptune_data based on platform
dir_cells = file.path(dir_neptune_data, 'model/GL-NCEAS-SpeciesDiversity/ohi_spp/data')
dir_aqua  = file.path(dir_neptune_data, 'ingest/GL-SAUP-AquaMaps/tmp')
dir_tmp   = file.path(getwd(), 'Global/NCEAS-SpeciesDiversity_v2014/tmp')

# data
csqu_spp  = read.csv(file.path(dir_aqua , 'tbl_hcaf_species_native2.csv'), 
                     header=F, col.names=c('species_id','csquare_code','probability','boundbox_yn','fao_area_yn','id'))
spp       = read.csv(file.path(dir_aqua , 'tbl_fish_species.csv'))
cells     = read.csv(file.path(dir_cells, 'cells.csv'))
r         = raster(file.path(dir_cells, 'cells.tif'))
names(r)  = 'cid'

# associate cell id (cid) with csquare per species table (csqu_spp). 
# use dplyr "chaining" (%.%) and faster join methods (see http://blog.rstudio.org/2014/01/17/introducing-dplyr/)
cells_spp = csqu_spp %.%
  select(
    species_id,
    csquare_code, 
    probability) %.%
  inner_join(
    cells %.%
      select(
        csquare_code=csquarecod, 
        cid),
    by='csquare_code') %.%
  semi_join(as.data.frame(r), by='cid')

# get species id and cells associated with that species
sp_id = subset(spp, genus_name=='Eubalaena' & species_name=='glacialis', 'species_id', drop=T)
sp_cells = filter(cells_spp, species_id==sp_id)

# substitute values of raster cid with spp_cells' probability
r_sp = subs(r, sp_cells[,c('cid','probability')], by='cid', which='probability', subsWithNA=T)
spplot(r_sp)

# plot
png(file.path(dir_tmp, 'plot-sp-probability_Eubalaena-glacialis.png'), width=1500, height=800)
cols = rev(colorRampPalette(brewer.pal(11, 'Spectral'))(255)) # rainbow color scheme
plot(r_sp, col=cols)
map('world',col='gray95',fill=T,border='gray80',add=T)
dev.off()
