# library(foreign)
# library(sp)
# library(rgdal)
# library(raster)
# library(maptools)
library(readr)

source('~/github/ohiprep/src/R/common.R')

goal     <- 'lsp'
scenario <- 'v2016'
dir_anx       <- file.path(dir_M, 'git-annex/globalprep') 
dir_goal      <- file.path('~/github/ohiprep/globalprep', goal, scenario)
dir_goal_anx  <- file.path(dir_anx,            goal, scenario)
dir_data_wdpa <- file.path(dir_anx, '_raw_data/wdpa_mpa', 'd2016') 


rast_wdpa_file <- file.path(dir_goal_anx, 'int/wdpa_designated_mol.tif')
rast_wdpa <- raster::raster(rast_wdpa_file)

plot(rast_wdpa)

### to check existing SPDF for unfiltered polygons:
wdpa_df <- foreign::read.dbf(paste0(shp_xformed, '.dbf'))

names(wdpa_df)
#  WDPAID | WDPA_PID | PA_DEF | NAME | ORIG_NAME | DESIG | DESIG_ENG | DESIG_TYPE | IUCN_CAT
#  INT_CRIT | MARINE | REP_M_AREA | GIS_M_AREA | REP_AREA | GIS_AREA | NO_TAKE | NO_TK_AREA | STATUS
#  STATUS_YR | GOV_TYPE | OWN_TYPE | MANG_AUTH | MANG_PLAN | VERIF | METADATAID | SUB_LOC | PARENT_ISO
#  ISO3
nonmpa_check <- wdpa_df %>%
  filter(ISO3 == 'USA') %>% ### USA non-programmatic management plans were a problem in 2015
  select(MANG_PLAN) %>%
  distinct()
### if there are any non-MPA programmatic species management plans in here, filter 'em

newstatus_check <- wdpa_df %>%
  filter(STATUS %in% c("Adopted", "Inscribed"))
doubled_check <- wdpa_df %>%
  filter(str_detect(tolower(NAME), "yosemite|yellowst|chitwan|tikal|nahanni"))
### looks like most of those parks are covered under other rubrics as well

### check distribution of STATUS_YR to see if there are breaks 
status_yr <- wdpa_df$STATUS_YR
sum(status_yr == 0) ### 22072 (about 10%)
hist(status_yr[status_yr > 0])

quantile(status_yr, probs = seq(0, 1, 0.1))



stats_3nm <- read_csv(file.path(dir_goal, 'int', 'zonal_stats_3nm.csv'))
stats_1km <- read_csv(file.path(dir_goal, 'int', 'zonal_stats_1km.csv'))

### rgn areas from layers.csv

rgn_area_1km <- read_csv(file.path('~/github/ohi-global/eez2013/layers', 'rgn_area_inland1km.csv'))
rgn_area_3nm <- read_csv(file.path('~/github/ohi-global/eez2013/layers', 'rgn_area_offshore3nm.csv'))

### rgn names for convenience

rgn_names <- foreign::read.dbf(file.path(dir_anx, 'spatial/v2015/data/regions_gcs.dbf')) %>%
  filter(rgn_typ == 'eez') %>%
  select(rgn_id, rgn_name = rgn_nam)


lsp_thresh <- 0.30
### Determine total cells per region (n_cells_tot) and then a cumulative
### total of cells per region
prot_1km <- stats_1km %>%
  group_by(rgn_id) %>%
  mutate(n_cells_tot = sum(n_cells),
         n_cells_cum = cumsum(n_cells),
         a_tot_cells = n_cells_tot / 4,
         a_prot_cells = n_cells_cum / 4) %>%
  ungroup() %>%
  left_join(rgn_area_1km %>% rename(a_tot_rgn = area_km2), by = 'rgn_id') %>%
  filter(!is.na(year))  %>% ### this ditches non-protected cell counts but already counted in n_cells_tot
  mutate(pct_prot_cells   = n_cells_cum / n_cells_tot,
         lsp_status_cells = ifelse(pct_prot_cells > lsp_thresh, 100, (pct_prot_cells / lsp_thresh) * 100)) %>%
  mutate(pct_prot_rgn     = a_prot_cells / a_tot_rgn,
         lsp_status_rgn   = ifelse(pct_prot_rgn > lsp_thresh, 100, (pct_prot_rgn / lsp_thresh) * 100))

prot_3nm <- stats_3nm %>%
  group_by(rgn_id) %>%
  mutate(n_cells_tot = sum(n_cells),
         n_cells_cum = cumsum(n_cells),
         a_tot_cells = n_cells_tot / 4,
         a_prot_cells = n_cells_cum / 4) %>%
  ungroup() %>%
  left_join(rgn_area_3nm %>% rename(a_tot_rgn = area_km2), by = 'rgn_id') %>%
  filter(!is.na(year))  %>% ### this ditches non-protected cell counts but already counted in n_cells_tot
  mutate(pct_prot_cells   = n_cells_cum / n_cells_tot,
         lsp_status_cells = ifelse(pct_prot_cells > lsp_thresh, 100, (pct_prot_cells / lsp_thresh) * 100)) %>%
  mutate(pct_prot_rgn     = a_prot_cells / a_tot_rgn,
         lsp_status_rgn   = ifelse(pct_prot_rgn > lsp_thresh,   100, (pct_prot_rgn / lsp_thresh) * 100))



prot_df <- prot_1km %>%
  dplyr::select(rgn_id, year, 
                lsp_st_1km_rgn   = lsp_status_rgn, 
                lsp_st_1km_cells = lsp_status_cells,
                a_tot_rgn_1km    = a_tot_rgn,
                a_tot_cells_1km  = a_tot_cells) %>%
  left_join(prot_3nm %>% 
              dplyr::select(rgn_id, year, 
                            lsp_st_3nm_rgn   = lsp_status_rgn, 
                            lsp_st_3nm_cells = lsp_status_cells,
                            a_tot_rgn_3nm    = a_tot_rgn,
                            a_tot_cells_3nm  = a_tot_cells),
            by = c('rgn_id', 'year')) %>%
  mutate(lsp_status_rgn = (lsp_st_1km_rgn + lsp_st_3nm_rgn) / 2,
         lsp_status_cells = (lsp_st_1km_cells + lsp_st_3nm_cells) / 2)



#########################################################################=
### Check areas based directly on 1 km, 3 nm, and global(?) Mollweide shapefile
#########################################################################=

rgn_poly_mol <- rgdal::readOGR(dsn = file.path(dir_anx, '../Global/NCEAS-Regions_v2014/data'), 
                               layer = 'rgn_inland1km_mol', 
                               stringsAsFactors = FALSE)

area_mol_df <- rgn_poly_mol@data %>%
  mutate(area_recalc = rgeos::gArea(rgn_poly_mol, byid = TRUE))

area_mol_df <- area_mol_df %>%
  mutate(area_recalc = area_recalc/1e6) %>%
  rename(area_km2_from_shp = area_km2,
         area_km2_from_gArea = area_recalc) %>%
  left_join(rgn_area_1km %>% rename(area_km2_from_layer = area_km2), by = 'rgn_id')

area_mol_df <- area_mol_df %>%
  select(rgn_id, rgn_name, area_km2_from_shp, Shape_Area, area_km2_from_gArea, area_km2_from_layer) %>%
  left_join(prot_1km %>%
              filter(year == 2015) %>%
              select(rgn_id, a_km2_from_cells = a_tot_cells),
            by = 'rgn_id')
write_csv(area_mol_df, file.path(dir_goal, 'int/area_check_inland1km.csv'))

### now for 3 nm

rgn_poly_mol <- rgdal::readOGR(dsn = file.path(dir_anx, '../Global/NCEAS-Regions_v2014/data'), 
                               layer = 'rgn_offshore3nm_mol', 
                               stringsAsFactors = FALSE)

area_mol_df <- rgn_poly_mol@data %>%
  mutate(area_recalc = rgeos::gArea(rgn_poly_mol, byid = TRUE))

area_mol_df <- area_mol_df %>%
  mutate(area_recalc = area_recalc/1e6) %>%
  rename(area_km2_from_shp = area_km2,
         area_km2_from_gArea = area_recalc) %>%
  left_join(rgn_area_1km %>% rename(area_km2_from_layer = area_km2), by = 'rgn_id')

area_mol_df <- area_mol_df %>%
  dplyr::select(rgn_id, rgn_name, area_km2_from_shp, Shape_Area, area_km2_from_gArea, area_km2_from_layer) %>%
  left_join(prot_3nm %>%
              filter(year == 2015) %>%
              dplyr::select(rgn_id, a_km2_from_cells = a_tot_cells),
            by = 'rgn_id')
write_csv(area_mol_df, file.path(dir_goal, 'int/area_check_offshore3nm.csv'))

#########################################################################=
### check vs 2015 assessment (using eez2013 scores)
#########################################################################=

lsp_v2015 <- read_csv('~/github/ohi-global/eez2013/scores.csv') %>%
  rename(rgn_id = region_id, lsp_v2015 = score) %>%
  filter(dimension == 'status' & goal == 'LSP') %>%
  left_join(prot_df %>% 
              filter(year == 2012), 
            by = 'rgn_id') %>%
  left_join(rgn_names, by = 'rgn_id')

library(ggplot2)
lsp_plot_v2015 <- ggplot(lsp_v2015, 
                         aes(x = lsp_v2015, y = lsp_status_cells)) +
  geom_point(alpha = .6, color = 'green') +
  geom_point(aes(y = lsp_status_rgn), alpha = .6) +
  theme(legend.position = 'none') +
  geom_abline(slope = 1, intercept = 0, color = 'red') +
  labs(x = 'LSP status v2015 (eez2013: data through 2012)',
       y = 'LSP status v2016 (data through 2012))',
       title = 'LSP status compare')


library(plotly)
ggplotly(lsp_plot_v2015)

ggsave(file.path(dir_goal, 'int/plot_v2015_v2016_cellarea.png'), plot = lsp_plot_v2015)

# lsp_v2015 <- lsp_v2015 %>%
#   dplyr::select(rgn_id, year, lsp_v2015, lsp_status_rgn, lsp_status_cells, rgn_name) %>%
#   mutate(diff = lsp_status_rgn - lsp_v2015 ) %>%
#   arrange(diff)
# 
# write_csv(lsp_v2015, file.path(dir_goal_anx, 'int/check_vs_2015.csv'))
