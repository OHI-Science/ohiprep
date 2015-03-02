########
## Compare different rasters datasets (different methods of extracting data: 1km vs. 500m)
## Make sure our data looks similar to Ben B's previous data
## MRF: Feb 26 2015

new1  <-  read.csv(file.path(dir_prod, 'tmp/new_rasters_1kmcell/lsp_protarea_inland1km.csv'))
old1 <-  read.csv(file.path(dir_prod, 'tmp/orig_rasters/lsp_protarea_inland1km.csv'))
new500 <- read.csv(file.path(dir_prod, 'tmp/new_rasters_500mcell/lsp_protarea_inland1km.csv'))
old_old <- read.csv('Global/WDPA-MPA_v2014/data/lsp_protarea_inland1km.csv')

new1  <- new1 %>%
  select(rgn_id, year, area_km2_new1km=area_km2)
old1  <- old1 %>%
  select(rgn_id, year, area_km2_oldnear1km=area_km2)
new500  <- new500 %>%
  select(rgn_id, year, area_km2_new500=area_km2)
old_old <- old_old %>%
  select(rgn_id, year, area_km2_oldData=area_km2)

compare <- merge(new1, old1, all=TRUE)
compare <- merge(compare, new500, all=TRUE)
compare <- merge(compare, old_old, all=TRUE)

compare <- compare %>%
  mutate(total = rowSums(compare[, c('area_km2_new1km', 'area_km2_oldnear1km', 'area_km2_new500', 'area_km2_oldData')], na.rm=TRUE)) %>%
  filter(total > 0) %>%
  select(-total)


compare$new500vs1km <- compare$area_km2_new500 - compare$area_km2_new1km
hist(compare$new500vs1km)
mean(compare$new500vs1km, na.rm=TRUE)
median(compare$new500vs1km, na.rm=TRUE)
range(compare$new500vs1km, na.rm=TRUE)
write.csv(compare, file.path(dir_prod, "compare_data.csv"), row.names=FALSE)

TotalArea <- compare %>%
  select(rgn_id, year, area_km2_new500) %>%
  group_by(rgn_id) %>%
  summarize(totalArea_new500=sum(area_km2_new500, na.rm=TRUE)) %>%
  arrange(totalArea_new500)
write.csv(TotalArea, file.path(dir_prod, "TotalArea.csv"), row.names=FALSE)

## offshore calculations:
new1  <-  read.csv(file.path(dir_prod, 'tmp/new_rasters_1kmcell/lsp_protarea_offshore3nm.csv'))
old1 <-  read.csv(file.path(dir_prod, 'tmp/orig_rasters/lsp_protarea_offshore3nm.csv'))
new500 <- read.csv(file.path(dir_prod, 'tmp/new_rasters_500mcell/lsp_protarea_offshore3nm.csv'))
old_old <- read.csv('Global/WDPA-MPA_v2014/data/lsp_protarea_offshore3nm.csv')

new1  <- new1 %>%
  select(rgn_id, year, area_km2_new1km=area_km2)
old1  <- old1 %>%
  select(rgn_id, year, area_km2_oldnear1km=area_km2)
new500  <- new500 %>%
  select(rgn_id, year, area_km2_new500=area_km2)
old_old <- old_old %>%
  select(rgn_id, year, area_km2_oldData=area_km2)

compare <- merge(new1, old1, all=TRUE)
compare <- merge(compare, new500, all=TRUE)
compare <- merge(compare, old_old, all=TRUE)

compare <- compare %>%
  mutate(total = rowSums(compare[, c('area_km2_new1km', 'area_km2_oldnear1km', 'area_km2_new500', 'area_km2_oldData')], na.rm=TRUE)) %>%
  filter(total > 0) %>%
  select(-total)


compare$new500vs1km <- compare$area_km2_new500 - compare$area_km2_new1km
hist(compare$new500vs1km)
mean(compare$new500vs1km, na.rm=TRUE)
median(compare$new500vs1km, na.rm=TRUE)
range(compare$new500vs1km, na.rm=TRUE)
write.csv(compare, file.path(dir_prod, "compare_data_offshore.csv"), row.names=FALSE)

TotalArea <- compare %>%
  select(rgn_id, year, area_km2_new500) %>%
  group_by(rgn_id) %>%
  summarize(totalArea_new500=sum(area_km2_new500, na.rm=TRUE)) %>%
  arrange(totalArea_new500)
write.csv(TotalArea, file.path(dir_prod, "TotalArea_offshore.csv"), row.names=FALSE)
