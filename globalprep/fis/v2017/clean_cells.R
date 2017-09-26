## Fix Cell Issues

## Jamie Afflerbach

## This script fixes the issue of some half degree cells that slighlty overlap
## the OHI regions shapefile, leaving them with a proportional area less than 1.
## This would cause us to lose catch when assigning catch to cells. To fix this,
## we define a vector of cellids that have a proportionArea <1 and are NOT
## duplicated (i.e. the other portion of the area missing is not accounted for)
## and assign a proportionArea of 1 to these cells.

## The output is saved as cells.csv

## this data links OHI rgns to SAUP cell ids and calculates total proportion of each cell within each eez
cells_raw <- read.csv(file.path(dir_M, "git-annex/globalprep/fis/v2015/raw/saup_rasters_to_ohi_rgns.csv"))%>%
  rename(CellID=saup_cell_id) %>%
  group_by(CellID) %>%
  mutate(total_area = sum(proportionArea)) 

### list of cells with areas > 1 (indicates something strange is going on)
bad_ones <- filter(cells_raw, total_area>1) %>%
  arrange(CellID) 

data.frame(bad_ones)

## get a list of cells that have eez or fao categories:
## used later to determine whether cells with OHI regions but no FAO region are an issue:
cells_water <- cells_raw %>%
  group_by(CellID) %>%
  mutate(cell_rgns = paste(sort(unique(rgn_typ)), collapse=", ")) %>%
  arrange(CellID)

# check this went ok:
list <- table(cells_water$cell_rgns)
list
filter(cells_water, cell_rgns=="eez, land")

#list the types of cells
eez_fao_cats <- c("eez", "eez-ccamlr", "eez-ccamlr, land-ccamlr", "eez-disputed",
                  "eez-disputed, fao", "eez-disputed, land-disputed", "eez-disputed, land, land-disputed", "eez, eez-ccamlr",
                  "eez, eez-disputed", "eez, eez-disputed, fao", "eez, eez-disputed, land", "eez, eez-disputed, land-disputed",
                  "eez, eez-disputed, land, land-disputed", "eez, fao", "eez, land", "eez, land, land-noeez", "fao")                                   
cells_water <- cells_water %>%
  filter(CellID, cell_rgns %in% eez_fao_cats)


## cells should have a total area of <=1, what causes some cells to have more...and is this a large problem
## No, most cells are fine.
## This seems to happen because of polygon overlap.
## scenario 1: FAO region 262 overlaps other FAO region polygons by a small amount.  In this case a small proportion of the catch within a cell will
## be assigned to two regions.  This will be a small error. (No correction)
## scenario 2: In many cases, the total area is very close to one which may reflect rounding error and is not significant. (Usually no correction)
## scenario 3: In some cases, the regions are small islands where there doesn't appear to be a hole so both the land and underlying eez are counted. 

## This can also occur along any eez/land boundary...but it looks like it mainly happens for islands. If the 
## overlap is for land/eez within the same region, this will be corrected. 


cells <- read.csv(file.path(dir_M, "git-annex/globalprep/fis/v2015/raw/saup_rasters_to_ohi_rgns.csv")) %>%
  rename(CellID=saup_cell_id) %>%
  group_by(CellID, rgn_id) %>%   # groups land and eez data (that way the cell catch is fully applied to the region...rather than cutting the portion that overlaps land) 
  dplyr::summarise(area = sum(proportionArea)) %>%
  mutate(area = ifelse(area > 1, 1, area))%>%  ## this corrects when there is land/eez overlap within the same region resulting in cell area >1 (scenario 3 above)
  ungroup()  

## Remaining errors from scenario 1 and 2 above:  These are very small errors..nothing to be concerned about.
bad_ones <- cells %>%
  group_by(CellID) %>%
  mutate(total_cell_area = sum(area)) %>%
  filter(total_cell_area > 1) %>%
  arrange(CellID) 
data.frame(bad_ones)


# get the list of cell ids that are duplicated, and use this list of values to adjust the area to equal 1 ONLY for those cells that are not duplicated


## Id duplicated cells:
dup <- cells$CellID[duplicated(cells$CellID)]


## these are the cells that were cut off prematurely due to edge effects, etc.
tmp <- filter(cells, !(CellID %in% dup) & area < 1)
head(tmp)

#read in the dataset matching each cell to an FAO region (need both OHI and FAO region for analysis)
fao_cells <- read.csv( file.path(dir_M, "git-annex/globalprep/fis/v2015/raw/saup_rasters_to_fao_rgns.csv")) %>%
  rename(CellID=saup_cell_id)

cells_df <- cells %>%
  mutate(area = ifelse(CellID %in% dup, area, 1))%>%  # if the cell doesn't cover >1 region, then change cell areas to one to capture entire cell's catch (these are <1 area due to edge effects)
  left_join(fao_cells) 

summary(cells_df)
## One issue of concern:
## Some regions aren't assigned and FAO region value (in places where land > 50% of cell cover)...and we need both for the analysis
#84218/259200

## check to see how many remain after we take out the land cells:

cells_df_water <- cells_df %>%
  filter(CellID %in% cells_water$CellID)
summary(cells_df_water)
#187/252083
# N= 187...no too bad...does not seem worth fretting over


## save

write.csv(cells_df_water, file = "globalprep/fis/v2017/cells.csv")
