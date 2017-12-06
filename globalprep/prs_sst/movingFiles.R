##### Moving intermediate SLR rasters to Mazu (getting too large for Github)

source("src/R/common.R")

## 2016 
tmp <- list.files("globalprep/prs_sst/v2016/int", pattern = "tif", full=TRUE)
file.copy(tmp, file.path(dir_M, "git-annex/globalprep/prs_sst/v2016/int"))

