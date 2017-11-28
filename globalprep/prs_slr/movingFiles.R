##### Moving intermediate SLR rasters to Mazu (getting too large for Github)

source("src/R/common.R")

## 2016 
tmp <- list.files("globalprep/prs_slr/v2016/int", pattern = "tif", full=TRUE)
file.copy(tmp, file.path(dir_M, "git-annex/globalprep/prs_slr/v2016/int"))

tmp <- list.files("globalprep/prs_slr/v2016/int/msla_annual_mean", pattern = "tif", full=TRUE)
file.copy(tmp, file.path(dir_M, "git-annex/globalprep/prs_slr/v2016/int/msla_annual_mean"))

tmp <- list.files("globalprep/prs_slr/v2016/int/msla_monthly_coast", pattern = "tif", full=TRUE)
file.copy(tmp, file.path(dir_M, "git-annex/globalprep/prs_slr/v2016/int/msla_monthly_coast"))

## 2017 

tmp <- list.files("globalprep/prs_slr/v2017/int/msla_annual_mean", pattern = "tif", full=TRUE)
file.copy(tmp, file.path(dir_M, "git-annex/globalprep/prs_slr/v2017/int/msla_annual_mean"))

tmp <- list.files("globalprep/prs_slr/v2017/int/msla_monthly_coast", pattern = "tif", full=TRUE)
file.copy(tmp, file.path(dir_M, "git-annex/globalprep/prs_slr/v2017/int/msla_monthly_coast"))
