##########################################
## B/bmsy calculations: testing phase
## purpose is to develop a script to 
## test b/bmsy values.  These are test values
## because we are expecting the data to be
## updated.
##########################################

library(devtools)

## source path directories
source('src/R/common.R')

## source b/bmsy script
devtools::install('../datalimited/datalimited')
library(datalimited)

# explore general calculation
?cmsy
load('~/datalimited/data/blue_gren.rda')
x <- cmsy(blue_gren$yr, ct = blue_gren$ct, reps = 2e4)
# > names(x)
# [1] "theta"       "biomass"     "bmsy"        "msy"         "mean_ln_msy"

par(mfrow = c(2, 2))
plot(blue_gren$yr, blue_gren$ct, type = "o", xlab = "Year", ylab = "Catch (t)")
plot(blue_gren$yr,  apply(x$biomass, 2, median)[-1], type = "o",
     ylab = "Estimated biomass", xlab = "Year")
hist(x$bmsy)
plot(x$theta$r, x$theta$k)
bbmsy <- x$biomass[, -1] / x$bmsy
bbmsy_out <- summarize_bbmsy(bbmsy, log=FALSE)
bbmsy_out$year <- blue_gren$yr
library("ggplot2")
ggplot(bbmsy_out, aes(year, bbmsy_q50)) + geom_line()  +
  geom_ribbon(aes(ymin = bbmsy_q25, ymax = bbmsy_q75), alpha = 0.2) +
  geom_ribbon(aes(ymin = bbmsy_q2.5, ymax = bbmsy_q97.5), alpha = 0.1) +
  geom_hline(yintercept = 1, lty = 2)


##### preparing our data for b/bmsy calculations:

# saup data
saup_catch <- read.csv(file.path(dir_neptune_data, "git-annex/globalprep/SAUP_data_2015/raw/Catch_Value_11062015.csv"), 
                                 header=FALSE)
names(saup_catch) <- c("EEZID", "FAOAreaID", "Year", "TaxonKey", "CatchAmount", "Value")
saup_catch$mergeID <- paste(saup_catch$EEZID, saup_catch$FAOAreaID, 
                            saup_catch$Year, saup_catch$TaxonKey, sep="_")
dups <- duplicated(saup_catch$mergeID)
saup_catch_dups <- saup_catch[dups, ]
