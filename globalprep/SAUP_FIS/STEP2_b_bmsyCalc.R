##############################################
## Calculating b/bmsy scores for each species
## MRF: June 16 2015
##############################################

## source b/bmsy script
library(devtools)
#devtools::install('../datalimited/datalimited')
library(datalimited)


## source path directories
source('src/R/common.R')


## read in data
data <- read.csv(file.path(dir_neptune_data, 'git-annex/globalprep/SAUP_data_2015/tmp/b_bmsy_11062015.csv'))
data$stock_id <- paste(data$TaxonKey, data$FAOAreaID, sep = "_")


stocks <- unique(data$stock_id)
stocks <- unique(data$stock_id)[which(stocks=="601250_41"):length(stocks)]


b_bmsy_data <- data.frame()

for(stock in stocks) { #stock <- "600004_87" stock <- "601250_41"
subset <- data[data$stock_id %in% stock, ]
x <- cmsy(subset$Year, ct = subset$catch)
# par(mfrow = c(2, 2))
# plot(subset$Year, subset$catch, type = "o", xlab = "Year", ylab = "Catch (t)")
# plot(subset$Year,  apply(x$biomass, 2, median)[-1], type = "o",
#      ylab = "Estimated biomass", xlab = "Year")
# hist(x$bmsy)
# plot(x$theta$r, x$theta$k)

bbmsy <- x$biomass[, -1] / x$bmsy
bbmsy_out <- summarize_bbmsy(bbmsy, log=FALSE)
bbmsy_out$year <- subset$Year
bbmsy_out$stock_id <- stock
bbmsy_out$prior <- "constrained"
b_bmsy_data <- rbind(b_bmsy_data, bbmsy_out)

}

#checks:
length(unique(b_bmsy_data$stock_id))  #should be N=2706
tmp <- b_bmsy_data %>%
  group_by(stock_id) %>%
  summarize(N=length(stock_id))
hist(tmp$N)
max(tmp$N) #nothing should be over 31 in this case

write.csv(b_bmsy_data, file.path(dir_neptune_data, 'git-annex/globalprep/SAUP_data_2015/tmp/b_bmsy_scores_constrained.csv'), row.names=FALSE)
