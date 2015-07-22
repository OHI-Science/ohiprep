##############################################
## Calculating b/bmsy scores for each species
## MRF: June 16 2015
##############################################

## source b/bmsy script
# library(devtools)
# devtools::install('../datalimited/datalimited')
library(datalimited)
library(tidyr)


## source path directories
source('src/R/common.R')


## read in data (catch data summarized by FAO region)
data <- read.csv('globalprep/SAUP_FIS/v2015/tmp/b_bmsy_v16072015.csv')

################################################
## constrained prior calculations ----
################################################

stocks <- unique(data$stock_id)
#stocks <- unique(data$stock_id)[which(stocks=="601250_41"):length(stocks)]  # if gets interrupted in between time.

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

print(stock)
print(which(stocks==stock))

}

#checks:
length(unique(b_bmsy_data$stock_id))  #should be N=2684
tmp <- b_bmsy_data %>%
  group_by(stock_id) %>%
  summarize(N=length(stock_id))
hist(tmp$N)
max(tmp$N) #nothing should be over 31 in this case

write.csv(b_bmsy_data, "globalprep/SAUP_FIS/v2015/tmp/b_bmsy_scores_constrained.csv", row.names=FALSE)


################################################
## uniform prior calculations ----
################################################

stocks <- unique(data$stock_id)
#stocks <- unique(data$stock_id)[which(stocks=="601250_41"):length(stocks)]  # if gets interrupted in between time.


b_bmsy_data_uniform <- data.frame()

for(stock in stocks) { #stock <- "600004_87" stock <- "601250_41"
  subset <- data[data$stock_id %in% stock, ]
  x <- cmsy(subset$Year, ct = subset$catch, finalbio=c(0.01, 0.7))
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
  bbmsy_out$prior <- "uniform"
  b_bmsy_data_uniform <- rbind(b_bmsy_data_uniform, bbmsy_out)
print(stock)
print(which(stocks==stock))
}

#checks:
length(unique(b_bmsy_data_uniform$stock_id))  #should be N=2684
tmp <- b_bmsy_data_uniform %>%
  group_by(stock_id) %>%
  summarize(N=length(stock_id))
hist(tmp$N)
max(tmp$N) #nothing should be over 31 in this case and nothing less than 10

write.csv(b_bmsy_data_uniform, "globalprep/SAUP_FIS/v2015/tmp/b_bmsy_scores_uniform.csv", row.names=FALSE)


# -------------------------------------------------------------------
## Taking the 5 year running average of b/bmsy values to smooth data
# -------------------------------------------------------------------
library(zoo)
constrained <- read.csv('globalprep/SAUP_FIS/v2015/tmp/b_bmsy_scores_constrained.csv')
uniform <- read.csv('globalprep/SAUP_FIS/v2015/tmp/b_bmsy_scores_uniform.csv')

new_b_bmsy <- function(b_bmsy=constrained){
  b_bmsy <- b_bmsy %>%
    select(stock_id, year, bbmsy_mean, prior) %>%
    arrange(stock_id, year) %>%
    group_by(stock_id) %>%
    mutate(mean_5year = rollmean(bbmsy_mean, 5, align="right", fill=NA))
  write.csv(b_bmsy, sprintf('globalprep/SAUP_FIS/v2015/tmp/b_bmsy_scores_%s_mean5yrs.csv', unique(b_bmsy$prior)), row.names=FALSE)
} 

new_b_bmsy(constrained)
new_b_bmsy(uniform)

constrained_mean <- read.csv('globalprep/SAUP_FIS/v2015/tmp/b_bmsy_scores_constrained_mean5yrs.csv')
uniform_mean <- read.csv('globalprep/SAUP_FIS/v2015/tmp/b_bmsy_scores_uniform_mean5yrs.csv')
head(uniform_mean)

###############################################################3
### Exploring b/bmsy results ----
################################################################
library(ggplot2)

constrained <- read.csv("globalprep/SAUP_FIS/v2015/tmp/b_bmsy_scores_constrained.csv") %>%
  select(stock_id, year, bbmsy_mean_constrained=bbmsy_mean)
uniform <- read.csv("globalprep/SAUP_FIS/v2015/tmp/b_bmsy_scores_uniform.csv")%>%
  select(stock_id, year, bbmsy_mean_uniform=bbmsy_mean)
names <- read.csv(file.path(dir_neptune_data, 'git-annex/globalprep/SAUP_FIS_data/v2015/raw/ohi_taxon.csv'))

catch <- data %>%
  left_join(constrained, by=c('stock_id', "Year"="year")) %>%
  left_join(uniform, by=c('stock_id', "Year"="year")) %>%
  left_join(names, by=c('TaxonKey'='taxonkey')) %>%
  group_by(stock_id) %>%
  mutate(catch_prop = catch/max(catch)) %>%
  ungroup() %>%
  mutate(stock_id_name = paste(stock_id, common.name, sep="_")) %>%
  arrange(FAOAreaID, stock_id, Year) %>%
  select(stock_id_name, Year, catch_prop, bbmsy_mean_constrained, bbmsy_mean_uniform)

ggplot(catch, aes(x=bbmsy_mean_constrained, y=bbmsy_mean_uniform)) +
  geom_point(shape=19) +
  geom_vline(xintercept=1, col="red", linetype=2) +
  geom_hline(yintercept=1, col="red", linetype=2) + 
  geom_abline(slope=1, intercept=0, col="orange") +
  theme_bw()


catch <- gather(catch, "variable", "value", catch_prop:bbmsy_mean_uniform)
stock_id <- unique(catch$stock_id_name)
small <- c(1, 1 + seq(100, length(stock_id), by=100)) 
large <- c(seq(100, length(stock_id), by=100), length(stock_id))


for(i in 1:length(small)){
ggplot(subset(catch, stock_id_name %in% stock_id[small[i]:large[i]]), 
       aes(x=Year, y=value, group=variable, color=variable)) +
  geom_point(size=1) +
  geom_line()+
  facet_wrap( ~ stock_id_name, nrow=10, scale='free')+
  theme_bw() +
  theme(strip.text = element_text(size=rel(.55)), 
        axis.text = element_text(size=rel(0.7)))
ggsave(width=17, height=10, units="in", 
       sprintf("globalprep/SAUP_FIS/v2015/tmp/bbmsy_figs/Bmsy_vs_catch_%s_%s.png", small[i], large[i]))
}