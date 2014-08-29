#######################################################
## prepares data and runs cmsy
## to generate b/bmsy data
## Ended up going with the non-zero padding 
## (i.e., NA's after first recorded catch not replaced
## with zeros)
#######################################################
library(plyr)
library(dplyr)
library(parallel)

rm(list = ls())


####################################################
### Preparing the catch data to feed into the cmsy file:
####################################################
## Prepare data: Make it look like this:
#stock_id            res           ct   yr
#Ablennes hians_51   NA            27.05 1985
#Ablennes hians_51   NA            38.18 1990
#Ablennes hians_51   NA            54.10 1991
#...

#NOTES: 
# There needs to be >=7 years of data to run cmsy- however, the data 
# is subset to include 10 years of data with no zero or NA values.

######################################
# Antarctica zeroes included-----
cdat <- read.csv("Antarctica/AQ_FIS_2014a/data/CCAMLR_with0s_w_resil_Aug132014.csv")
cdat <- cdat %>%
  select(stock_id, ct, yr) %>%
  arrange(stock_id, yr)
cdat <- unique(cdat)

 #get ID's of taxa with >= 10 years of non-zero data:
cdat_years <- cdat %>%
  filter(ct!=0) %>%
  group_by(stock_id) %>%
  summarise(years=length(yr))

cdat10plus <- cdat_years$stock_id[cdat_years$years>=10]

# subset original data to include only the species with 10+ years
cdat <- cdat[cdat$stock_id %in% cdat10plus, ]

## Run bbmsy script
source('Global/FIS_Bbmsy/cmsy_constrained.R')

get_b_bmsy <- function(i){  
  test <- runCMSY(stockNumber=i, cdat=cdat)
  new <- data.frame(taxon_name=test[[1]],
                    b_bmsy=test[[2]],
                    year=test[[7]])
  return(new)
}

print(system.time({    
  r = mclapply(1:length(cdat10plus), get_b_bmsy, mc.cores=detectCores(), mc.preschedule=F) 
}))

r <- ldply(r)

write.csv(r, "Global/GL-AQ-FIS_v2013/tmp/b_bmsy_AQ_with_zeros_constrained.csv", row.names=FALSE)

## compared with old data and R2 was 1 - feel confident everything went well
# ## compare with old data (should be the same):
# old <- read.csv("Global/GL-AQ-FIS_v2013/tmp/fnk_fis_b_bmsy.csv")
# old <- old %>%
#   select(taxon_name, old_b_bmsy=b_bmsy, year) %>%
#   join(r, by=c("taxon_name", "year"))
# 
# plot(old_b_bmsy~b_bmsy, data=old)
# abline(0, 1)
# mod <- lm(old_b_bmsy~b_bmsy, data=old)
# summary(mod)
######################################
# Antarctica zeroes excluded-----

cdat <- read.csv("Antarctica/AQ_FIS_2014a/data/CCAMLR_no0s_w_resil.csv")
cdat <- cdat %>%
  select(stock_id, ct, yr) %>%
  arrange(stock_id, yr)
cdat <- unique(cdat)

#get ID's of taxa with >= 10 years of non-zero data:
cdat_years <- cdat %>%
  filter(ct!=0) %>%
  group_by(stock_id) %>%
  summarise(years=length(yr))

cdat10plus <- cdat_years$stock_id[cdat_years$years>=10]

# subset original data to include only the species with 10+ years
cdat <- cdat[cdat$stock_id %in% cdat10plus, ]

## Run bbmsy script
source('Global/FIS_Bbmsy/cmsy_constrained.R')

get_b_bmsy <- function(i){  
  test <- runCMSY(stockNumber=i, cdat=cdat)
  new <- data.frame(taxon_name=test[[1]],
                    b_bmsy=test[[2]],
                    year=test[[7]])
  return(new)
}

print(system.time({    
  r = mclapply(1:1:length(cdat10plus), get_b_bmsy, mc.cores=detectCores(), mc.preschedule=F) 
}))

r <- ldply(r)

write.csv(r, "Global/GL-AQ-FIS_v2013/tmp/b_bmsy_AQ_no_zeros_constrained.csv", row.names=FALSE)

#final data:
write.csv(r, "Global/GL-AQ-FIS_v2013/data/fnk_fis_b_bmsy.csv", row.names=FALSE)

## compare zeros/no zeros
noZ <- read.csv("Global/GL-AQ-FIS_v2013/tmp/b_bmsy_AQ_no_zeros_constrained.csv")
Z <- read.csv("Global/GL-AQ-FIS_v2013/tmp/b_bmsy_AQ_with_zeros_constrained.csv")

Compare <- noZ %>%
  select(taxon_name, noZ_b_bmsy=b_bmsy, year) %>%
  left_join(Z, by=c('taxon_name', 'year'))

ggplot(subset(Compare, year==2011), aes(x=noZ_b_bmsy, y=b_bmsy, color=taxon_name, group=taxon_name)) +
  geom_point() +
  geom_abline(intercept=0, slope=1) +
  theme_bw()+
  theme(legend.position="none") +
  labs(x="b/bmsy: no zeros", y="b/bmsy: zeroes included")

mod <- lm(b_bmsy ~ noZ_b_bmsy, data=Compare)

confint(mod)

