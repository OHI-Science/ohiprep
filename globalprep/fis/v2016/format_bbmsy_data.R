##############################################
## This script is used to do the final B/Bmsy
## calculations after
## generating the initial B/Bmsy values
## using the functions in the R datalimited
## package
##############################################

library(zoo)
library(tidyr)
library(dplyr)

#### Ultimately, there will be three B/Bmsy values calculated using different methods
### These will be averaged.  But, in the meantime, I will do the b/bmsy values
### estimated using CMSY methods

# -------------------------------------------------------------------
## Taking the 5 year running average of b/bmsy values to smooth data
# -------------------------------------------------------------------

constrained <- read.csv('globalprep/fis/v2016/int/cmsy_bbmsy.csv') %>%
  mutate(prior = 'constrained') %>%
  filter(!is.na(bbmsy_mean))
uniform <- read.csv('globalprep/fis/v2016/int/cmsy_bbmsy_uni_prior.csv') %>%
  mutate(prior = "uniform") %>%
  filter(!is.na(bbmsy_mean))

new_b_bmsy <- function(b_bmsy=constrained){
  b_bmsy <- b_bmsy %>%
    dplyr::select(stock_id, year, bbmsy_mean, prior, model) %>%
    arrange(stock_id, year) %>%
    group_by(stock_id) %>%
    mutate(mean_5year = rollmean(bbmsy_mean, 5, align="right", fill=NA))
  write.csv(b_bmsy, sprintf('globalprep/fis/v2016/int/cmsy_b_bmsy_%s_mean5yrs.csv', unique(b_bmsy$prior)), row.names=FALSE)
} 

new_b_bmsy(constrained)
new_b_bmsy(uniform)



  #--------------------------------------------------------------------
  ## CMSY b/bmsy data selection based on stock resilience scores
  # B/bmsy data is generated using different models 
  # depending on the resilience score of the stock
  # (calculated in 2015 and no updates to data)
  # -------------------------------------------------------------------
  
  # resilience scores to select the appropriate b/bmsy 
  res <- read.csv("globalprep/fis/v2015/tmp/stock_resil_06cutoff_2015.csv")


  b_bmsy_uniform <- read.csv('globalprep/fis/v2016/int/cmsy_b_bmsy_uniform_mean5yrs.csv')
  b_bmsy_uniform <- b_bmsy_uniform %>%
    select(stock_id, year, b_bmsy_uniform=mean_5year) 
  
  b_bmsy_constrained <- read.csv('globalprep/fis/v2016/int/cmsy_b_bmsy_constrained_mean5yrs.csv')
  b_bmsy_constrained <- b_bmsy_constrained %>%
    select(stock_id, year, b_bmsy_constrained=mean_5year) 
  
  setdiff(res$stock_id, b_bmsy_constrained$stock_id)
  setdiff(b_bmsy_constrained$stock_id, res$stock_id)
  setdiff(res$stock_id, b_bmsy_uniform$stock_id)
  setdiff(b_bmsy_uniform$stock_id, res$stock_id)
  
  bmsy <- b_bmsy_uniform %>%
    left_join(b_bmsy_constrained, by=c("stock_id", "year")) %>%
    left_join(res, by="stock_id")
  
  bmsy <- bmsy %>%
    mutate(b_bmsy = ifelse(unif_prior==1, b_bmsy_uniform, b_bmsy_constrained)) 
  
  bmsy <- separate(bmsy, stock_id, c("TaxonKey", "fao_id")) %>%
    mutate(TaxonKey = as.integer(TaxonKey),
           fao_id = as.integer(fao_id)) %>%
    dplyr::select(TaxonKey, fao_id, year, b_bmsy); head(bmsy)
  
  #--------------------------------------------------------------------
  #### getting b/bmsy data to teh correct spatial scale
  #-----------------------------------------------------------------------
  catch <- read.csv('globalprep/SAUP_FIS/v2015/data/mean_catch.csv')
  catch <- separate(catch, fao_ohi_id, c("fao_id", "rgn_id")) %>%
    mutate(fao_id = as.numeric(fao_id)) %>%
    mutate(rgn_id = as.numeric(rgn_id))
  catch <- separate(catch, taxon_name_key, c("TaxonName", "TaxonKey"), sep="_") %>%
    mutate(TaxonKey = as.numeric(TaxonKey)) 
  
  catch <- select(catch, TaxonKey, fao_id, rgn_id, year); head(catch); summary(catch)
  
  dim(unique(catch))
  
  bmsy_fao_rgn <- catch %>%
    left_join(bmsy) %>%
    filter(!is.na(b_bmsy))
  head(bmsy_fao_rgn)
  summary(bmsy_fao_rgn)
  
  # just checking things out
  # NA catch data: non-species catch, species with < 10 years non-zero data
  summary(bmsy_fao_rgn[is.na(bmsy_fao_rgn$b_bmsy), ])
  bmsy_fao_rgn[is.na(bmsy_fao_rgn$b_bmsy) & bmsy_fao_rgn$TaxonKey>=600000, ]
  
  source('../ohiprep/src/R/common.R') # set dir_neptune_data
  data <- read.csv(file.path(dir_neptune_data, 'git-annex/globalprep/SAUP_FIS_data/v2015/tmp/Catch_v16072015_summary.csv')) 
  data[data$TaxonKey == 690177 & data$FAOAreaID == 71, ] #rgn_id=7 is saup_id=90 
  
  filter(bmsy_fao_rgn, fao_id==37 & TaxonKey==600030 & year==2006) #should all have the same value (and they do)
  ###
  
  # -------------------------------------------------------------------
  ### Read in RAM data and replace cmsy data where possible
  # -----------------------------------------------------------------
  ram <- read.csv('globalprep/SAUP_FIS/v2015/tmp/RAM_fao_ohi.csv') %>%
    select(TaxonKey=Taxonid, fao_id=FAO_rgn, rgn_id=ohi_rgn, year, ram_b_bmsy=bbmsy) %>%
    filter(!is.na(ram_b_bmsy)); head(ram); summary(ram)
  
  bmsy_final <- bmsy_fao_rgn %>%
    left_join(ram)
  
  sum(!is.na(bmsy_final$ram_b_bmsy)) #9368/83679, about 11% of Taxon/fao/rgn records ram b/bmsy data 
  
  bmsy_final$bbmsy <- ifelse(is.na(bmsy_final$ram_b_bmsy), bmsy_final$b_bmsy, bmsy_final$ram_b_bmsy) 
  bmsy_final <- bmsy_final %>%
    mutate(fao_ohi_id = paste(fao_id, rgn_id, sep='_')) %>%
    select(fao_ohi_id, taxonkey=TaxonKey, year, b_bmsy=bbmsy); head(bmsy_final); summary(bmsy_final)
  filter(bmsy_final, fao_ohi_id=='71_13', TaxonKey==600107)
  
  write.csv(bmsy_final, 'globalprep/SAUP_FIS/v2015/data/fnk_fis_b_bmsy_lyr.csv', row.names=F, na='')
  
  #### Check against catch data
  catch <-  read.csv('globalprep/SAUP_FIS/v2015/data/mean_catch.csv'); head(catch)
  catch <- separate(catch, taxon_name_key, c("TaxonName", "TaxonKey"), sep="_")
  catch$stock_ohi_id <- paste(catch$TaxonKey, catch$fao_ohi_id, sep="_")
  
  bmsy_final$stock_ohi_id <- paste(bmsy_final$TaxonKey, bmsy_final$fao_ohi_id, sep="_") 
  setdiff(bmsy_final$stock_ohi_id, catch$stock_ohi_id)  #should all be in there...and they are...
  
}


###############################################################3
### Exploring b/bmsy results ----
################################################################
library(ggplot2)

constrained <- read.csv("globalprep/fis/v2016/int/cmsy_b_bmsy.csv") %>%
  select(stock_id, year, bbmsy_mean_constrained=bbmsy_mean)
uniform <- read.csv("globalprep/fis/v2016/int/cmys_b_bmsy_uni_prior.csv")%>%
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
  
  