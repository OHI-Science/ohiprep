#### Exploring different scenarios for generating LSP data
#### We modified our methods somewhat because large regions were 
#### added to the recent WDPA data that didn't seem particularly
#### protective.  We now use a strategy where some of the regions are not
#### included in the analysis.

library(foreign) #read dbf files
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

### See issue: https://github.com/OHI-Science/issues/issues/387

### des: calculated using the old method where everything was included
### iucn: using iucn classification to eliminate some protected ares from analysis

## Prediction 1: 2014 to 2015 des: Big jumps from 2014 to 2015 
## (due to addition of these large protected areas that are not excluded in this scenario)
setwd("~/ohiprep")
file_path <- 'globalprep/WDPA_MPA/v2015/tmp/iucn_dbfs'

##########################################################
## comparing data within the latest dataset
##########################################################

# 2014 vs 2015
offshore_old <- read.csv(file.path(file_path, "rgn_offshore3nm_wdpa_2015des.dbf_summarized.csv"))
ggplot(offshore_old, aes(x=area_2014, y=area_2015))+
  geom_point(size=3) +
  scale_y_log10() +
  scale_x_log10() +
  geom_abline(slope=1, intercept=0, col="red") +
  labs(title="offshore: old (des)") +
  theme_bw()
offshore_old[offshore_old$rgn_id==163,]

offshore_new <- read.csv(file.path(file_path, "rgn_offshore3nm_wdpa_2015iucn.dbf_summarized.csv"))
ggplot(offshore_new, aes(x=area_2014, y=area_2015))+
  geom_point(size=3) +
  scale_y_log10() +
  scale_x_log10() +
  geom_abline(slope=1, intercept=0, col="red") +
  labs(title="offshore: new (iucn)") +
  theme_bw()
offshore_new[offshore_new$rgn_id==163,]



inland_old <- read.csv(file.path(file_path, "rgn_inland1km_wdpa_2015des.dbf_summarized.csv"))
ggplot(inland_old, aes(x=area_2014, y=area_2015))+
  geom_point(size=3) +
  scale_y_log10() +
  scale_x_log10() +
  geom_abline(slope=1, intercept=0, col="red") +
  labs(title="inland: old (des)") +
  theme_bw()



inland_new <- read.csv(file.path(file_path, "rgn_inland1km_wdpa_2015iucn.dbf_summarized.csv"))
ggplot(inland_new, aes(x=area_2014, y=area_2015))+
  geom_point(size=3) +
  scale_y_log10() +
  scale_x_log10() +
  geom_abline(slope=1, intercept=0, col="red") +
  labs(title="inland: new (iucn)")
  theme_bw()


## old method vs new method
offshore_year_compare <- offshore_new %>%
  select(rgn_id, area_2014_iucn=area_2014, area_2015_iucn=area_2015) %>%
  left_join(offshore_old)
ggplot(offshore_year_compare, aes(x=area_2015, y=area_2015_iucn))+
  geom_point(size=3) +
  scale_y_log10() +
  scale_x_log10() +
  geom_abline(slope=1, intercept=0, col="red") +
  labs(title="offshore", y="New method area, log", x="Old method area, log") +
  theme_bw()

inland_year_compare <- inland_new %>%
  select(rgn_id, area_2014_iucn=area_2014, area_2015_iucn=area_2015) %>%
  left_join(inland_old)
ggplot(inland_year_compare, aes(x=area_2015, y=area_2015_iucn))+
  geom_point(size=3) +
  scale_y_log10() +
  scale_x_log10() +
  geom_abline(slope=1, intercept=0, col="red") +
  labs(title = "inland", y="New method area, log", x="Old method area, log") +
  theme_bw()

##########################################################
## comparing data with last data (year 2014 only)
##########################################################
old_data_2014_offshore <- read.csv("Global/WDPA-MPA_v2014/data/lsp_protarea_offshore3nm.csv")
old_data_2014_offshore <- old_data_2014_offshore %>%
  arrange(rgn_id, year) %>%
  group_by(rgn_id) %>%
  mutate(OHI2014analysis=cumsum(area_km2)) %>%
  filter(year==max(year)) %>%
  select(rgn_id, OHI2014analysis)
offshore_old  <-  offshore_old %>%
  left_join(old_data_2014_offshore)

ggplot(offshore_old, aes(x=OHI2014analysis, y=area_2014))+
  geom_point(size=3) +
  scale_y_log10() +
  scale_x_log10() +
  geom_abline(slope=1, intercept=0, col="red") +
  labs(title = "Offshore: All pa included", y="Newly analyzed 2014 data") +
  theme_bw()

offshore_new  <-  offshore_new %>%
  left_join(old_data_2014_offshore)

ggplot(offshore_new, aes(x=OHI2014analysis, y=area_2014))+
  geom_point(size=3) +
  scale_y_log10() +
  scale_x_log10() +
  geom_abline(slope=1, intercept=0, col="red") +
  labs(title = "Offshore: IUCN categories V and VI excluded", y="Newly analyzed 2014 data") +
  theme_bw()

offshore_new[offshore_new$area_2014==0,]


#########################################################
## creating data
## probably only needs to be done once
#########################################################

#files <- list.files(file_path)
files <- c("rgn_inland1km_wdpa_2015des.dbf", "rgn_inland1km_wdpa_2015iucn.dbf", "rgn_offshore3nm_wdpa_2015des.dbf", "rgn_offshore3nm_wdpa_2015iucn.dbf")

## Compare different datasets:
for (i in files){  
#  i <- 1
  data_file <- i
  
  d = read.dbf(file.path(file_path, data_file))
  
  m <- gather(d, year, area_m2, VALUE_0:VALUE_2014)
  
  m <- m %>%
    mutate(year=as.integer(gsub('VALUE_', '', year))) %>%
    mutate(area_km2 = area_m2/(1000*1000)) %>%      
    select(rgn_id=VALUE, year, area_km2) %>%
    arrange(rgn_id, year)
  
  # fix  for  lsp_protarea_offshore3nm.csv: make any non-represented rgn_id == 0, and 2 special fixes. 
  # see https://github.com/OHI-Science/ohidev/blob/master/report/compare_scores2layers/compare_scores2layers.md#lasting-special-places
  length(table(m$rgn_id)) #not all regions had data, need to add those
  
  rgns = read.csv('../ohi-global/eez2014/layers/rgn_global.csv') %>%
    select(rgn_id)  %>%
    filter(rgn_id < 255) %>%
    arrange(rgn_id)
  
  rgns <- expand.grid(rgn_id=rgns$rgn_id, year=unique(m$year), area_km2=0)
  
  # Here we might want to add in all the year data and not just the max year - use expand.grid for this.
  m <- rbind(m, 
             rgns %>%
               anti_join(m, by = 'rgn_id')) 
  
  n_2015 <- m %>%
    arrange(rgn_id, year) %>%
    group_by(rgn_id) %>%
    mutate(area_2015=cumsum(area_km2)) %>%
    ungroup() %>%
    filter(year==max(year)) %>%
    select(rgn_id, area_2015)

  n_2014 <- m %>%
    arrange(rgn_id, year) %>%
    group_by(rgn_id) %>%
    mutate(area_2014=cumsum(area_km2)) %>%
    ungroup() %>%
    filter(year==max(year)-1) %>%
    select(rgn_id, area_2014) %>%
    left_join(n_2015)
  
  
  # save layer
  write.csv(n_2014, file.path(file_path, paste(data_file, 'summarized.csv', sep="_")), row.names=F, na='')
   
}



###############################################
## 4/10/2016
## Comparing latest extraction of data
## 2014 data from this year vs. last year
###############################################

# country names
c_names <- read.csv("../ohi-global/eez2014/layers/rgn_labels.csv")

data %>%
  filter(total_old_off==0 & total_new_off>0)
data %>%
  filter(total_new_off==0 & total_old_off>0)

off_changes <- c('Kerguelen Islands',   'Crozet Islands',
                 'Amsterdam Island and Saint Paul Island',
                 'Haiti',   'Djibouti',   'Christmas Island',
                 'Sierra Leone', 'Tunisia', 'Sint Maarten',
                 'Saint Vincent and the Grenadines',
                 'Glorioso Islands',  'Micronesia',
                 'Maldives',  'Yemen',  'Tristan da Cunha',
                 'Palau',  'Yemen',   'New Caledonia',
                 'Northern Saint−Martin')

changes <- data.frame(label= off_changes, 
                      off_label = off_changes)

in_changes <- c('Kerguelen Islands',   'Crozet Islands',
                 'Amsterdam Island and Saint Paul Island',
                 'New Caledonia')

changes_in <- data.frame(label= in_changes, 
                      in_label = in_changes)


makeData  <- function(dataSet){
dataSet %>%
  arrange(rgn_id, year) %>%
  group_by(rgn_id) %>%
  mutate(area_km2_total = cumsum(area_km2)) %>%
  filter(year == max(year))%>%
  select(rgn_id, area_km2_total) %>%
  ungroup()
}

old_off <- read.csv("Global/WDPA-MPA_v2014/data/lsp_protarea_offshore3nm.csv")
old_off <- makeData(dataSet = old_off) %>%
  select(rgn_id, total_old_off = area_km2_total)

old_in <- read.csv("Global/WDPA-MPA_v2014/data/lsp_protarea_inland1km.csv")
old_in <- makeData(dataSet = old_in) %>%
  select(rgn_id, total_old_in = area_km2_total)

makeData_new <- function(dataSet){
  dataSet %>%
    arrange(rgn_id, year) %>%
    group_by(rgn_id) %>%
    mutate(area_km2_total = cumsum(area_km2)) %>%
    filter(year==2014) %>%
    select(rgn_id, area_km2_total) %>%
    ungroup()
}

new_off <- read.csv("globalprep/WDPA_MPA/v2015/data/lsp_protarea_offshore3nm.csv")
new_off <- makeData_new(dataSet=new_off) %>%
  select(rgn_id, total_new_off = area_km2_total)

new_in <- read.csv("globalprep/WDPA_MPA/v2015/data/lsp_protarea_inland1km.csv")
new_in <- makeData_new(dataSet=new_in) %>%
  select(rgn_id, total_new_in = area_km2_total)


data <- old_off %>%
  left_join(new_off) %>%
  left_join(old_in) %>%
  left_join(new_in) %>%
  left_join(c_names) %>%
  left_join(changes) %>%
  left_join(changes_in)


ggplot(data, aes(x=total_old_off+1, y=total_new_off+1))+
  geom_point(size=3, shape=19) +
  scale_y_log10() +
  scale_x_log10() +
  geom_abline(slope=1, intercept=0, col="red") +
  labs(title = "Offshore") +
  geom_text(aes(label=off_label), hjust=0, vjust=-1, size=2.5, angle=30) + 
  theme_bw()

ggsave("globalprep/WDPA_MPA/v2015/offshore.pdf")


ggplot(data, aes(x=total_old_in+1, y=total_new_in+1))+
  geom_point(size=3, shape=19) +
  scale_y_log10() +
  scale_x_log10() +
  geom_abline(slope=1, intercept=0, col="red") +
  labs(title = "Inland") +
  geom_text(aes(label=in_label), hjust=0, vjust=-1, size=2.5, angle=30) + 
  theme_bw()

ggsave("globalprep/WDPA_MPA/v2015/inland.pdf")


