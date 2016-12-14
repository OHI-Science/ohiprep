## Goal:
##   Compare: 
##     - xx uniform prior with zeros (last OHI analysis, data is on file.path(dir_neptune_data, "model/GL-NCEAS-FIS_2014a/raw/cmsy.ohi.df_Jul292014.csv")
##     - xx uniform prior no zeros 
##     - original prior with zeros
##     - original prior no zeros

##  SOP: 1. format each file using the below code, save to the designated file location 
##       2. run OHI2013 and OHI2012 models 
##       3. save files to FIS_B_bmsy/Hex outputs
##       4. compare data files


library(tidyr)
library(ggplot2)
library(zoo)
source('../ohiprep/src/R/common.R') # set dir_neptune_data

data = file.path(dir_neptune_data, "model/GL-NCEAS-FIS_2014a")
dir_d = '../ohiprep/Global/NCEAS-Fisheries_2014a' 



###############################################
## B-Bmsy data----
###############################################
## original run, uniform with zeros replacing NA values
#b_bmsy <- read.csv(file.path(data, "raw/cmsy.ohi.df_Jul292014.csv"), na.strings='')

# load("Global/FIS_Bbmsy/Hex outputs/cmsy_ohi_results_table_uniformPrior_no0s.RData")
# b_bmsy <- cmsy.ohi.unif.no0.df
 
# load("Global/FIS_Bbmsy/Hex outputs/cmsy_ohi_results_table_originalPrio_added0s.RData")
# b_bmsy <- cmsy.ohi.orig.with0.df

# load("Global/FIS_Bbmsy/Hex outputs/cmsy_ohi_results_table_originalPrio_no0s.RData")
# b_bmsy <- cmsy.ohi.orig.no0.df

b_bmsy_lyr <- b_bmsy %>%
  mutate(fao_id = sapply(strsplit(as.character(stock_id), "_"), function(x)x[2]),
         taxon_name = sapply(strsplit(as.character(stock_id), "_"), function(x)x[1])) %>%
  select(fao_id, taxon_name, year=yr, b_bmsy)

write.csv(b_bmsy_lyr, file.path(dir_d, 'data/fnk_fis_b_bmsy_lyr.csv'), row.names=F, na='')

###############################################
## B-Bmsy data - rolling mean for each taxa ----
###############################################

new_b_bmsy <- function(b_bmsy=b_bmsy){
  b_bmsy <- b_bmsy %>%
    select(stock_id, yr, b_bmsy) %>%
    arrange(stock_id, yr)
  i='Ablennes hians_51'
  b_bmsy=b_bmsy_u_0
  runningMean <- lapply(unique(b_bmsy$stock_id), function(i){
    tmp <- zoo(b_bmsy$b_bmsy[b_bmsy$stock_id %in% i], b_bmsy$yr[b_bmsy$stock_id %in% i])
    tmp2 <- data.frame(rollmean(tmp, 5, align="right"))
    tmp3  <- data.frame(stock_id=i, yr=row.names(tmp2), b_bmsy=tmp2$rollmean.tmp..5..align....right..)
    tmp3
  }
  )
  runningMean_df <- ldply(runningMean)
  runningMean_df
}


## original run, uniform with zeros replacing NA values
b_bmsy_u_0 <- read.csv(file.path(data, "raw/cmsy.ohi.df_Jul292014.csv"), na.strings='')
new_data <- new_b_bmsy(b_bmsy_u_0)
# check sample size, lose 4 years for each stock_id
length(unique(b_bmsy$stock_id))*4  #7652
86937-79285  #7652
## excellent..it works out....
write.csv(new_data, file.path(dir_d, 'data/fnk_fis_b_bmsy_lyr_uniform_w0_runningMean.csv'), row.names=F, na='')


load("Global/FIS_Bbmsy/Hex outputs/cmsy_ohi_results_table_uniformPrior_no0s.RData")
b_bmsy_u_na <- cmsy.ohi.unif.no0.df
new_data <- new_b_bmsy(b_bmsy_u_na)
write.csv(new_data, file.path(dir_d, 'data/fnk_fis_b_bmsy_lyr_uniform_no0_runningMean.csv'), row.names=F, na='')

load("Global/FIS_Bbmsy/Hex outputs/cmsy_ohi_results_table_originalPrio_added0s.RData")
b_bmsy_c_0 <- cmsy.ohi.orig.with0.df
new_data <- new_b_bmsy(b_bmsy_c_0)
write.csv(new_data, file.path(dir_d, 'data/fnk_fis_b_bmsy_lyr_constrained_w0_runningMean.csv'), row.names=F, na='')

load("Global/FIS_Bbmsy/Hex outputs/cmsy_ohi_results_table_originalPrio_no0s.RData")
b_bmsy_c_na <- cmsy.ohi.orig.no0.df
new_data <- new_b_bmsy(b_bmsy_c_na)
write.csv(new_data, file.path(dir_d, 'data/fnk_fis_b_bmsy_lyr_constrained_no0_runningMean.csv'), row.names=F, na='')

###############################################
## Visualizing differences----
###############################################
list <- grep("EEZ", list.files("Global/FIS_Bbmsy/Hex outputs"), value=TRUE)
all.data <- lapply(list, function(i){
  #x <- list[1]  
  tmp <- read.csv(file.path("Global/FIS_Bbmsy/Hex outputs", i))
  tmp$model <- sapply(strsplit(i, "_"), function(x)x[4])
  tmp$zeros <- sapply(strsplit(i, "_"), function(x)x[5])
  tmp$zeros <- gsub(".csv", "", tmp$zeros)
  tmp$scenario <- sapply(strsplit(i, "_"), function(x)x[1])
  tmp
})

data <- ldply(all.data)

data  <- data %>%
  filter(goal=="FIS",
         dimension=="score",
         region_id !=0)

mod <- lm(score ~ model*scenario, data=data)

compareScenario <- data %>%
  mutate(model=as.factor(model),
         zeros=as.factor(zeros)) %>%
  group_by(region_id, model, zeros) %>%
  summarize(diff_2013minus2012 = score[scenario=='EEZ2013'] - score[scenario=='EEZ2012'])


### compare distributions of models

compareModel <- spread(data, model, score)

ggplot(subset(compareModel, scenario=="EEZ2013"), aes(x=constrained, y=uniform, color=zeros, group=zeros)) +
  geom_point(size=3) +
  geom_abline(intercept=0, slope=1) +
  theme_bw()



ggplot(subset(compareScenario, model=="uniform" & zeros=="w0s"), aes(x=diff_2013minus2012) ) +
  geom_histogram(fill="grey", color="darkgray")+
  labs(title="uniform, with 0s")

ggplot(subset(compareScenario, model=="uniform" & zeros=="no0s"), aes(x=diff_2013minus2012) ) +
  geom_histogram(fill="grey", color="darkgray")+
  labs(title="uniform, no added 0s")

ggplot(subset(compareScenario, model=="constrained" & zeros=="w0s"), aes(x=diff_2013minus2012) ) +
  geom_histogram(fill="grey", color="darkgray")+
  labs(title="constrained, with 0s")

ggplot(subset(compareScenario, model=="constrained" & zeros=="no0s"), aes(x=diff_2013minus2012) ) +
  geom_histogram(fill="grey", color="darkgray")+
  labs(title="constrained, no added 0s")


compareZeros <- spread(data, zeros, score)

ggplot(subset(compareZeros, scenario=="EEZ2013"), aes(x=no0s, y=w0s, color=model, group=model)) +
  geom_point(size=3) +
  geom_abline(intercept=0, slope=1) +
  theme_bw()


data <- ldply(all.data)

data  <- data %>%
  filter(goal=="FIS",
         dimension=="status",
         region_id !=0,
         zeros=="w0s",
         scenario=="EEZ2013") %>%
  mutate(model=as.factor(model),
         zeros=as.factor(zeros),
         scenario=as.factor(scenario))

ggplot(subset(data, model=="constrained"), aes(x=score)) +
  geom_histogram(fill="gray", color="darkgray") +
  theme_bw() +
  labs(x="FIS status", title="Constrained, EEZ2013, with 0s")


ggplot(subset(data, model=="uniform"), aes(x=score)) +
  geom_histogram(fill="gray", color="darkgray") +
  theme_bw() +
  labs(x="FIS status", title="Constrained, EEZ2013, with 0s")


#### comparing how zeros in catch weigth data affect final scores

data <- ldply(all.data)

data  <- data %>%
  filter(goal=="FIS",
         dimension=="score",
         region_id !=0,
         model=="constrained",
         scenario=="EEZ2013") %>%
  mutate(model=as.factor(model),
         zeros=as.factor(zeros),
         scenario=as.factor(scenario))

compareModel <- spread(data, zeros, score)

ggplot(compareModel, aes(x=no0s, y=w0sCatch0s)) +
  geom_point(size=3) +
  geom_abline(intercept=0, slope=1) +
  theme_bw()

ggplot(compareModel, aes(x=w0s, y=w0sCatch0s)) +
  geom_point(size=3) +
  geom_abline(intercept=0, slope=1) +
  theme_bw()

ggplot(compareModel, aes(x=no0s, y=w0s)) +
  geom_point(size=3) +
  geom_abline(intercept=0, slope=1) +
  theme_bw()

## model run with zeros added to mean catch data as well (comparing how that effects final results):

dir_d = '../ohiprep/Global/NCEAS-Fisheries_2014a' # set folder where files are saved
eez_data  <- read.csv(file.path(dir_d, "tmp/saup_fao_eez_ids.csv")) %>%
  select(region_id=eez_id, fao_id)
eez_data <- unique(eez_data)  

compareModel  <- compareModel %>%
  left_join(eez_data)
dups <- compareModel$region_id[duplicated(compareModel$region_id, values=TRUE)] #77 ohi regions that are in multiple fao regions
compareModel[compareModel$region_id %in% dups, ]

ggplot(compareModel, aes(x=no0s, group=fao_id)) +
  geom_histogram() +
  geom_histogram(aes(x=w0sCatch0s, group=fao_id), alpha=0.5, fill="red") + 
  theme_bw() +
  facet_wrap(~fao_id)+
  labs(title="black:no zero padding   red:zero padding")
