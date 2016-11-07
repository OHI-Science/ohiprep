# Modified from data-edit/model/clean_UNDP (by JStewart Jul2013)

source("../ohiprep/src/R/common.R")

## sovereignty (parent-children) gapfilling with add_gapfill_sov.r

#setwd('N:/git-annex/Global/FigurePrep_2013OHI/GL-UNDP-HDI_v2012_continuousVals_mrf/raw')

#library(reshape2)
#library(gdata)
library(stringr)
#require(sqldf)
#options(max.print=5E6)
#options(gsubfn.engine = "R") # otherwise, get X11 launching for sqldf pa

#This is the old version:
#source('C:/Users/Melanie/Desktop/NCEAS/Projects/OHI 2013/GL-UNDP-HDI_v2012/ohi_clean_fxns.r') 
source('../ohicore/R/gapfill_georegions.R')
source('../ohicore/R/name_to_rgn.R')

d.all =  matrix(nrow=0, ncol=0)
for (f in list.files(path="Global/GL-UNDP-HDI_v2012_continuousVals/raw", pattern=glob2rx('*Table1.csv'))){
  #f <- list.files(pattern=glob2rx('*Table1.csv'))[[1]]
  d = read.csv(file.path("Global/GL-UNDP-HDI_v2012_continuousVals/raw", f), header=F) 
    names(d)[1] = 'Country'
  
  # add identifier column
  a = strsplit(f, '_', fixed=FALSE)
  d$Developed_Developing = rep.int(unlist(a)[2], dim(d)[1])  

  d.all = rbind(d.all, d)
}


### following isn't necessary for what I'm doing - but keep in place if we need additional variables in future:
d.all[, "Developed_Developing"] = gsub('.*ed', '1', d.all[, "Developed_Developing"]) 
d.all[, "Developed_Developing"] = gsub('.*ing', '0', d.all[, "Developed_Developing"]) 
d.all[, "Developed_Developing"] = as.numeric(d.all[, "Developed_Developing"])
  
d.all$year = 2013

d.all <- plyr::rename(d.all, c(V2="HDI", V3="LifeExp", V4="Schooling", 
                         V5="ExpectedSchooling", V6="GNI", V7="GNIrankHDIrank",
                         V8= "NonIncomeHDI"))
### end of unnecessary stuff

d.all <- d.all %>%
  select(Country, year, HDI)

# Weighted average of China data based on population
(0.906*7.859 + 0.699*1367)/(1367+7.859) # 0.700
d.all <- subset(d.all, Country!="Hong Kong China (SAR)")
d.all[d.all$Country ==  "China", ]
d.all$HDI[d.all$Country=="China"] <- 0.700


data <- name_to_rgn(d.all, fld_name="Country", fld_value="HDI")


## sovereign gapfilling with gapfill_georegions.r ----
# use gapfill_georegions: lookup table that has sov_ids and weight the 'parent' country with 1, others with 0

# read in lookups
sovregions = read.csv('../ohiprep/src/LookupTables/eez_rgn_2013master.csv', na.strings='') %>% 
  select(rgn_id = rgn_id_2013,
         r2 = sov_id) %>%     # r2 is actually rgn_ids of sovereign regions
  group_by(rgn_id) %>%                       # remove duplicated countrys from this rgn_id list                    
  summarize(r2 = mean(r2, na.rm=T)) %>% # duplicates always have the same sov_id (r2 value)
  mutate(r1 = r2, 
         r0 = r2,
         fld_wt = as.integer(rgn_id == r2)) %>%  # flag the 'parent' rgn_id with 1, others with 0 
  filter(rgn_id < 255, rgn_id != 213); head(sovregions)

# join fld_wt weighting to m_d
data = data %>% 
  left_join(sovregions %>%
              select(rgn_id, fld_wt),
            by = 'rgn_id'); head(test)

data = data %>%
  filter(!rgn_id %in% c(213,255)) %>%
  select(rgn_id, HDI, fld_wt)


# gapfill_georegions
attrsave  = "Global/GL-UNDP-HDI_v2012_continuousVals/data/rgn_hdi_2013_attr.csv"

# library(devtools); load_all('../ohicore')
# source('../ohicore/R/gapfill_georegions.R')
data_gap_filled = gapfill_georegions(
  data = data,
  fld_id = c('rgn_id'),
  fld_weight = 'fld_wt',
  georegions = sovregions %>%
    select(-fld_wt),
  r0_to_NA = TRUE, 
  attributes_csv = (attrsave)) 

# fill in missing values with NA
all_rgns <- read.csv("src/LookupTables/eez_rgn_2013master.csv") %>%
  filter(rgn_typ == "eez",
         rgn_id_2013 != 255) %>%
  select(rgn_id=rgn_id_2013) %>%
  unique() %>%
  left_join(data_gap_filled) %>%
  arrange(rgn_id)
  


write.csv(all_rgns, "Global/GL-UNDP-HDI_v2012_continuousVals/data/hdi.csv", row.names=FALSE, na="")



# investigate attribute tables
head(attr(d_g_a, 'gapfill_georegions'))  # or to open in excel: system(sprintf('open %s', attrsave))

test2 <- gapfill_georegions(test, fld_id='region_id', fld_value="HDI")



