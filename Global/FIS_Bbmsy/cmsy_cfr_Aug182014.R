########################################################
###### checking B_Bmsy with different runs #############

# upload results from Hexagon runs with uniform/constrained prior, with/without 0s padding
dir_Hex_data = 'Global/FIS_Bbmsy/Hex outputs'
file_1<- 'cmsy_ohi_results_table_originalPrio_added0s.RData'
load(file.path(dir_Hex_data, file_1 )) ; head(cmsy.ohi.orig.with0.df)
opw0 <- cmsy.ohi.orig.with0.df
file_2<- 'cmsy_ohi_results_table_originalPrio_no0s.RData' 
load(file.path(dir_Hex_data, file_2 )) ; head(cmsy.ohi.orig.no0.df)
opno0 <- cmsy.ohi.orig.no0.df
file_3 <- 'cmsy_ohi_results_table_unifPrio_no0s.RData' 
load(file.path(dir_Hex_data, file_3 )) ; head(cmsy.ohi.unif.no0.df)
upno0 <- cmsy.ohi.unif.no0.df

# separate taxon name and fao id
opw0 <- opw0 %.%
  mutate(fao_id = sapply(strsplit(as.character(stock_id), "_"), function(x)x[2]),
         taxon_name = sapply(strsplit(as.character(stock_id), "_"), function(x)x[1])) %.%
  select(stock_id, fao_id, taxon_name, year=yr, b_bmsy) ; head(upw0)
opno0 <- opno0 %.%
  mutate(fao_id = sapply(strsplit(as.character(stock_id), "_"), function(x)x[2]),
         taxon_name = sapply(strsplit(as.character(stock_id), "_"), function(x)x[1])) %.%
  select(stock_id, fao_id, taxon_name, year=yr, b_bmsy) ; head(opno0)
upno0 <- upno0 %.%
  mutate(fao_id = sapply(strsplit(as.character(stock_id), "_"), function(x)x[2]),
         taxon_name = sapply(strsplit(as.character(stock_id), "_"), function(x)x[1])) %.%
  select(stock_id, fao_id, taxon_name, year=yr, b_bmsy) ; head(upno0)
  
# upload old results with unif and 0s
upw0 <- read.csv("Global/FIS_Bbmsy/raw/cmsy.ohi.df_Jul292014.csv", na.strings=''); head(upw0)

upw0 <- upw0 %.%
  mutate(fao_id = sapply(strsplit(as.character(stock_id), "_"), function(x)x[2]),
         taxon_name = sapply(strsplit(as.character(stock_id), "_"), function(x)x[1])) %.%
  select(stock_id, fao_id, taxon_name, year=yr, b_bmsy) ; head(upw0)

#### make histograms of scores by major fishing area in 2011, per method
library(ggplot2)
upw0 <- upw0 %>% mutate(whence='unif_w0')
upno0 <- upno0 %>% mutate(whence='unif_no0')
opw0 <- opw0 %>% mutate(whence='constr_w0')
opno0 <- opno0 %>% mutate(whence='constr_no0')

cmsy.all <- rbind(upw0, upno0, opw0, opno0)
cmsy11 <- cmsy.all %>% filter( year == 2011) # only 2011
cmsy11 <- cmsy11 %>% filter(!fao_id %in% c(48, 58, 88)) # exclude fao regions overlapping with Antarctica

ch <- ggplot(cmsy11, aes(x=b_bmsy)) + geom_histogram(binwidth=0.1,colour="white", fill="black") 
ch1 <- ch + facet_grid(whence ~ fao_id) 
ch2 <- ch1 + geom_vline(data = cmsy11, aes(xintercept= 1), colour='red', linetype="dashed", size=1)

### make boxplots of differences between 2011 and 2010 by major fishing area, per method
cmsy10 <- cmsy.all %>% filter( year == 2010) %>% rename(c('b_bmsy' = 'b_bmsy10')) # only 2010
cmsy10 <- cmsy10 %>% filter(!fao_id %in% c(48, 58, 88)) # exclude fao regions overlapping with Antarctica
cmsy_dif <- join(cmsy10, cmsy11, by=c('stock_id', 'whence')) %>% mutate (dif = b_bmsy - b_bmsy10 ) 

cb <- ggplot(cmsy_dif, aes(x=fao_id, y=dif, fill=fao_id)) + geom_boxplot()
cb1 <- cb + facet_wrap( ~ whence)
cb1

####### take stocks with highest mean catch 
# upload average catch by eez (or: src('Global/NCEAS-Fisheries_2014a/cnk_fis_meancatch_datamaker.R') and get cnk_fis_meancatch)
mc <- read.csv('Global/NCEAS-Fisheries_2014a/data/fnk_fis_meancatch_lyr.csv', na.strings='', stringsAsFactors=F); head(mc)

# create identifier to link average catch and b_bmsy files by stock id and fao id
mc = mc %.%
  mutate(
    fao_id     = as.numeric(str_replace(fao_saup_id   , '^(.*)_(.*)$', '\\1')),
    saup_id    = as.numeric(str_replace(fao_saup_id   , '^(.*)_(.*)$', '\\2')),
    taxon_name =            str_replace(taxon_name_key, '^(.*)_(.*)$', '\\1'),
    TaxonKey   = as.numeric(str_replace(taxon_name_key, '^(.*)_(.*)$', '\\2')),   
    #Create Identifier for linking assessed stocks with country-level catches
    stock_id   = sprintf('%s_%d', taxon_name, fao_id))

# upload average catch by high seas (or: src('Global/NCEAS-Fisheries_2014a/cnk_fis_meancatch_datamaker.R' and get hs_cnk_fis_meancatch))
hmc <- read.csv(hs_cnk_fis_meancatch, '../ohiprep/HighSeas/GL_HS_FIS/data/fnk_fis_meancatch_lyr.csv', stringsAsFactors=F)

#### high seas comparisons
# join mean catch and b_bmsy by year and stock_id, plot the mean catch and the 4 versions for the top 2 stocks per major fishing area

## 1 # get the 2 top mean weights per fishing area

## 2 # join the 4 methods time-series for those stocks with mean catch

## 3 # ggplot them