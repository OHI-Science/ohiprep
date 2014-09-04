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
  select(stock_id, fao_id, taxon_name, year=yr, b_bmsy) ; head(opw0)
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

#### make histograms of scores by major fishing area in 2011, per method - ALL STOCKS
library(ggplot2)
upw0 <- upw0 %>% mutate(whence='unif_w0')
upno0 <- upno0 %>% mutate(whence='unif_no0')
opw0 <- opw0 %>% mutate(whence='constr_w0')
opno0 <- opno0 %>% mutate(whence='constr_no0')

cmsy.all <- rbind(upw0, upno0, opw0, opno0) # %>% filter(!fao_id %in% c(48, 58, 88)) #join and  exclude fao regions overlapping with Antarctica
cmsy11 <- cmsy.all %>% filter( year == 2011) # only 2011

# ch <- ggplot(cmsy11, aes(x=b_bmsy)) + geom_histogram(binwidth=0.1,colour="white", fill="black") 
# ch1 <- ch + facet_grid(whence ~ fao_id) 
# ch2 <- ch1 + geom_vline(data = cmsy11, aes(xintercept= 1), colour='red', linetype="dashed", size=1)

### make boxplots of differences between 2011 and 2010 by major fishing area, per method
cmsy10 <- cmsy.all %>% filter( year == 2010) %>% rename(c('b_bmsy' = 'b_bmsy10')) # only 2010
cmsy_dif <- join(cmsy10, cmsy11, by=c('stock_id', 'whence')) %>% mutate (dif = b_bmsy - b_bmsy10 ) 

# cb <- ggplot(cmsy_dif, aes(x=fao_id, y=dif, fill=fao_id)) + geom_boxplot()
# cb1 <- cb + facet_wrap( ~ whence)
# cb1

#####################################################################################################################################
####### Separate HIGH SEAS an EEZs and plot them ##############
#################################################
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
hmc <- read.csv('../ohiprep/HighSeas/GL_HS_FIS_2014/data/fnk_fis_meancatch_lyr.csv', stringsAsFactors=F)

hmc = hmc %.%
  mutate(
    fao_id     = as.numeric(str_replace(fao_saup_id   , '^(.*)_(.*)$', '\\1')),
    saup_id    = as.numeric(str_replace(fao_saup_id   , '^(.*)_(.*)$', '\\2')),
    taxon_name =            str_replace(taxon_name_key, '^(.*)_(.*)$', '\\1'),
    TaxonKey   = as.numeric(str_replace(taxon_name_key, '^(.*)_(.*)$', '\\2')),   
    #Create Identifier for linking assessed stocks with country-level catches
    stock_id   = sprintf('%s_%d', taxon_name, fao_id))

hmcr <- select(hmc, mean_catch, stock_id, fao_id )
# hmcr <- hmcr %>% filter(!fao_id %in% c(48, 58, 88))
hmcr <- unique(hmcr)

# histograms for high seas stocks
cmsy11.hs <- cmsy11 %>% filter(stock_id %in% hmcr$stock_id) 
# ch3 <- ggplot(cmsy11.hs, aes(x=b_bmsy, fill=fao_id)) + geom_histogram(binwidth=0.1,colour="black") 
# ch4 <- ch3 + facet_grid(whence ~ fao_id) 
# ch5 <- ch4 + geom_vline(data = cmsy11.hs, aes(xintercept= 1), colour='blue', linetype="dashed", size=1)
# ch5

# png("../ohiprep/HighSeas/GL_HS_FIS/Hist_compare_cmsy_4versions_HighSeas.png", width=1300, height=900)
# plot(ch5)
# dev.off()

# boxplots of differences between 2011 and 2010 by major fishing area, per method - HIGH SEAS
cmsy10.hs <- cmsy.all %>% filter(stock_id %in% hmcr$stock_id) %>% 
                          filter( year == 2010) %>% rename(c('b_bmsy' = 'b_bmsy10')) # only 2010 for high seas
cmsy_dif.hs <- join(cmsy10.hs, cmsy11.hs, by=c('stock_id', 'whence')) %>% mutate (dif = b_bmsy - b_bmsy10 ) 

# hcb <- ggplot(cmsy_dif, aes(x=fao_id, y=dif, fill=fao_id)) + geom_boxplot()
# hcb1 <- hcb + facet_wrap( ~ whence)
# hcb1
# 
# png("../ohiprep/HighSeas/GL_HS_FIS/boxplots_compare_4cmsy_versions_HighSeas.png", width=1300, height=900)
# plot(hcb1)
# dev.off()

### EEZs comparisons
# histograms for EEZs stocks
cmsy11.eez <- cmsy11 %>% filter(stock_id %in% mc$stock_id) 
# ch6 <- ggplot(cmsy11.eez, aes(x=b_bmsy)) + geom_histogram(binwidth=0.1,colour="white", fill='black') 
# ch7 <- ch6 + facet_grid(whence ~ fao_id) 
# ch8 <- ch7 + geom_vline(data = cmsy11.eez, aes(xintercept= 1), colour='green', linetype="dashed", size=1)
# ch8

#####################################################################################################################################
####### take stocks with highest mean catch 
#####################################################################################################################################
# by EEZ 
## 1 # get the two top mean weights per fishing area
# test <- hmcr %>% group_by (fao_id) %>% arrange(fao_id, desc(mean_catch))
hmcr2 <- hmcr %>% group_by (fao_id) %>% summarise( max_catch = max(mean_catch, na.rm=T), first_stock = stock_id[which.max(mean_catch)],
                                                  second_stock = stock_id[ order(mean_catch)[length(mean_catch)-1] ] )
# top_stocks2 <- rbind(as.data.frame(cbind('stock_id' = hmcr2$first_stock,'what' = 'first_stock')), as.data.frame(cbind('stock_id' = hmcr2$second_stock, 'what' = 'second_stock')))
top_stocks <- c( hmcr2$first_stock, hmcr2$second_stock)

## 2 # join the 4 methods time-series for those stocks with highest mean catch 
# cmsy.all$fao_id <- as.numeric(as.character(cmsy.all$fao_id))
cmsy.top <- cmsy.all %>% filter(stock_id %in% top_stocks) # Joining by: "stock_id"

## 3 # join with catch time-series
nS7 <- nS7 %>% rename(c('yr' = 'year', 'ct' = 'b_bmsy')) %>% ungroup() ; head(nS5) # it's a bit weird to rename catch with 'b_bmsy', 
# this should be 'value' but for now will leave as is to be able to rbind to bbmsy values

ct.all <- nS7 %>% 
          mutate(
                  fao_id     = as.numeric(str_replace(stock_id, '^(.*)_(.*)$', '\\2')),
                  taxon_name = str_replace(stock_id, '^(.*)_(.*)$', '\\1') ) 
ct.all <- ct.all %>% filter(stock_id %in% top_stocks) %>% select ( stock_id, fao_id, taxon_name, year, b_bmsy) 
ct.all <- ct.all  %>% mutate (whence = 'catch')
ct.all.resc <- ct.all  %>% group_by(stock_id) %>% mutate(b_bmsy = b_bmsy/max(b_bmsy))
cmsy.top <- rbind (cmsy.top, ct.all.resc) 


# for HIGH SEAS
dir_hs <- '../ohiprep/HighSeas/GL_HS_FIS_2014'
Mct <- read.csv( file.path ( dir_hs, 'data/fnk_fis_meancatch_lyr.csv') ) ; head(Mct)

## 4 # ggplot them

# tp <- ggplot(cmsy.top, aes(x=year,  y=b_bmsy, group=whence, color=whence)) +
#   geom_line() +
#   facet_wrap(~ stock_id) + 
#   theme_bw()
# tp
# 
# png("../ohiprep/HighSeas/GL_HS_FIS/Timeseries_compare_4cmsy_versions_Top2Stocks.png", width=1300, height=900)
# plot(tp)
# dev.off()
# 
# tp <- ggplot(cmsy.top, aes(x=year,  y=b_bmsy, group=whence, color=whence)) +
#   geom_point(position=position_jitter(width=0.01,height=.01)) +
#   geom_line() +
#       facet_wrap(~ stock_id) + 
#       theme_bw()
#  tp

cmsy.top.no0 <- filter(cmsy.top, whence %in% c("unif_no0", "constr_no0") )
  cmsy.top.w0 <- filter(cmsy.top, whence %in%  c("unif_w0", "constr_w0") )

# tp.no0 <- ggplot(cmsy.top.no0, aes(x=year,  y=b_bmsy, group=whence, color=whence)) +
#   geom_point() +
#   geom_line() +
#   facet_wrap(~ stock_id, ncol = 6) +
#   theme_bw()
# tp.no0
# 
# tp.w0 <- ggplot(cmsy.top.w0, aes(x=year,  y=b_bmsy, group=whence, color=whence)) +
#   geom_point() +
#   geom_line() +
#   facet_wrap(~ stock_id, ncol = 6) +
#   theme_bw()
# tp.w0

