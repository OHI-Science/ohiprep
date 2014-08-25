
library(gdata)
library(stringr)

source('../ohiprep/src/R/common.R') # set dir_neptune_data
source('../ohiprep/src/R/ohi_clean_fxns.R') # name-to-region functions (but need to add new saup names files)


dir_d = '../ohiprep/Global/NCEAS-Fisheries_2014a' # set folder where files are saved

unif_sm_no0 <- 'fnk_fis_b_bmsy_lyr_uniform_no0_runningMean.csv'
constr_sm_no0 <- 'fnk_fis_b_bmsy_lyr_constrained_no0_runningMean.csv'

usm <- read.csv( file.path(dir_d, 'tmp', unif_sm_no0) ) ; head(usm)
csm <- read.csv( file.path(dir_d, 'tmp', constr_sm_no0) ) ; head(csm)

csm11 <- csm %>% filter (yr == 2011) %>% mutate (fao_id = str_split_fixed(stock_id, '_', 2)[,2], whence = 'const_sm') 
usm11 <- usm %>% filter (yr == 2011) %>% mutate (fao_id = str_split_fixed(stock_id, '_', 2)[,2], whence = 'unif_sm')

u09 <- cmsy.all %>% filter (year == 2009, whence == 'unif_no0') %>% mutate ( fao_id = str_split_fixed(stock_id, '_', 2)[,2], whence = 'unif_09')
u09 <- left_join( u09, fao_id_nm ) %>% select ( stock_id, fao_id, rgn_name, b_bmsy, whence )

# add FAO region names
# rfmo_fao <- read.csv(file.path(dir_neptune_data, dir_r2, 'tmp/RFMOperFAO.csv'), stringsAsFactors = F)
# fao_id_rgn <- read.csv('../ohiprep/tmp/install_local/ohi-global-master/highseas2014/layers/FAOregions.csv', stringsAsFactors = F)
# rfmo_fao <- left_join(rfmo_fao, fao_id_rgn) ; rfmo_fao 
# fao_id_nm <- rbind( rfmo_fao[,c(2,18)], c('Mediterranean and Black Sea', 37))

csm11 <- left_join( csm11, fao_id_nm ) # Joining by: "fao_id"
usm11 <- left_join( usm11, fao_id_nm ) # Joining by: "fao_id"

# src('cmsy_cfr_Aug182014.R')# 
cmsy11.all <- cmsy11.all %>% mutate (whence = 'combo_sm') %>% select (whence, stock_id, year, b_bmsy) 
# create same headers for all
ucsm11 <- cmsy11.all %>% select ( stock_id, fao_id, rgn_name, b_bmsy, whence )
csm11 <- csm11 %>% select( stock_id, fao_id, rgn_name, b_bmsy, whence )
usm11 <- usm11 %>% select( stock_id, fao_id, rgn_name, b_bmsy, whence )

sm.all <- rbind(ucsm11, csm11, usm11, u09)
sm.all <- sm.all %>% filter ( rgn_name != is.na(rgn_name) ) # remove the Antarctic regions, since they are not present in all versions

############## PLOTS #################

# cfr the combo version with the uniform smoothed
# c_us <- filter(sm.all, whence %in% c( 'combo_sm', 'unif_sm') )
# 
# library(ggplot2)
# ch <- ggplot(c_us, aes(x=b_bmsy, fill = rgn_name)) + geom_histogram(binwidth=0.1,colour="white") 
# ch1 <- ch + facet_grid(whence ~ rgn_name) 
# ch2 <- ch1 + geom_vline(data = c_us, aes(xintercept= 1), colour='blue', linetype="dashed", size=1)
# ch2

library(ggplot2)
ch <- ggplot(sm.all, aes(x=b_bmsy, fill = rgn_name)) + geom_histogram(binwidth=0.1,colour="white") 
ch1 <- ch + facet_grid(whence ~ rgn_name) 
ch2 <- ch1 + geom_vline(data = sm.all, aes(xintercept= 1), colour='blue', linetype="dashed", size=1)
ch2




then do individual plots for suspect countries

then look at their resilience (already did for a few)

#####################################################################################################################################
####### take stocks with highest mean catch 
#####################################################################################################################################

## 1 # get the two top mean weights per HIGH SEAS
dir_hs <- '../ohiprep/HighSeas/GL_HS_FIS_2014'
Mct <- read.csv( file.path ( dir_hs, 'data/fnk_fis_meancatch_lyr.csv') ) ; head(Mct)

library(stringr)

Mct <- Mct %>% 
  mutate( fao_id = str_split_fixed ( fao_saup_id , '_', 2) [,1], 
          saup_id = str_split_fixed ( fao_saup_id , '_', 2) [,2],
          taxon_name = str_split_fixed ( taxon_name_key, '_', 2) [,1],
          TL = substr( str_split_fixed ( taxon_name_key, '_', 2) [,2], 1, 1) ,
          stock_id = paste(taxon_name, fao_id, sep = '_' ) ) ; head(Mct)

top_taxon_hs <- Mct %>% filter (year == 2011) %>% group_by (fao_id) %>% summarise( top_stock = stock_id[which.max(mean_catch)],
               second_stock = stock_id[ order(mean_catch)[length(mean_catch)-1] ] 
              )
 test <- Mct %>% group_by (fao_id) %>% filter (year == 2011) %>% arrange(fao_id, desc(mean_catch))

top_stock_hs <- Mct %>% filter (year == 2011, TL == 6) %>% group_by (fao_id) %>% summarise( top_stock = stock_id[which.max(mean_catch)],
               second_stock = stock_id[ order(mean_catch)[length(mean_catch)-1] ] 
              )
test2 <- Mct %>% group_by (fao_id) %>% filter (year == 2011, TL == 6) %>% arrange(fao_id, desc(mean_catch))



# top_stocks2 <- rbind(as.data.frame(cbind('stock_id' = hmcr2$first_stock,'what' = 'first_stock')), as.data.frame(cbind('stock_id' = hmcr2$second_stock, 'what' = 'second_stock')))
top_stocks <- c( Mct$first_stock, Mct$second_stock)

############################################################################################
## 2 # join smoothed b_bmsy for both priors, and unsmoothed b_bmsy for uniform prior - for those stocks with highest mean catch 
# cmsy.all$fao_id <- as.numeric(as.character(cmsy.all$fao_id))
cmsy.top <- cmsy.all %>% filter(stock_id %in% top_stocks) # Joining by: "stock_id"

## 3 # join with catch time-series - no 0s
nS5 <- nS5 %>% rename(c('yr' = 'year', 'ct' = 'value')) %>% ungroup() ; head(nS5)  

ct.all <- nS5 %>% 
  mutate(
    fao_id     = as.numeric(str_replace(stock_id, '^(.*)_(.*)$', '\\2')),
    taxon_name = str_replace(stock_id, '^(.*)_(.*)$', '\\1') ) 
ct.all <- ct.all %>% filter(stock_id %in% top_stocks) %>% select ( stock_id, fao_id, taxon_name, year, value) 
ct.all <- ct.all  %>% mutate (whence = 'catch')
ct.all.resc <- ct.all  %>% group_by(stock_id) %>% mutate(value = value/max(value))
# then rbind it
cmsy.top <- rbind (cmsy.top, ct.all.resc) 

## 4 # ggplot them

tp <- ggplot(cmsy.top, aes(x=year,  y=b_bmsy, group=whence, color=whence)) +
  geom_line() +
  facet_wrap(~ stock_id) + 
  theme_bw()
tp

png("../ohiprep/HighSeas/GL_HS_FIS/Timeseries_compare_4cmsy_versions_Top2Stocks.png", width=1300, height=900)
plot(tp)
dev.off()

tp <- ggplot(cmsy.top, aes(x=year,  y=b_bmsy, group=whence, color=whence)) +
  geom_point(position=position_jitter(width=0.01,height=.01)) +
  geom_line() +
  facet_wrap(~ stock_id) + 
  theme_bw()


## 2 # get the two top mean weights for EEZs