
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

#### compare last 2 years: 2011-2010 #####
csm10 <- csm %>% filter( yr == 2010 ) %>% rename ( c('b_bmsy' = 'b_bmsy10') )
csm.dif <- left_join(csm11, csm10) %>% mutate (dif = b_bmsy - b_bmsy10, whence = 'sm_const')

usm10 <- usm %>% filter( yr == 2010 ) %>% rename ( c('b_bmsy' = 'b_bmsy10') )
usm.dif <- left_join(usm11, usm10) %>% mutate (dif = b_bmsy - b_bmsy10, whence = 'sm_unif')

ucsm10 <- cmsy.all %>% filter (year == 2010, whence == 'unif_no0') %>% mutate ( fao_id = str_split_fixed(stock_id, '_', 2)[,2], whence = 'unif_09')
ucsm.dif <- left_join( ucsm11, ucsm10 ) %>% select ( stock_id, fao_id, rgn_name, b_bmsy, whence )


sm.dif <- rbind(usm.dif, csm.dif)

ch <- ggplot(sm.dif, aes(x=dif, fill = rgn_name)) + geom_histogram(binwidth=0.1,colour="white") 
ch1 <- ch + facet_grid(whence ~ rgn_name) 
ch2 <- ch1 + geom_vline(data = sm.dif, aes(xintercept= 0), colour='black', size=1)
ch2


#####################################################################################################################################
####### take stocks with highest mean catch in SUSPECT regions #####################################
#####################################################################################################################################
## something went wrong
### 1 ## create a list of suspect countries ###
W_latinoamerica <- c(222,152,153,604)
latin_lookup <- rfmo_lookup[ rfmo_lookup$saup_id %in% W_latinoamerica, ] %>% arrange(saup_id, fao_id) %>% mutate(reg = 'W_latinoamerica') ; head(latin_lookup)
EAsia <- c(361,362,702,608,764,460,459,626)
asian_lookup <- rfmo_lookup[ rfmo_lookup$saup_id %in% EAsia, ] %>% arrange(saup_id, fao_id) %>% mutate(reg = 'SE_Asia') ; asian_lookup
WAfr <- c(384,566,132,288)
WA_lookup <- rfmo_lookup[ rfmo_lookup$saup_id %in% WAfr, ] %>% arrange(saup_id, fao_id) %>% mutate(reg = 'W_Africa') ; WA_lookup
Med <- c(300,380,434,470)
Med_lookup <- rfmo_lookup[ rfmo_lookup$saup_id %in% Med, ] %>% arrange(saup_id, fao_id) %>% mutate(reg = 'Med') ; Med_lookup
EAfr <- c(690,404,706,834, 462, 251, 508)
EA_lookup <- rfmo_lookup[ rfmo_lookup$saup_id %in% EAfr, ] %>% arrange(saup_id, fao_id) %>% mutate(reg = 'E_Africa') ; EA_lookup
Alaska <- 841
Alaska_lookup <- rfmo_lookup[ rfmo_lookup$saup_id %in% Alaska, ] %>% arrange(saup_id, fao_id) %>% mutate(reg = 'Alaska') ; Alaska_lookup

prob_rgn <- rbind( latin_lookup, asian_lookup, WA_lookup, Med_lookup, EA_lookup, Alaska_lookup )

## 2 # get mean catch to later select the two top mean weights per HIGH SEAS
dir_hs <- '../ohiprep/HighSeas/HS_FIS_v2013'
Mct <- read.csv( file.path ( dir_hs, 'data/cnk_fis_meancatch.csv') ) ; head(Mct)

library(stringr)

Mct <- Mct %>% 
  mutate( fao_id = str_split_fixed ( fao_saup_id , '_', 2) [,1], 
          saup_id = str_split_fixed ( fao_saup_id , '_', 2) [,2],
          taxon_name = str_split_fixed ( taxon_name_key, '_', 2) [,1],
          TL = substr( str_split_fixed ( taxon_name_key, '_', 2) [,2], 1, 1) ,
          stock_id = paste(taxon_name, fao_id, sep = '_' ) ) ; head(Mct)

### 3 # get smoothed data, no 0s, with both priors, and join with resilience data

dir_d = '../ohiprep/Global/NCEAS-Fisheries_2014a' # set folder where files are saved

unif_sm_no0 <- 'fnk_fis_b_bmsy_lyr_uniform_no0_runningMean.csv'
constr_sm_no0 <- 'fnk_fis_b_bmsy_lyr_constrained_no0_runningMean.csv'

usm <- read.csv( file.path(dir_d, 'tmp', unif_sm_no0) ) ; head(usm)
usm <- usm %>% mutate ('whence' = 'u_sm_no0')
csm <- read.csv( file.path(dir_d, 'tmp', constr_sm_no0) ) ; head(csm)
csm <- csm %>% mutate ('whence' = 'c_sm_no0')

sm <- rbind(csm, usm)

### 4 # join smoothed data with resilience scores

sm$stock_id <- as.character(sm$stock_id)
cmsy.r <- left_join(sm, res_scores) # 184 excluded (no recent catch)

### 5 # join mean catch
Mct2 <- Mct %>% filter (saup_id == 0) %>% select (fao_id, mean_catch, stock_id) 
Mct2 <- unique( Mct2 )
Mct3 <- Mct2 %>% ungroup() %>% group_by (fao_id) %>% mutate( rel.ct = mean_catch / sum(mean_catch) ) %>% select (fao_id, mean_catch, stock_id, rel.ct) %>% arrange (fao_id, desc(rel.ct)) %>% ungroup()
# select problem fao regions
Mct3 <- Mct3 %>% filter (fao_id %in% prob_rgn$fao_id)
cmsy.r.m <- left_join (Mct3, cmsy.r)
#select top weight stocks
Mct4 <- cmsy.r.m %>% group_by (fao_id) %>% summarise( top_stock = stock_id[which.max(rel.ct)],
           second_stock = stock_id[ order(rel.ct)[length(rel.ct)-1] ] 
          )

Cdata <- nS5 %>% select (stock_id, ct, yr)
Cdata2 <- left_join(cmsy.r.m, Cdata) %>% filter (whence == 'c_sm_no0') %>% select(fao_id, mean_catch, stock_id, rel.ct, yr, final_score, unif_prior, ct) %>% mutate (whence = 'catch') %>% rename (c(ct = 'b_bmsy' )) %>% select (fao_id, mean_catch, stock_id, rel.ct, yr, b_bmsy, whence, final_score, unif_prior)
Cdata3 <- rbind(Cdata2, cmsy.r.m)
Cdata3 <- Cdata3 %>% filter(stock_id %in% c(Mct4$top_stock, Mct4$second_stock)) %>% filter(!is.na(whence))

# test <- filter(cmsy.r.m, rel.ct > 0.2)
# length(unique(test$stock_id))
test2 <- filter(cmsy.r.m, rel.ct > 0.1, whence != is.na(whence))
length(unique(test2$stock_id))
tp <- ggplot(test2, aes(x=yr,  y=b_bmsy, group=whence, color=whence)) +
  geom_line() +
  facet_wrap( ~ stock_id) + 
  theme_bw()
tp

tp <- ggplot(Cdata3, aes(x=yr,  y=b_bmsy, group=stock_id, color=whence)) +
  geom_line() +
  facet_wrap( ~ stock_id) + 
  theme_bw()
tp


## get the selected output, get the catch



# 3) get the top 2 stocks for rel_ct per ohi_id (no replicates)
# 4) merge dfs to obtain the following info all in one file
# region identifiers : fao_id mora_score rfmo_score ohi_id saup_id
# stocks: stock_id, resilience score, rel_ct in the EEZ, catch time-series
# cmsy: smoothed with unif prior, smoothed with constrained prior, flag on which was used
# 5) hist: how many of the top stocks were assigned to the constrained prior in the trouble areas? 
# -> top stocks (get rel catch and select top stocks by EEZ: 1-2)
# 6) mora score vs stock score vs relstock catch in eez vs hs:
#   -> get rel ct within and outside EEZs per stock (used to calc resil score), and resil score
# stacked hist of rel_ct in high seas vs EEZ per stock, with resil score on x-axis:
#  is resil score higher for stocks with rel ct mostly in high seas? check in each fao_id
# 7) time-seriesw of b_bmsy constrained prior vs uniform (smoothed, no0s) vs catch - separate plots by which was used: 
#  -> get smoothed uniform, smoothed constrained, flag of which was used, catch - only for top stocks of problem areas by fao_region/eez
#   - does constrained seem the better model when constrained was picked?
#  - does uniform seem the better model when uniform was picked?


# take the most influential stocks and plot the uniform versus constrained cmsy ##

# then do individual plots for suspect countries
# 
# then look at their resilience (already did for a few)

## 1 # get the two top mean weights per HIGH SEAS
dir_hs <- '../ohiprep/HighSeas/HS_FIS_v2013'
Mct <- read.csv( file.path ( dir_hs, 'data/cnk_fis_meancatch.csv') ) ; head(Mct)

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
## 2 # join smoothed b_bmsy for both priors, and smoothed b_bmsy combo - for those stocks with highest mean catch 
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