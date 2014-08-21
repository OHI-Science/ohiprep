src('Global/FIS_Bbmsy/Hex outputs/cmsy_cfr_Aug182014.R')
library(plyr)
library(dplyr)

cmsy11$stock_id <- as.character(cmsy11$stock_id)
cmsy11.r <- left_join(cmsy11, res_scores) # 184 excluded (no recent catch)
cmsy11.r <- cmsy11.r %>% filter (fao_id != 18)
# select only model versions without 0s

# select different model versions depending on the flag in field 'unif_prior'
# cmsy11.r <- cmsy11.r %>% filter(whence %in% c('unif_no0', 'constr_no0'))
# test <- cmsy11.r %>% group_by (
#                         stock_id, fao_id ) %>%  summarise (
#                                                   new.b_bmsy = ifelse(
#                                                                       unif_prior == 1, 
#                                                                       b_bmsy[which(whence == 'unif_no0')], 
#                                                                       b_bmsy[which(whence == 'constr_no0')] 
#                                                                          )
#                                  )

cmsy11.u <- cmsy11.r %>% group_by (
  stock_id, fao_id ) %>% filter (whence == 'unif_no0', unif_prior == 1)
cmsy11.c <- cmsy11.r %>% group_by (
  stock_id, fao_id ) %>% filter (whence == 'constr_no0', unif_prior == 0)

cmsy11.all <- rbind(cmsy11.u, cmsy11.c)
# add fao region names
fao_id_nm <- rbind( rfmo_fao[,c(2,18)], c('Mediterranean and Black Sea', 37))
cmsy11.all <- left_join( cmsy11.all, fao_id_nm )

# save the run using 0.6 cutoff
cmsy11.all.06 <- cmsy11.all

# redo with 0.5 cutoff
cmsy11.r <- left_join(cmsy11, res_scores) # 184 excluded (no recent catch)
cmsy11.r <- cmsy11.r %>% filter (fao_id != 18)
cmsy11.u <- cmsy11.r %>% group_by (
  stock_id, fao_id ) %>% filter (whence == 'unif_no0', unif_prior == 1)
cmsy11.c <- cmsy11.r %>% group_by (
  stock_id, fao_id ) %>% filter (whence == 'constr_no0', unif_prior == 0)
cmsy11.all <- rbind(cmsy11.u, cmsy11.c)
fao_id_nm <- rbind( rfmo_fao[,c(2,18)], c('Mediterranean and Black Sea', 37))
cmsy11.all <- left_join( cmsy11.all, fao_id_nm )
# cmsy11.all.05 <- cmsy11.all
# cmsy11.all.04 <- cmsy11.all
 cmsy11.all.07 <- cmsy11.all
# join the two versions
cmsy11.all.05 <- cmsy11.all.05 %>% mutate (cutoff = '0.5')
cmsy11.all.06 <- cmsy11.all.06 %>% mutate (cutoff = '0.6')
cmsy11.all.v <- rbind (cmsy11.all.05, cmsy11.all.06)
cmsy11.all.04 <- cmsy11.all.04 %>% mutate (cutoff = '0.4')
cmsy11.all.07 <- cmsy11.all.07 %>% mutate (cutoff = '0.7')
cmsy11.all.v <- rbind (cmsy11.all.04, cmsy11.all.v, cmsy11.all.07)
                       
library(ggplot2)
ch <- ggplot(cmsy11.all, aes(x=b_bmsy, fill = rgn_name)) + geom_histogram(binwidth=0.1,colour="white") 
ch1 <- ch + facet_wrap( ~ rgn_name) 
ch2 <- ch1 + geom_vline(data = cmsy11.all, aes(xintercept= 1), colour='blue', linetype="dashed", size=1)
ch2

library(ggplot2)
ch <- ggplot(cmsy11.all.v, aes(x=b_bmsy, fill = rgn_name)) + geom_histogram(binwidth=0.1,colour="black") 
ch1 <- ch + facet_grid(cutoff ~ rgn_name) 
ch2 <- ch1 + geom_vline(data = cmsy11.all.v, aes(xintercept= 1), colour='blue', linetype="dashed", size=1)
ch2

ggsave('cmsy_combo_resil_cfr.png')
