 src('Global/FIS_Bbmsy/Hex outputs/cmsy_cfr_Aug182014.R')
library(plyr)
library(dplyr)

cmsy11$stock_id <- as.character(cmsy11$stock_id)
cmsy11.r <- left_join(cmsy11, res_scores) # 184 excluded (no recent catch)
# cmsy11.r <- cmsy11.r %>% filter (fao_id != 18)
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
# cmsy11.r <- left_join(cmsy11, res_scores) # 184 excluded (no recent catch)
# cmsy11.r <- cmsy11.r %>% filter (fao_id != 18)
# cmsy11.u <- cmsy11.r %>% group_by (
#   stock_id, fao_id ) %>% filter (whence == 'unif_no0', unif_prior == 1)
# cmsy11.c <- cmsy11.r %>% group_by (
#   stock_id, fao_id ) %>% filter (whence == 'constr_no0', unif_prior == 0)
# cmsy11.all <- rbind(cmsy11.u, cmsy11.c)
# fao_id_nm <- rbind( rfmo_fao[,c(2,18)], c('Mediterranean and Black Sea', 37))
# cmsy11.all <- left_join( cmsy11.all, fao_id_nm )
# # cmsy11.all.05 <- cmsy11.all
# # cmsy11.all.04 <- cmsy11.all
#  cmsy11.all.07 <- cmsy11.all
# # join the two versions
# cmsy11.all.05 <- cmsy11.all.05 %>% mutate (cutoff = '0.5')
# cmsy11.all.06 <- cmsy11.all.06 %>% mutate (cutoff = '0.6')
# cmsy11.all.v <- rbind (cmsy11.all.05, cmsy11.all.06)
# cmsy11.all.04 <- cmsy11.all.04 %>% mutate (cutoff = '0.4')
# cmsy11.all.07 <- cmsy11.all.07 %>% mutate (cutoff = '0.7')
# cmsy11.all.v <- rbind (cmsy11.all.04, cmsy11.all.v, cmsy11.all.07)
                       
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

##################
head(cmsy_dif)
cmsy_dif$stock_id <- as.character(cmsy_dif$stock_id)

# modify the resilience cutoff
# res_scores <- res_scores %>% mutate (
#   unif_prior = ifelse( final_score > 0.5, 1, 0)
# )

cmsy_dif.r <- left_join(cmsy_dif, res_scores) ; head(cmsy_dif.r) # 184 excluded (no recent catch)
cmsy_dif.r <- cmsy_dif.r %>% filter (fao_id != 18)  # should no longer do this

cmsydif.u <- cmsy_dif.r %>% group_by (
  stock_id, fao_id ) %>% filter (whence == 'unif_no0', unif_prior == 1)
cmsydif.c <- cmsy_dif.r %>% group_by (
  stock_id, fao_id ) %>% filter (whence == 'constr_no0', unif_prior == 0)

cmsydif.all <- rbind(cmsydif.u, cmsydif.c)
# add fao region names
fao_id_nm <- rbind( rfmo_fao[,c(2,18)], c('Mediterranean and Black Sea', 37))
cmsydif.all <- left_join( cmsydif.all, fao_id_nm )
# save versions of the runs using different cutoffs
cmsydif.all.06 <- cmsydif.all
cmsydif.all.05 <- cmsydif.all
cmsydif.all.06 <- cmsydif.all.06 %>% mutate(cutof = '0.6')
cmsydif.all.05 <- cmsydif.all.05 %>% mutate(cutof = '0.5')
cmsydif.all.v <- rbind(cmsydif.all.05, cmsydif.all.06)

hcb <- ggplot(cmsydif.all.v, aes(x=fao_id, y=dif, fill=rgn_name)) + geom_boxplot()
hcb1 <- hcb + facet_wrap( ~ cutof)
hcb1