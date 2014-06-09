
wd = 'C:/Users/best/Documents/data/ingest/GL-SeaLifeBase-SpeciesTrophicLevels'
setwd(wd)

fb_troph = read.csv('data/fb_troph.csv')
summary(fb_troph)
fb_troph['source'] = 'Fishbase'

slb_trophs = read.csv('data/slb_trophs.csv')
summary(slb_trophs)
slb_trophs = slb_trophs[,1:5]
slb_trophs['source'] = 'SeaLifeBase'
names(slb_trophs) = names(fb_troph)

troph = rbind(fb_troph, slb_trophs)
troph['scientific'] = sprintf('%s %s', troph$Genus, troph$Species)
sum(duplicated(troph$scientific)) # 0 duplicates

write.csv(troph, 'img/trophs.csv', row.names=F)

# png('img/trophs_hist.png')
# h = hist(troph$EstimateTroph, main='Trophic Levels for FishBase + SeaLifeBase', xlab='Trophic Level Estimate')
# dev.off()
# 
# h1 = h$breaks[1:(length(h$breaks)-1)]
# h2 = h$breaks[2:length(h$breaks)]
# h = data.frame(breaks=sprintf('%g to %g', h1, h2), counts=h$counts)
# write.csv(h, 'img/trophs_hist_0.5breaks.csv', row.names=F)
# 
# 
# png('img/trophs_hist_0.5breaks.png')
# h = hist(troph$EstimateTroph, breaks=seq(min(troph$EstimateTroph, na.rm=T), max(troph$EstimateTroph, na.rm=T), 0.5),
#          main='Trophic Levels for FishBase + SeaLifeBase/nat 0.5 intervals', xlab='Trophic Level Estimate')
# dev.off()
# 
# h1 = h$breaks[1:(length(h$breaks)-1)]
# h2 = h$breaks[2:length(h$breaks)]
# h = data.frame(breaks=sprintf('%g to %g', h1, h2), counts=h$counts)
# write.csv(h, 'img/trophs_hist_0.5breaks.csv', row.names=F)

# read in IUCN spp with SAUP trophics
spp = read.csv('C:/Users/best/Documents/data/model/GL-IUCN-RedList/data/spp.csv', na.strings='')
names(spp)[names(spp)=='rl_category'] = 'status'
names(spp)[names(spp)=='sid_shp'] = 'sid_iucn'
names(spp)[names(spp)=='src_shp'] = 'src_shp_iucn'
names(spp)[names(spp)=='src_xls'] = 'src_xls_iucn'

# IUCN shapefile sources with counts
write.csv(table(spp$src_shp), 'img/spp_src_shp.csv')

# species in troph missing in spp
match = rbind(table(troph[troph$scientific %in% spp$scientific,'source']),
              table(troph[!troph$scientific %in% spp$scientific,'source']))
rownames(match) = c('SAUP Trophic Level in IUCN Assessed', 'SAUP Trophic Level NOT in IUCN Assessed')
write.csv(match, 'img/counts_saup_iucn.csv')
write.csv(troph[troph$scientific %in% spp$scientific,], 'img/troph_notin_iucn.csv')

# prefix colnames and merge (i.)IUCN with (t.)Trophics
names(spp) = sprintf('i.%s', names(spp))
names(troph) = sprintf('t.%s', names(troph))
spp = merge(spp, troph, by.x='i.scientific', by.y='t.scientific', all.x=T, all.y=T)


# read in SAUP list of species with AquaMaps distributions
saup = read.csv('C:/Users/best/Documents/data/ingest/GL-AquaMaps/data/tbl_fish_species.csv')
saup['scientific'] = sprintf('%s %s', saup$genus_name, saup$species_name)
names(saup) = sprintf('s.%s', names(saup))
spp = merge(spp, saup, by.x='i.scientific', by.y='s.scientific', all.x=T, all.y=T)
summary(spp)
names(spp)

# convert SAUP IUCN categories from IUCN Red List Categories & Criteria v2.3 (1994-2000) to v3.1 (since 2001)
# http://en.wikipedia.org/wiki/Wikipedia:Conservation_status
# LR/cd: Least Risk / Conservation Dependent -> NT
# LR/lc: Least Risk / Least Concern -> LC
# LR/cd: Least Risk / Conservation Dependent -> NT
# convert other SAUP IUCN oddities, presumably: NL = not listed = NT; \\N  or N.E. = not evaluated = NE
#paste(levels(spp$s.iucn), collapse="','")
levels.s.iucn.0    = c('\\N','CR','DD','EN','LC','LR/cd','LR/lc','LR/nt','N.E.','NL','NT','VU')
levels(spp$s.iucn) = c('NE' ,'CR','DD','EN','LC','NT'   ,'LC'   ,'NT'   ,'NE'  ,'NE','NT','VU')

UPDATE 

iucn.cats.ok = c('CR','EN','VU','NT','LC')
# NOTE: previously included Data Deficient (DD)
# used:
#   CR = Critically Endangered
#   EN = Endangered
#   VU = Vulnerable
#   NT = Near Threatened
#   LC = Least Concern
# skipped:
#   EX = Extinct
#   EW = Extinct in the wild
#   DD = Data deficient
#   NE = Not evaluated
#   PE = Probably extinct (informal)
#   PEW = Probably extinct in the wild (informal)


# histogram of counts
idx.its = which(( spp$i.rl_category %in% iucn.cats.ok | spp$s.iucn %in% iucn.cats.ok ) &
                ( !is.na(spp$i.src_shp) | !is.na(spp$s.id) ) &
                ( !is.na(spp$t.EstimateTroph) ))

cnts = cbind(table(spp$i.class[( spp$i.rl_category %in% iucn.cats.ok | spp$s.iucn %in% iucn.cats.ok ) & !is.na(spp$i.src_shp) & !is.na(spp$t.EstimateTroph)]),
             table(spp$i.class[( spp$i.rl_category %in% iucn.cats.ok | spp$s.iucn %in% iucn.cats.ok ) & !is.na(spp$s.id) & !is.na(spp$t.EstimateTroph)]),
             table(spp$i.class[ idx.its ]))
cnts
cnts = t(cnts)
rownames(cnts) = c('IUCN Category + IUCN Distribution + SAUP Trophic','IUCN Category + SAUP Distribution + SAUP Trophic','IUCN Category + SAUP or IUCN Distribution + SAUP Trophic')
cnts = as.data.frame(cnts)
png('img/hist_classes_rl_shp-sauporiucn_troph.png', width=1200,height=800)
cols = rainbow(length(rownames(cnts)))
barplot(as.matrix(cnts), col=cols, beside=T, xlab='Taxonomic Class')
labels = sprintf('%s (n=%d)', rownames(cnts), apply(cnts,1,sum))
legend('topright', fill=cols, labels, cex=2)
dev.off()

# write to csv
cnts[['TOTAL']] = apply(cnts,1,sum)
write.csv(cnts, 'img/hist_classes_rl_shp-sauporiucn_troph.csv')

# trophic level hist
spp_troph = spp[idx.its,]
png('img/hist_spp_rl_shp-sauporiucn_troph.png', width=1000,height=800)
hist(spp_troph$t.EstimateTroph,
     main=sprintf('Trophic Levels for Species with IUCN Category and IUCN or SAUP Distribution (n=%d)',nrow(spp_troph)), 
     xlab='Trophic Level Estimate')
dev.off()


png('img/hist_spp_rl_shp-sauporiucn_troph_0.5breaks.png', width=1000,height=800)
h = hist(spp_troph$t.EstimateTroph, breaks=seq(min(spp_troph$t.EstimateTroph, na.rm=T), max(spp_troph$t.EstimateTroph, na.rm=T), 0.5),
         main=sprintf('Trophic Levels for Species with IUCN Category and IUCN or SAUP Distribution (n=%d)',nrow(spp_troph)),
         xlab='Trophic Level Estimate')
dev.off()



###
# OLD... before prefixed names i./t./s. and before SAUP AquaMaps added
# histogram of counts
cnts = cbind(table(spp$class[!is.na(spp$rl_category)]),
             table(spp$class[!is.na(spp$rl_category) & !is.na(spp$popn_trend)]),
             table(spp$class[!is.na(spp$rl_category) & !is.na(spp$popn_trend) & !is.na(spp$src_shp)]),
             table(spp$i.class[!is.na(spp$i.rl_category) & !is.na(spp$i.popn_trend) & !is.na(spp$i.src_shp) & !is.na(spp$t.EstimateTroph)]))
cnts = t(cnts)
rownames(cnts) = c('IUCN Category','+IUCN Popn Trend','+IUCN Distribution','+SAUP Trophic Estimate')
cnts = as.data.frame(cnts)
png('img/hist_classes_rl_shp_troph.png', width=1200,height=800)
cols = rainbow(length(rownames(cnts)))
barplot(cnts, col=cols, beside=T, xlab='Taxonomic Class')
labels = sprintf('%s (n=%d)', rownames(cnts), apply(cnts,1,sum))
legend('topright', fill=cols, labels, cex=2)
dev.off()

# write to csv
cnts[['TOTAL']] = apply(cnts,1,sum)
write.csv(cnts, 'img/hist_classes_rl_shp_troph.csv')

spp_troph = spp[!is.na(spp$rl_category) & !is.na(spp$popn_trend) & !is.na(spp$src_shp) & !is.na(spp$EstimateTroph),]
png('img/hist_spp_rl_shp_troph.png', width=1000,height=800)
hist(spp_troph$EstimateTroph,
     main=sprintf('Trophic Levels for Species with IUCN Assessment, Category, Distribution (n=%d)',nrow(spp_troph)), 
     xlab='Trophic Level Estimate')
dev.off()
