require('RODBC')

options(stringsAsFactors=FALSE)
dir.in  = 'C:/Users/best/Documents/data/raw/GL-IUCN-RedList/data'
dir.out = 'C:/Users/best/Documents/data/model/GL-IUCN-RedList/data'

# TODO: popn_trend numeric in Marine_spp_for_OHI.xlsx and character in others

# setup data frame
d.all = data.frame(scientific=character(0),rl_category=character(0),popn_trend=character(0),src_xls=character(0),
  		       kingdom=character(0),phylum=character(0),class=character(0),order=character(0),family=character(0))

xls = list(list(file='Marine_spp_for_OHI.xlsx',
				tab='Sheet1',
				renames=list('populationtrend'='popn_trend',
							 'friendly_name'='scientific')),
		   list(file='Hagfish_export_RL.xlsx',
				tab='Hagfish_export_RL',
				renames = list('red list status'='rl_category',
							   'population trend'='popn_trend')),
		   list(file='SeaSnake_export_RL.xlsx',
				tab='SeaSnake_export_RL',
				renames = list('red list status'='rl_category',
							   'population trend'='popn_trend')),
		   list(file='Tuna_Billfishes_TaxaList.xlsx',
				tab='Tuna_Billfishes_TaxaList',
				renames = list('category'='rl_category',
							   'population trend'='popn_trend')))
		

for (i in 1:length(xls)){ # i=4
	x = xls[[i]]
	f = sprintf('%s/%s', dir.in, x[['file']])
	con = odbcConnectExcel2007(f) #sqlTables(con) # odbcCloseAll()
	d = sqlFetch(con,sprintf('%s$',x[['tab']])) #summary(dat)
	#for (f in names(dat.ohi)){
	  #cat(sprintf('%s=%s(0),',f, class(dat.ohi[[f]])))
	#}
	d['src_xls']=rep(x[['file']], nrow(d))

	names(d) = tolower(names(d))
	n = x[['renames']]
	for (j in 1:length(n)){
		names(d)[names(d)==names(n[j])] = n[[j]]
	} # names(d)
	
	if (!'scientific' %in% names(d)){
		if ('subspecies' %in% names(d)){
			d['scientific'] = gsub('(^ +)|( +$)', '', sprintf('%s %s %s',d$genus,d$species,d$subspecies))
		} else {
			d['scientific'] = gsub('(^ +)|( +$)', '', sprintf('%s %s',d$genus,d$species))
		}
	}

	for (n in names(d.all)){
		if (!n %in% names(d)){
			stop(sprintf('column "%s" missing from %s',n,x[['file']]))
		}	
	}
	
	d$popn_trend = tolower(d$popn_trend)
	popn_renames=list('0'='extinct','1'='increasing','2'='decreasing','3'='stable','4'='unknown')
	for (j in 1:length(popn_renames)){
		fro=names(popn_renames)[j]
		to=popn_renames[[j]]
		d$popn_trend[d$popn_trend==fro]=to
	}

	d = subset(d, select=names(d.all))
	d.all = rbind(d.all,d)
}
table(d.all$popn_trend) # 0=Extinct, 1=Increasing, 2=Decreasing, 3=Stable, 4=Unknown
table(d.all$rl_category)

# > table(d.all$popn_trend)
#          0          1          2          3          4 decreasing Decreasing 
#          2         25        702        378       1962         12         16 
# Increasing     stable     Stable    unknown    Unknown 
#          1          9         19        125         28 
# 
# FIXED...
# > table(d.all$popn_trend) # 0=Extinct, 1=Increasing, 2=Decreasing, 3=Stable, 4=Unknown
# 
# decreasing    extinct increasing     stable    unknown 
#        730          2         26        406       2115 
# 
# > table(d.all$rl_category)
#    CR    DD    EN    EX    FF    LC LR/nt    NT    VU 
#    45   882    91     3     1  1502     1   371   390
# 
# CR=0.8, critically endangered
# EN=0.5, endangered
# VU=0.3, vulnerable
# LC=0.1, least concern
# NT=0.01, not threatened # reduces average if species present
# DD=NA, data deficient
# ?:EX,FF,LR/nt

d.order = with(d.all, order(kingdom,phylum,class,order,family,scientific))
d.all = d.all[d.order,]
idx = which(!d.all$rl_category %in% c('CR','EN','VU','LC','NT','DD')) # ,'EX','FF','LR/nt',,) 

spp_rl.csv = sprintf('%s/spp_rl.csv', dir.out)
f = sprintf('%s/spp_rl.csv', dir.out)
cat(sprintf('%s: %d\n',basename(f),nrow(d.all[-idx,])))
write.csv(d.all[-idx,], spp_rl.csv, row.names=F)
f = sprintf('%s/spp_rl_oddcategories.csv', dir.out)
cat(sprintf('%s: %d\n',basename(f),nrow(d.all[idx,])))
write.csv(d.all[idx,], f, row.names=F)


spp_shp.csv = sprintf('%s/spp_shp.csv', dir.out)
d.rl = read.csv(spp_rl.csv)
d.shp = read.csv(spp_shp.csv)
names(d.shp) = c('oid','sid_shp','src_shp','scientific')
cat(sprintf('%s: %d\n',basename(spp_shp.csv),nrow(d.shp)))

d = merge(d.rl,d.shp,by='scientific',all=TRUE)
d.order = with(d, order(kingdom,phylum,class,order,family,scientific))
d = d[d.order,]

f = sprintf('%s/spp.csv', dir.out)
cat(sprintf('%s: %d\n',basename(f),nrow(d)))
write.csv(d, f, row.names=F, na='')

idx = which(!is.na(d$src_xls) & !is.na(d$src_shp))
f = sprintf('%s/spp_rl_andhas_shp.csv', dir.out)
cat(sprintf('%s: %d\n',basename(f),nrow(d[idx,])))
write.csv(d[idx,], f, row.names=F, na='')

idx = which(!is.na(d$src_xls) & is.na(d$src_shp))
f = sprintf('%s/spp_rl_notin_shp.csv', dir.out)
cat(sprintf('%s: %d\n',basename(f),nrow(d[idx,])))
write.csv(d[idx,], f, row.names=F, na='')

idx = which(is.na(d$src_xls) & !is.na(d$src_shp))
f = sprintf('%s/spp_shp_notin_rl.csv', dir.out)
cat(sprintf('%s: %d\n',basename(f),nrow(d[idx,])))
write.csv(d[idx,], f, row.names=F, na='')