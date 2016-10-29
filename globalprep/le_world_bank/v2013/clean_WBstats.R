# clean_BWstats.r: reformat and add ISO codes to World Bank statistics (by JStewart May2013)
#   Data: 
#         GDP = Gross Domestic Product (current $USD)
#         LAB = Labor force, total (# people)
#         UEM = Unemployment, total (% of total labor force)
#         PPP = Purchase power parity
#         PPPpcGDP = GDP adjusted per capita by PPP     
#         POP = Total population count
                                  
#   call add_rgn_id.r to add 2013 OHI region_ids
#   georegional gapfilling with add_gapfill.r
#   translation from rgn_id to cntry_id and country_id: scroll way down, there is a lot...

library(reshape2)
library(gdata)
library(plyr)
options(max.print=5E6)
source('/Volumes/local_edit/src/R/jstewart/ohi_clean_fxns.R')

dir1 = ('/Volumes/data_edit/model/GL-WorldBank-Statistics_v2012')
setwd(file.path(dir1, 'raw'))

# read in files
d.all =  matrix(nrow=0, ncol=0)
for (f in list.files(pattern=glob2rx('*reformatted.csv'))){
  
  d = read.csv(f,check.names=F) # this will keep R from adding the stupid X in front of the numeric column names)
  
  # remove final year column if no data
  aa = dim(d)[1] - sum(is.na(d[,dim(d)[2]]))
  if(aa == 0) {
    d = d[,-dim(d)[2]]
  }
  
  # remove any countries that have no data for the whole dataset:
  d.1 = matrix(nrow=0, ncol=0)
  for(i in 1:dim(d)[1]){             # d = d[complete.cases(d),] # suboptimal: this removes anytime there are missing values
    bb = dim(d)[2] - sum(is.na(d[i,]))
    if(bb != 2) { # this means just the countryname and country code name are not NA
      d.1 = rbind(d.1,d[i,])
    }
  }
  
  d.m = melt(data=d.1, id.vars=names(d.1)[1:2], variable.name='year')
  names(d.m) = c('country','country_codeWB','year','value_num')
  
  # add layer column
  a = strsplit(f, '_', fixed=TRUE)
  d.m$layer = rep.int(unlist(a)[2], length(d.m$value)) 
  d.m$units = rep.int(unlist(a)[3], length(d.m$value))
  
  # concatenate f files
  d.all = rbind(d.all, d.m)
}

# remove Channel Islands and Isle of Man:
d.all <- d.all[d.all[,1] != "Channel Islands",] # remove Channel Islands
d.all <- d.all[d.all[,1] != "Isle of Man",] # remove Isle of Man

# prep as add_rgn_id expects
d.all2 = d.all[c('country','value_num','year','layer','units')]

d.all3 = d.all2[order(d.all2$layer, d.all2$country,  d.all2$year),]

# Print out all the unique indicators
print('these are all the variables that are included in the cleaned file: ')
print(data.frame(unique(d.all3$layer)))


## run add_rgn_id and save
uifilesave = file.path(dir1, 'data', 'GL-WorldBank-Statistics_v2012-cleaned.csv')
#paste0(dir1, 'data', 'file.csv')
#sprintf('%s/%s/img_%03d.img', dir1, 'data', 80)
add_rgn_id(d.all3, uifilesave)


# GAP FILLING ----
## georegional gapfilling with add_gapfill.r

# save files as 2013a. saving as 2012a is below. 
cleaned_data1 = read.csv(uifilesave)
layer_uni = unique(cleaned_data1$layer)
dx = which(layer_uni == 'POP'); # deal with population (POP) separately, below.
layer_uni = layer_uni[-dx] # deal with population (POP) separately, below.
layernames = sprintf('rgn_wb_%s_2013a.csv', tolower(layer_uni))
s_island_val = NA # assign what southern islands will get. this could be something fancier, depending on the dataset. 

for(k in 1:length(layer_uni)) { # k=1
  cleaned_layer = cleaned_data1[cleaned_data1$layer == layer_uni[k],]
  cleaned_layer$layer = NULL; head(cleaned_layer)
  names(cleaned_layer)[3] = as.character(cleaned_layer$units[2])
  cleaned_layer$units = NULL; tail(cleaned_layer)
  
  # save 2013a files
  layersave = file.path(dir1, 'data', layernames[k]) 
  # layersave = sprintf('%s/data/%s.csv', dir1, layernames[k]); basename(file_path_sans_ext(layersave)); dirname(layersave)
  # sprintf('%s/data/%s.csv', dir1, layernames) 

  cleaned_layer$rgn_nam = NULL
  
  cleaned_layert = temporal.gapfill(cleaned_layer, fld.id = 'rgn_id', fld.value = names(cleaned_layer)[2], fld.year = 'year', verbose=F); head(cleaned_layert) 
  cleaned_layert2 = cleaned_layert; cleaned_layert2$whence = NULL; cleaned_layert2$whence_details = NULL
  add_gapfill(cleaned_layert2, layersave, s_island_val)
  
} 

# and then save 2012a ----
layernames2012 = sprintf('rgn_wb_%s_2012a.csv', tolower(layer_uni))
for(k in 1:length(layer_uni)) { # k=3 
  
  f = read.csv(paste(dir1, 'data/', layernames[k], sep = '')) #these are the 2013a files created above
  
  f_2012 = f[f$year != max(f$year, na.rm=T),]
  write.csv(f_2012, paste(dir1, 'data/', layernames2012[k], sep=''), na = '', row.names=FALSE)
}

# change gdp, uem, ppp, lab from rgn to country

# layernames_rgn = c('rgn_wb_lab_2012a.csv', 'rgn_wb_lab_2013a.csv', 'rgn_wb_uem_2012a.csv', 'rgn_wb_uem_2013a.csv', 'rgn_wb_ppp_2012a.csv', 'rgn_wb_ppp_2013a.csv')
# layernames_cntry = c('cntry_wb_lab_2012a.csv', 'cntry_wb_lab_2013a.csv', 'cntry_wb_uem_2012a.csv', 'cntry_wb_uem_2013a.csv', 'cntry_wb_ppp_2012a.csv', 'cntry_wb_ppp_2013a.csv')
fpath = '/Volumes/data_edit/model/GL-WorldBank-Statistics_v2012/data'
layers = c('rgn_wb_gdp_2012a.csv' = 'cntry_wb_gdp_2012a.csv', 
           'rgn_wb_gdp_2013a.csv' = 'cntry_wb_gdp_2013a.csv',
           'rgn_wb_lab_2012a.csv' = 'cntry_wb_lab_2012a.csv', 
           'rgn_wb_lab_2013a.csv' = 'cntry_wb_lab_2013a.csv',
           'rgn_wb_ppp_2012a.csv' = 'cntry_wb_ppp_2012a.csv',
           'rgn_wb_ppp_2013a.csv' = 'cntry_wb_ppp_2013a.csv',
           'rgn_wb_uem_2012a.csv' = 'cntry_wb_uem_2012a.csv',
           'rgn_wb_uem_2013a.csv' = 'cntry_wb_uem_2013a.csv' 
)

fld.value = c('USD', 'USD', 'count', 'count', 'LCU', 'LCU', 'percent', 'percent')


cntry_rgn_2013 = read.csv('/Volumes/data_edit/model/GL-NCEAS-OceanRegions_v2013a/manual_output/cntry_rgn_2013.csv')
for(k in length(layers)) { # k=1
  csv.in  = names(layers)[k]
  csv.out = layers[k]
  
  f = read.csv(file.path(fpath, csv.in)) #these are the files created above
  
  a = merge(x = f, y = cntry_rgn_2013, by.x = 'rgn_id', by.y = 'rgn_id', all.x = T); head(a); summary(a)  
  ####problem here bc gives the value for rgn_id to all associated; so to both Trindade and Brazil
  a.duplicated = a[a$rgn_id %in% a[duplicated(a[,c('rgn_id','year')]),'rgn_id'], ]
  table(a.duplicated$rgn_id, as.character(a.duplicated$cntry_key))
  #       Alaska BRA CHL CHN Easter Island ECU Galapagos Islands GLP GUM Hawaii MNP MTQ PRI Trindade USA VIR
  #   13       0   0   0   0             0   0                 0   0  21      0  21   0   0        0   0   0
  #   116      0   0   0   0             0   0                 0   0   0      0   0   0  42        0   0  42
  #   137      0   0   0   0             0  21                21   0   0      0   0   0   0        0   0   0
  #   140      0   0   0   0             0   0                 0  21   0      0   0  21   0        0   0   0
  #   163     21   0   0   0             0   0                 0   0   0     21   0   0   0        0  21   0
  #   171      0  21   0   0             0   0                 0   0   0      0   0   0   0       21   0   0
  #   209      0   0   0  63             0   0                 0   0   0      0   0   0   0        0   0   0
  #   224      0   0  21   0            21   0                 0   0   0      0   0   0   0        0   0   0
  
  a1 = a[,c(4,3,2)]
  
  write.csv(a1, file.path(fpath, csv.out), na = '', row.names=FALSE)
}



## translate from rgn_id 2013 to country_2012 ----
# convert from rgn_id 2013 to country_2012 for wages multipliers
library(plyr)

# PPP ----
# PPP!! And now make  rgn_wb_ppp_2013a.csv -> country_wb_ppp_2013a.csv # simpler than cntry_wb_ppp_2013a.csv -> country_wb_ppp_2013a.csv Sept 16 
p = read.csv('/Volumes/data_edit/model/GL-WorldBank-Statistics_v2012/data/rgn_wb_gdppcppp_2013a.csv'); head(p)
names(p)[2] = 'LCUpUSD';head(p)

# lumpers: remove duplicates by summing LCUpUSD across all uniquely identified columns
p[duplicated(p[,c('rgn_id','year')]),]
p = ddply(p, .(rgn_id, year), summarize, LCUpUSD = mean(LCUpUSD, na.rm=T)) #average!!

# translate new from rgn_id_2013 to country_id_2012 using a lookup:
country12_rgn13 = read.csv('/Volumes/data_edit/model/GL-NCEAS-OceanRegions_v2013a/manual_output/rgn2013_to_country2012_oneUSA.csv'); head(country12_rgn13)
print(subset(country12_rgn13, country_id %in% c('Alaska','Hawaii','USA')), row.names=F)
#  rgn_id country_id cntry_key duplicated_rgn country_not_cntry
#     163     Alaska    Alaska              1                NA
#     163     Hawaii    Hawaii              1                NA
#     163        USA       USA              1                NA

p_m = merge(x=p, by.x='rgn_id',
             y=country12_rgn13, by.y='rgn_id',
             all.x=T); head(p_m); summary(p_m)

# splitters: ok b/c children = parent
head(subset(p_m, duplicated_rgn==1))

# lumpers again from multiple rgn_id per single country_id
table(p_m[duplicated(p_m[,c('country_id','year')]), 'country_id'])
# ANT MYS 
# 132  33
p_m = ddply(p_m, .(country_id, year), summarize, LCUpUSD = mean(LCUpUSD, na.rm=T)) #average!!

# write file
write.csv(arrange(p_m, country_id, country_id, year), '/Volumes/data_edit/model/GL-WorldBank-Statistics_v2012/data/country_wb_gdppcppp_2013a.csv', row.names=F, na='')


# GDP ----
# GDP!! And now make  rgn_wb_ppp_2013a.csv -> country_wb_ppp_2013a.csv # simpler than cntry_wb_ppp_2013a.csv -> country_wb_ppp_2013a.csv Sept 16 
p = read.csv('/Volumes/data_edit/model/GL-WorldBank-Statistics_v2012/data/rgn_wb_gdp_2013a.csv'); head(p)

# lumpers: remove duplicates by summing LCUpUSD across all uniquely identified columns
p[duplicated(p[,c('rgn_id','year')], fromLast=F) | duplicated(p[,c('rgn_id','year')], fromLast=T),]
p = ddply(p, .(rgn_id, year), summarize, USD = sum(USD, na.rm=T)) ##sum!!

# translate new from rgn_id_2013 to country_id_2012 using a lookup:
country12_rgn13 = read.csv('/Volumes/data_edit/model/GL-NCEAS-OceanRegions_v2013a/manual_output/rgn2013_to_country2012.csv'); head(country12_rgn13)

p_m = merge(x=p, by.x='rgn_id',
             y=country12_rgn13, by.y='rgn_id',
             all.x=T); head(p_m); summary(p_m)

# splitters: children != parents, so split original rgn value by coastal population into country value
table(subset(p_m, duplicated_rgn==1, country_id))
#            Alaska               BRA               CHL     Easter Island               ECU Galapagos Islands 
#                52                52                52                52                52                52 
#               GLP               GUM            Hawaii               MNP               MTQ               PRI 
#                52                52                52                52                52                52 
#          Trindade               USA               VIR 
#                52                52                52
# MNP=GUM, PRI=VIR, ECU=Galapagos Islands, GLP=MTQ, USA=Alaska=Hawaii, BRA=Trindade, Easter Island=CHL
cp = read.csv('/Volumes/data_edit/model/GL-NCEAS-CoastalPopulation_v2013/data/cntry_popsum2013_inland25mi_complete.csv', na.strings=''); head(cp)
cp.rgnsum = ddply(cp, .(rgn_id), summarize, rgn_popsum2013_inland25mi = sum(cntry_popsum2013_inland25mi, na.rm=T)); head(cp.rgnsum)
cp = merge(cp, cp.rgnsum, by.x='rgn_id', by.y='rgn_id', all.x=T)
cp$cp_ratio = with(cp, cntry_popsum2013_inland25mi/rgn_popsum2013_inland25mi); head(cp)
p.country = merge(p_m, subset(cp, cntry_key %in% p_m[p_m$duplicated_rgn==1,'cntry_key'], c(cntry_key, cp_ratio)), by.x='cntry_key', by.y='cntry_key', all.x=T); head(p.country); dim(p_m); dim(p.country)
p.country[is.na(p.country$cp_ratio),'cp_ratio'] = 1
p.country$USD = with(p.country, USD * cp_ratio)
subset(p.country, year==2000 & rgn_id==163)

# lumpers (ANT, MYS): sum
table(subset(p.country, country_not_cntry==1, country_id))
# ANT MYS 
# 226  99
table(p.country[duplicated(p.country[,c('country_id','year')], fromLast=F) |
                  duplicated(p.country[,c('country_id','year')], fromLast=T), 'country_id'])
# ANT MYS 
# 226  94 
p.lumped = ddply(p.country, .(country_id, year), summarize, USD = sum(USD, na.rm=T)) ##sum!!
p.lumped = arrange(p.lumped, country_id, year)

# write file
write.csv(p.lumped, '/Volumes/data_edit/model/GL-WorldBank-Statistics_v2012/data/country_wb_gdp_2013a.csv', row.names=F, na='')



# -----
# Make Employment EMP Data File ----

torun = c('2012a', '2013a') 
for(r in torun) {
  
  # for 2012a:
  if(r == '2012a'){
    uem = read.csv(file.path(fpath,'cntry_wb_uem_2012a.csv')); head(uem); dim(uem)
    lab = read.csv(file.path(fpath,'cntry_wb_lab_2012a.csv')); head(lab); dim(lab)
    csv.out = 'cntry_wb_emp_2012a.csv'
    
    # for 2013a:
  } else if (r == '2013a'){
    
    uem = read.csv(file.path(fpath,'cntry_wb_uem_2013a.csv')); head(uem); dim(uem)
    lab = read.csv(file.path(fpath,'cntry_wb_lab_2013a.csv')); head(lab); dim(lab)
    csv.out = 'cntry_wb_emp_2013a.csv'
    
  }
  
  emp = merge(x=lab, y = uem, by.x = c('cntry_key', 'year'), all.x = T); head(emp)
  emp = na.omit(emp)
  names(emp)[3:4] = c('tot_lab_count', 'percent_uem'); head(emp)
  emp$percent_emp = (100 - emp$percent_uem); head(emp)
  emp$employed_count = (emp$percent_emp * emp$tot_lab_count)/ 100; head(emp)
  
  emp2 = emp[c('cntry_key','year','percent_emp')]
  names(emp2)[3] = 'percent'; head(emp2)
  write.csv(emp2, file.path(fpath, csv.out), na = '', row.names=FALSE)
  
}

# -----
# POP: treated separately.prepare file for BH to gap-fill by hand
cleandata = read.csv('GL-WorldBank-Statistics_v2012-cleanedOnlyPOP.csv',check.names=F); head(cleandata)

# read in master OHI list with 2letter code; region ids
rk = read.csv('/Volumes/data_edit/model/GL-NCEAS-OceanRegions_v2013a/manual_output/rgn_eez_v2013a.csv')

# read in gap-filling UN region document
gf = read.csv('/Volumes/data_edit/model/GL-NCEAS-OceanRegions_v2013a/manual_output/rgn_un_georegions.csv')

# indicate which ohi_regions still need to be gap-filled
rkx = rk[rk$rgn_id_2013 < 255,]# remove high seas and non-regions
rk_uni = unique(rkx)
rgn_tofill = sqldf("SELECT a.rgn_id_2013, a.rgn_nam_2013
                 FROM rk_uni AS a
                 LEFT OUTER JOIN (
                     SELECT DISTINCT rgn_id_2013
                     FROM cleandata
                     ) AS b ON b.rgn_id_2013 = a.rgn_id_2013
                  WHERE b.rgn_id_2013 IS null") 

write.csv(rgn_tofill, 'GL-WorldBank-Statistics_POPtofill.csv')

## then, BH fills in these missing countries by hand--read that file in and concatenate. 
p = read.csv(paste(dir1, 'raw/', 'missing_total_pop_data_BH.csv', sep=''))
p1 = p
p1$rgn_nam_2013 = NULL
names(p1) = c('rgn_id', 'count', 'year')
p1$year[is.na(p1$year)] = 2010 # give a year for the only pop value for this country so can have value for 2013 and 2012.  

cleandata$layer = NULL
cleandata$rgn_nam_2013 = NULL
names(cleandata)[c(1,2)] = c('rgn_id', as.character(cleandata$units[2]))
cleandata$units = NULL

# temporal gapfill population? no need. good data.

pclean = rbind(cleandata, p1)
pclean$rgn_id = as.numeric(pclean$rgn_id)
pclean$year = as.numeric(pclean$year)
pclean = pclean[order(pclean$rgn_id, pclean$year),]
pclean = na.omit(pclean)

# BB checking corrections via KL 2013-08-30
p  = read.csv('/Volumes/data_edit/model/GL-WorldBank-Statistics_v2012/data/rgn_wb_pop_2013a.csv'); head(p); summary(p); dim(p)
#      rgn_id          count                year     
#  Min.   :  1.0   Min.   :0.000e+00   Min.   :1960  
#  1st Qu.: 65.0   1st Qu.:2.997e+05   1st Qu.:1973  
#  Median :124.0   Median :3.658e+06   Median :1986  
#  Mean   :122.8   Mean   :2.749e+07   Mean   :1986  
#  3rd Qu.:185.0   3rd Qu.:1.611e+07   3rd Qu.:2000  
#  Max.   :250.0   Max.   :1.351e+09   Max.   :2013  
#  NA's   :106     NA's   :80                        

# TODO: remove duplicates of rgn_id & year
unique(p$rgn[p$rgn %in% p$rgn[duplicated(p[,c('rgn_id','year')])]])
# 13, 116, 209

# save pop for 2013a
popname = 'rgn_wb_pop_2013a.csv'
write.csv(pclean, paste(dir1, 'data/', popname, sep=''), , na = '', row.names=FALSE)

#### Now, save 2012a file for POP. 
popname2012 = 'rgn_wb_pop_2012a.csv'

## come back here for the logic of how to make the 2012a pop file. 
puni = unique(pclean$rgn_id)
for(r in puni){ # r = 1
  x = pclean[pclean[,1] == r,]
  if(dim(x)[1] > 1){
    x = x[x$year != max(x$year, na.rm=T),]  
  }
  if (!exists('y')){
    y = x
  } else {
    y = rbind(y, x)
  }
}
write.csv(y, paste(dir1, 'data/', popname2012, sep=''), na = '', row.names=FALSE)


##
##
##


## whence tracking; May/June 2014 ---- 
devtools::install_github('ohi-science/ohicore')
dir_neptune_data = c('Windows' = '//neptune/data_edit',
                     'Darwin'  = '/Volumes/data_edit',
                     'Linux'   = '/var/data/ohi')[[ Sys.info()[['sysname']] ]]
dir_root = file.path('/Users', Sys.info()[['user']])
source(file.path(dir_root, 'github/ohiprep/src/R/ohi_clean_fxns.R'))
library(dplyr)



## check for duplicate regions, sum them ----

# explore; identify dups
dclean = read.csv(uifilesave); head(dclean)
d.dup = dclean[duplicated(dclean[,c('rgn_id', 'year', 'layer', 'units')]),]; head(d.dup)
dup_ids = unique(d.dup$rgn_id) # 13, 116, 209, NA
filter(dclean, rgn_id == 13, year == 1960)
filter(dclean, rgn_id == 116, year == 1960)
filter(dclean, rgn_id == 209, year == 1960)

# remove duplicates with sum_duplicates.r from ohi_clean_fxns.R
d_fix = sum_duplicates(dclean, dup_ids, fld.nam = 'value_num'); head(d_fix)

# confirm
filter(d_fix, rgn_id == 13, year == 1960)
filter(d_fix, rgn_id == 116, year == 1960)
filter(d_fix, rgn_id == 209, year == 1960)


## georegional gapfilling with add_gapfill.r ----

# remove anything without a rgn_id after confirming that the list printed does not contain any actual regions
d.2 = d_fix; tail(d.2)
unique(d.2$rgn_nam[is.na(d.2$rgn_id)])
d.2 = d.2[!is.na(d.2$rgn_id),]; tail(d.2)

# save files as 2013a (population data hasn't been updated so no special considerations. See clean_WB gapfilling for those).
layer_uni = unique(d.2$layer)
layernames = sprintf('rgn_wb_%s_2013a_whence', tolower(layer_uni))
s_island_val = NA # assign what southern islands will get. this could be something fancier, depending on the dataset. 
dirsave = file.path(dir1, 'data')

detach("package:dplyr")
library(plyr)
for(k in 1:length(layer_uni)) { # k=1
  cleaned_layer = d.2[d.2$layer == layer_uni[k],]
  cleaned_layer$layer = NULL; head(cleaned_layer)
  names(cleaned_layer)[5] = as.character(cleaned_layer$units[2])
  cleaned_layer$units = NULL; tail(cleaned_layer)
  
  # save 2013a files
  layersave = layernames[k]
  cleaned_layer$rgn_nam = NULL; tail(cleaned_layer)
  
  cleaned_layert = temporal.gapfill(cleaned_layer, fld.id = 'rgn_id', fld.value = names(cleaned_layer)[2], fld.year = 'year', verbose=T); head(cleaned_layert) 
  cleaned_layert2 = cleaned_layert; cleaned_layert2$whence = NULL; cleaned_layert2$whence_details = NULL
  add_gapfill(cleaned_layert2, dirsave, layersave, s_island_val, dpath = file.path(dir_root, 'github/ohiprep/src/LookupTables'))
} 

# using BB's gapfilling tracking

library(reshape2)
library(dplyr)
georegions = read.csv('/Volumes/data_edit/model/GL-NCEAS-OceanRegions_v2013a/manual_output/rgn_georegions_long_2013b.csv', na.strings='') %.%
  dcast(rgn_id ~ level, value.var='georgn_id')

rgn_labels = read.csv('/Volumes/data_edit/model/GL-NCEAS-OceanRegions_v2013a/data/rgn_details.csv') %.%
  filter(rgn_typ=='eez') %.%
  select(rgn_id, rgn_label = rgn_nam)


gl = read.csv('/Volumes/data_edit/model/GL-NCEAS-OceanRegions_v2013a/manual_output/rgn_georegions_labels_long_2013b.csv', na.strings='')
georegion_labels = gl  %.%    
  mutate(level_label = sprintf('%s_label', level)) %.%
  dcast(rgn_id ~ level_label, value.var='label') %.%
  left_join(
    rgn_labels %.%
      select(rgn_id, v_label=rgn_label),
    by='rgn_id')


# JS code

for(k in 1:length(layer_uni)) { # k=1
  cleaned_layer = d.2[d.2$layer == layer_uni[k],]
  cleaned_layer$layer = NULL; head(cleaned_layer)
  #names(cleaned_layer)[5] = as.character(cleaned_layer$units[2])
  cleaned_layer$units = NULL; tail(cleaned_layer)
  
  # save 2013a files
  layersave = layernames[k]
  cleaned_layer$rgn_nam = NULL; tail(cleaned_layer)
  d = cleaned_layer
  
  # setup data for georegional gapfilling (remove Antarctica rgn_id=213)
  
  # method 1: install from github
  devtools::install_github('ohi-science/ohicore')
  library(ohicore)
  # method 2: load dev version locally
  #library(devtools)
  #load_all('../ohicore')  
  ?gapfill_georegions
  d_g = gapfill_georegions(
    data = d %.%
      filter(rgn_id!=213) %.%
      select(rgn_id, year, value_num),
    georegions = georegions,
    georegion_labels = georegion_labels) %.%
    arrange(rgn_id, year)
  
  # inspect gapfill attributes table
  # names(attributes(d_g))
  z = attr(d_g, 'gapfill_georegions')
  
  # compare with original 
  o = read.csv(sprintf('%s/data/%s.csv', dir1, layernames[k]))
  head(d_g)
  head(o)
  
  m = d_g %.%
    select(year, rgn_id, val = value_num) %.%
    merge(
      o %.%
        select(year, rgn_id, val_o = value_num, whencev01, whence_choice),
      by=c('year', 'rgn_id'), all=T) %.%
    mutate(
      val_dif    = val - val_o,
      val_notna  = is.na(val)!=is.na(val_o)) %.%
    filter(abs(val_dif) > 0.01 | val_notna == T) %.%
    arrange(desc(val_notna), desc(abs(val_dif)), rgn_id, year) %.%
    select(rgn_id, year, val_o, val, val_dif) %.%
    merge(
      z,
      by.x=c('year','rgn_id'),
      by.y=c('yr', 'id')) # %.% filter(z_level=='r0')
        
  
  # check to see r0 gapfilling # only Southern Islands were gapfilled with r0
  m_r0 = m %.%
    filter(z_level == 'r0')
  
  layer_uni
  
  # compare old vs new scores
      vs = scores %.%
        merge(scores_old, by=c('goal','dimension','region_id'), all=T) %.%
        merge(rgn_labels, by='region_id', all.x=T) %.%
        mutate(
          score_dif    = score - score_old,
          score_notna  = is.na(score)!=is.na(score_old)) %.%  
        filter(abs(score_dif) > 0.01 | score_notna == T) %.%
        arrange(goal, desc(dimension), desc(score_notna), desc(abs(score_dif))) %.%
        select(goal, dimension, region_id, region_label, score_old, score, score_dif)
      csv = sprintf('%s/git-annex/Global/NCEAS-OHI-Scores-Archive/scores/scores.%s_%s_2013-10-09-dif.csv', dir_conf$neptune_data, scenario, format(Sys.Date(), '%Y-%m-%d'))
      write.csv(vs, csv, row.names=F, na='')
  

}