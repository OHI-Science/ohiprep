# data_prep.R. 
# Reformat and add rgn_ids for World Bank statistics data
# Previously had been named clean_BWstats.r (by JStewart May2013). This script was created by JStewartLowndes Mar2014 with improved functions by BBest in Jun2014
#   Data: 
#         GDP = Gross Domestic Product (current $USD)
#         LAB = Labor force, total (# people)
#         UEM = Unemployment, total (% of total labor force)
#         PPP = Purchase power parity
#         PPPpcGDP = GDP adjusted per capita by PPP     
#         POP = Total population count

#   add OHI region_ids with name_to_rgn_id.r  ** differs from data_prep.old
#   georegional gapfilling with gapfill_georegions.R ** differs from data_prep.old

# setup ----


# from get paths configuration based on host machine name
source('src/R/common.R') # set dir_neptune_data; load reshape2, plyr, dplyr
source('src/R/ohi_clean_fxns.R') # get functions
dir_d = 'Global/WorldBank-Statistics_v2012'

# load libraries
library(gdata)
# devtools::install_github('ohi-science/ohicore') # may require uninstall and reinstall
library(ohicore)  # for github/ohicore/R/gapfill_georegions.R

# read in and process files ----
d.all =  matrix(nrow=0, ncol=0)
count = 0
for (f in list.files(path = file.path(dir_d, 'raw'), pattern=glob2rx('*xls'), full.names=T)) {  # f = "Global/WorldBank-Statistics_v2012/raw/sl.uem.totl.zs_Indicator_en_excel_v2.xls"
  count = count + 1
  #d = read.xls(file.path(dir_d, 'raw', f), sheet=1, skip=1, check.names=F) # do not add the stupid X in front of the numeric column names
  d = read.xls(f, sheet=1, skip=1, check.names=F);  head(d) # do not add the stupid X in front of the numeric column names
  
  # remove final year column if it is completely NAs
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
  
  d.1 = d.1[,-(2:4)] # get rid of extra columns in .xls sheet
  d.m = melt(data=d.1, id.vars=names(d.1)[1], variable.name='year')
  names(d.m) = c('country','year','value'); head(d.m)
  
  # add layer column
  #a = strsplit(f, '.', fixed=TRUE)
  a = strsplit(basename(f), '.', fixed=TRUE) # tools::file_path_sans_ext(basename(f))
  d.m$layer = rep.int(unlist(a)[2], length(d.m$year)) 
  lkup = c('gdp' = 'usd', 
           'tlf' = 'count',
           'uem' = 'percent') # names(lkup)[count]
  d.m$units = rep.int(lkup[count], length(d.m$year)); head(d.m)
  
  # concatenate f files
  d.all = rbind(d.all, d.m)
}

# remove Channel Islands and Isle of Man
d.all <- d.all[d.all[,1] != "Channel Islands",] # remove Channel Islands
d.all <- d.all[d.all[,1] != "Isle of Man",] # remove Isle of Man

# Print out all the unique indicators
print('these are all the variables that are included in the cleaned file: ')
print(data.frame(unique(d.all$layer)))

# add rgn_id: country to rgn_id  # source('src/R/ohi_clean_fxns.R') ----
m_d = name_to_rgn(d.all, fld_name='country', flds_unique=c('country','year','layer', 'units'), fld_value='value', add_rgn_name=T) 

m_d = m_d %.%
  select(rgn_id, layer, units, year, value) %.%
  arrange(layer, units, rgn_id, year)

## georegional gapfilling with gapfill_georegions.r ----

# read in lookups
georegions = read.csv('../ohicore/inst/extdata/layers.Global2013.www2013/rgn_georegions_long_2013b.csv', na.strings='') %.%
  dcast(rgn_id ~ level, value.var='georgn_id')

georegion_labels = read.csv('../ohicore/inst/extdata/layers.Global2013.www2013/rgn_georegions_labels_long_2013b.csv') %.%    
  mutate(level_label = sprintf('%s_label', level)) %.%
  dcast(rgn_id ~ level_label, value.var='label') %.%
  left_join(
    read.csv('../ohicore/inst/extdata/layers.Global2013.www2013/rgn_labels.csv') %.%
      select(rgn_id, v_label=label),
    by='rgn_id') %.%
  arrange(r0_label, r1_label, r2_label, v_label); head(georegion_labels)


# prepare for for loop below
layer_uni = unique(m_d$layer)
layername = sprintf('rgn_wb_%s_2014a.csv', layer_uni)
attrname  = sprintf('rgn_wb_%s_2014a_attr.csv', layer_uni)

for(k in 1:length(layer_uni)) { # k=1
  m_l = m_d[m_d$layer == layer_uni[k],] 
  names_m_l = names(m_l)
   
  m_l2 = m_l %.%
  select(-layer, -units); head(m_l2)  # before troubleshooting, also selected rgn_name

  layersave = file.path(dir_d, 'data', layername[k])
  attrsave = file.path(dir_d, 'data', attrname[k])
  
  # library(devtools); load_all('../ohicore')
  d_g_a = gapfill_georegions(
    data = m_l2 %.%
      filter(!rgn_id %in% c(213,255)) %.%
      select(rgn_id, year, value),
    fld_id = 'rgn_id',
    georegions = georegions,
    georegion_labels = georegion_labels,
    r0_to_NA = TRUE, 
    attributes_csv = (attrsave)) # don't chain gapfill_georegions or will lose head(attr(d_g_a, 'gapfill_georegions')) ability
  
  # investigate attribute tables
  head(attr(d_g_a, 'gapfill_georegions'))  # or to open in excel: system(sprintf('open %s', attrsave))
  
  # save gapfilled layer
  d_g = d_g_a %.%
    select(rgn_id, year, value) %.%
    arrange(rgn_id, year); head(d_g)
  
  names(d_g)[names(d_g) == 'value'] <- m_l$units[1]; head(d_g)
  
  write.csv(d_g, layersave, na = '', row.names=FALSE)
  
}

# --- fin




# # compare gapfill_georegions.r by BB to add_gapfill.r by JSL
# 
# # unemployment
# gg = read.csv('Global/WorldBank-Statistics_v2012/data/rgn_wb_uem_2014a.csv'); head(gg)
# ag = read.csv('Global/WorldBank-Statistics_v2012/tmp/rgn_wb_uem_2014awith_add_gapfill.csv'); head(ag)
# 
# vs = gg %.%
#   select(rgn_id, 
#          year,
#          value_gg = value) %.%
#   left_join(ag %.%
#               select(rgn_id, 
#                      year, 
#                      value_ag = perc), 
#             by = c('rgn_id', 'year')) %.%
#   mutate(
#     val_dif    = value_gg - value_ag,
#     val_notna  = is.na(value_gg)!=is.na(value_ag)) %.%   
#   filter(abs(val_dif) > 0.01 | val_notna == T) 
#   
# van = vs %.%
#   filter(rgn_id == 6)
# #               arrange(goal, desc(dimension), desc(score_notna), desc(abs(score_dif))) %.%
# #         select(goal, dimension, region_id, region_label, score_old, score, score_dif)
#               


# other trouble shooting-- this actually doesn't work because ohicore requires these packages. So this is not the problem. 
#     # ensure dplyr's summarize overrides plyr's summarize by loading in succession
#     if ('package:reshape2'  %in% search()) detach('package:reshape2')
#     if ('package:plyr'      %in% search()) detach('package:plyr')
#     if ('package:dplyr'     %in% search()) detach('package:dplyr')
#     library(reshape2)
#     library(plyr)
#     library(dplyr)
