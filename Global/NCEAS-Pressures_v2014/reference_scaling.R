library(foreign)
library(tools)
library(plyr)
library(dplyr)
library(stringr)

source('src/R/common.R') # get dir_neptune_data based on OS
wd = 'Global/NCEAS-Pressures_v2014'

rescaling_fxns = list(
  'no_rescaling'      = expression(v),
  'score-max-plus10'  = expression(v / (max(v, na.rm=T) * 1.10)),
  'slr_clamp0_110pct' = expression(ifelse(v<0,0,v) / (max(v, na.rm=T) * 1.10)),
  'slr_clamp0_110pct' = sprintf('ifelse(v<0,0,v) / %f', max(v, na.rm=T) * 1.10),
  'slr_clamp0_110pct' = expression('ifelse(v<0,0,v) / 1.8'),  
  'sst_abs313_110pct' = expression(abs(v) / (313 * 1.10))
) 

p = read.csv(file.path(wd, 'scaling_in.csv'), stringsAsFactors=F)
for (i in 1:nrow(p)){ # i=2
  
  # set pressure (non-year-specific) variables
  pressure  = p[i,'pressure']
  
  # get data for reference year
  ref_csv = file.path(wd, p[i, sprintf('csv_%d', p[i, 'ref_year'])])
  d = read.csv(ref_csv)
  
  # evaluate expression to apply to all years of raw data
  expression_in  = p[i, 'scaling_raw']
  p[i, 'scaling_out'] = with(d, eval(parse(text=scaling_expression)))
  
}
write.csv(p, file.path(wd, 'scaling_out.csv'), row.names=F, na='')

# OLD ----


#   rescaling = ifelse(p[i,'rescaling']=='', 'no_rescaling', p[i,'rescaling'])
#   rgn       = file_path_sans_ext(basename(p$rgn[i]))
#   
#   for (yr in 2012:2013){ # yr=2012
#     
#     # start with details per region for readability
#     d = read.csv(file.path(dir_neptune_data, 'model/GL-NCEAS-OceanRegions_v2013a/data/rgn_details.csv'))[,c('rgn_id','rgn_typ','rgn_key','rgn_nam')]    
#     
#     # set year-specific variables    
#     tifs      = file_path_sans_ext(basename(unlist(strsplit(p[i, sprintf('tifs_%da', yr)], ','))))
#     tif.dbfs  = sprintf('%s_%s.dbf', rgn, tifs)
#     tif.flds  = make.names(tifs)    
#     cat(sprintf('\n\n--------\n%s for %da. rescaling: %s. tifs: %s\n', pressure, yr, rescaling, paste(tifs, collapse=', ')))    
#     
#     # read in tifs
#     for (j in 1:length(tifs)){   # j=1
#       
#       flds.dbf = setNames(c('rgn_id', tif.flds[j]), c('VALUE','MEAN'))
#       d = merge(x=d, by='rgn_id',
#                 y=rename(read.dbf(file.path(dir_wd, 'tmp', tif.dbfs[j])), flds.dbf)[,flds.dbf], all.x=T)
#       v = d[[tif.flds[j]]]
#       d[[tif.flds[j]]] = eval(rescaling_fxns[[rescaling]])
#     }      
#     
#     # get average of tifs (NOTE: removing NAs for mean)
#     d[['pressure_score']] = apply(d[, tif.flds, drop=F], 1, mean, na.rm=T)
#     
#     # write to csv per year-pressure
#     csv = sprintf('%s/data/%s_%d_NEW.csv', dir_wd, pressure, yr)
#     write.csv(d[, c('rgn_id', 'pressure_score')], csv, na='', row.names=F)    
#     
#     # DEBUG: get difference with OLD ----
#     d_0 = read.csv(sprintf('%s/data/%s_%d.csv', dir_wd, pressure, yr))
#     d_ck = merge(d_0, rename(d[, c('rgn_id', 'pressure_score')], c('pressure_score'='pressure_score_NEW')), by='rgn_id', all=T)
#     d_ck[['dif']] = d_ck[['pressure_score_NEW']] - d_ck[['pressure_score']]
#     write.csv(d_ck, sprintf('%s/data/%s_%d_NEW-DIF.csv', dir_wd, pressure, yr))
#     p[i,sprintf('dif_mean_%d',yr)] = mean(d_ck[['dif']], na.rm=T)
#     p[i,sprintf('dif_min_%d',yr)]  =  min(d_ck[['dif']], na.rm=T)
#     p[i,sprintf('dif_max_%d',yr)]  =  max(d_ck[['dif']], na.rm=T)            
#   } 
# }
# 
# write.csv(p, file.path(dir_wd, 'tmp/_model_rasters_output_NEW-DIF.csv'), row.names=F, na='')