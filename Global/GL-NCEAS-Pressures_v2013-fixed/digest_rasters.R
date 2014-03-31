library(foreign)
library(tools)
library(plyr)
library(dplyr)

#wd = 'N:/model/GL-NCEAS-Pressures_v2013a/tmp'
wd = '/Volumes/data_edit/model/GL-NCEAS-Pressures_v2013a/tmp'
setwd(wd)

p = read.csv('../model_rasters_output.csv', stringsAsFactors=F)

# DEBUG: redo ones with 3nm since got overwritten
#p = subset(p, pressure %in% c('po_chemicals','po_chemicals_3nm','po_nutrients','po_nutrients_3nm'))

rescaling_fxns = list(
  'no_rescaling'      = expression(v),
  'score-max-plus10'  = expression(v / (max(v, na.rm=T) * 1.10)),
  'slr_clamp0_110pct' = expression(ifelse(v<0,0,v) / (max(v, na.rm=T) * 1.10)),
  'sst_abs313_110pct' = expression(abs(v) / (313 * 1.10))
) 

for (i in 1:nrow(p)){    # i=1
# DEBUG redo
#for (i in which(p$pressure %in% c('cc_slr','cc_sst','cc_uv'))){    # i=1    
  
  for (yr in 2012:2013){ # yr=2012
    
    # start with details per region for readability
    d = read.csv('../../GL-NCEAS-OceanRegions_v2013a/data/rgn_details.csv')[,c('rgn_id','rgn_typ','rgn_key','rgn_nam')]    
    
    # set tif variables
    pressure  = p[i,'pressure']
    rescaling = p[i,'rescaling']
    tifs      = file_path_sans_ext(basename(unlist(strsplit(p[i, sprintf('tifs_%da', yr)], ','))))
    rgn       = file_path_sans_ext(basename(p$rgn[i]))
    tif.dbfs  = sprintf('%s_%s.dbf', rgn, tifs)
    tif.flds  = make.names(tifs)    
    cat(sprintf('\n\n--------\n%s for %da. rescaling: %s. tifs: %s\n', pressure, yr, rescaling, paste(tifs, collapse=', ')))    
    
    # read in tifs
    for (j in 1:length(tifs)){   # j=1
      
      flds.dbf = setNames(c('rgn_id', tif.flds[j]), c('VALUE','MEAN'))
      d = merge(x=d, by='rgn_id',
                y=rename(read.dbf(tif.dbfs[j]), flds.dbf)[,flds.dbf], all.x=T)  ; head(d)
      v = d[[tif.flds[j]]]
      
      if (rescaling!=''){
        cat(sprintf('    %s before rescaling:\n', tif.dbfs[j]))
        print(summary(v))
      }      
      
      # apply rescaling, if needed
      if (rescaling=='score-max-plus10'){
        d[[tif.flds[j]]] = v / (max(v, na.rm=T) * 1.10)
        p[i,'rescaling_digest'] = "rescaling=='score-max-plus10': v / (max(v, na.rm=T) * 1.10"
        
      } else if (rescaling!='slr_clamp0_110pct'){        
        d[[tif.flds[j]]] = ifelse(v<0,0,v) / (max(v, na.rm=T) * 1.10)
        p[i,'rescaling_digest'] = "rescaling!='slr_clamp0_110pct': ifelse(v<0,0,v) / (max(v, na.rm=T) * 1.10)"
            
      } else if (rescaling!='sst_abs313_110pct'){        
        d[[tif.flds[j]]] = abs(v) / (313 * 1.10)           
        p[i,'rescaling_digest'] = "rescaling!='sst_abs313_110pct': abs(v) / (313 * 1.10)"
            
      } else if (rescaling!=''){        
        stop(sprintf("rescaling '%s' not handled!", rescaling))
      }
      
      #if (rescaling!=''){
      cat(sprintf('    %s after %s:\n', tif.dbfs[j], p[i,'rescaling_function']))
      print(summary(d[[tif.flds[j]]]))
      #}
      
      # DEBUG ----
      # try all rescaling, and see which matches
      for (fxn in names(rescaling_fxns)){
        d[[sprintf('%s_%s', basename(tif.flds[j]), fxn)]] = eval(rescaling_fxns[[fxn]])        
      }
    } # end: for (j in 1:length(tifs)){
    
    # for file in *-no-rescaling.csv ; do mv "$file" "${file/-no-rescaling.csv/-no_rescaling.csv}" ; done
    
    # get average of tifs (NOTE: removing NAs for mean)
    d[['pressure_score']] = apply(d[, tif.flds, drop=F], 1, mean, na.rm=T)
    
    # write to csv per year-pressure
    csv = sprintf('../data/%s_%d_check.csv', pressure, yr) 
#    write.csv(d[, c('rgn_id', 'pressure_score')], csv, na='', row.names=F)    
    
    # DEBUG ----
    # try all rescaling, and see which matches
    d_0 = read.csv(sprintf('../data/%s_%d.csv', pressure, yr))
    for (fxn in names(rescaling_fxns)){
      fld = sprintf('%s_%s', pressure, fxn)
      d[[fld]] = apply(d[,sprintf('%s_%s', basename(tif.flds), fxn),drop=F], 1, mean, na.rm=T)
      write.csv(x=rename(d[,c('rgn_id',fld)], setNames('pressure_score', fld)), 
                file=sprintf('../data/%s_%d_DEBUG-%s.csv', pressure, yr, fxn), na='', row.names=F)    
      d_ck = merge(d_0, d[,c('rgn_id',fld)], by='rgn_id')
      if (isTRUE(all.equal(d_ck[[fld]], d_ck[['pressure_score']]))){
        p[i,sprintf('rescaling_used_%d',yr)] = paste(na.omit(c(p[i,'rescaling_used'], fxn)), collapse=' | ')  
        cat(sprintf('\n  rescaling_USED: %s\n', p[i,'rescaling_used']))
      }      
    }
    
    # NEW ----
    # get average of tifs (NOTE: removing NAs for mean)
    fld = sprintf('%s_%s', pressure, fxn)
    if(rescaling=='') rescaling='no_rescaling'
    d[['pressure_score']] = apply(d[, tif.flds, drop=F], 1, mean, na.rm=T)
    
    # write to csv per year-pressure
    csv = sprintf('../data/%s_%d_check.csv', pressure, yr)
    write.csv(d[, c('rgn_id', 'pressure_score')], csv, na='', row.names=F)

  } 
}

write.csv(p, '_model_rasters_output_check.csv', row.names=F, na='')