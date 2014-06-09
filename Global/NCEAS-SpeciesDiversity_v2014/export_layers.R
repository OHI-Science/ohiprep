# paths
dd = 'N:/model/GL-NCEAS-SpeciesDiversity_v2013a/data'
setwd(dd)

d = read.csv('rgn_spp.csv', na.strings=''); head(d)
#paste(names(d), collapse="','") # 'rgn_id','rgn_typ','rgn_key','rgn_nam','spp_score_2012','spp_score_2013','spp_trend_2012','spp_trend_2013','spp_score_dif','spp_trend_dif'
for (fld in c('spp_score_2012','spp_score_2013','spp_trend_2012','spp_trend_2013')){ # fld = c('spp_score_2012','spp_score_2013','spp_trend_2012','spp_trend_2013')[1]
  v = rename(d[,c('rgn_id',fld)],
             setNames('score',fld))
  if (fld %in% c('spp_score_2012','spp_score_2013')){
    v$score = v$score/100 # needed to scale 0 to 1 (not 0 to 100) for use in resilience and elsewhere
  }  
  write.csv(v, sprintf('rgn_%s.csv',fld), row.names=F, na='')
}