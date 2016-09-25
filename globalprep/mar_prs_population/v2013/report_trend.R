library(plyr)

wd = '/var/data/ohi/model/GL-NCEAS-CoastalPopulation_v2013/data'
setwd(wd)

years = 2005:2015
suppressWarnings(rm(list=c('d','di')))
for (i in 1:length(years)){ # i=1
  yr = years[i]
  di = plyr::rename(read.csv(sprintf('rgn_popsum%d_inland25mi.csv', yr)),
              setNames(c('popn'), sprintf('rgn_popsum%d_inland25mi',yr)))
  di$year = yr
  if (i==1){
    d = di
  } else {
    d = rbind(d, di)
  }
}

# get annual trend
y = ddply(subset(d, year %in% 2012:2012), .(rgn_id), summarize,
          trend = lm(popn ~ year)$coefficients[['year']])

# output
write.csv(rename(y, c('trend'='people_per_year')), 'rgn_annualpoptrend_2010to2012_inland25mi.csv', row.names=F, na='')

# get 5 year trend per scenario in % popn change per year ----
scenarios = list('2013a'=2008:2013,
                 '2012a'=2007:2012)
for (s in names(scenarios)){ # s='2013a'
  yrs = scenarios[[s]]
  x = ddply(subset(d, year %in% yrs), .(rgn_id), summarize,
            trend = (popn[which.max(year)] - popn[which.min(year)]) / popn[which.min(year)])
  csv = sprintf('rgn_popn5yrtrend_inland25mi_%dto%d.csv', min(yrs), max(yrs))
  write.csv(x[,c('rgn_id','trend')], csv, row.names=F, na='')
}