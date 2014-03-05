# data_prep.R. 
# Add rgn_ids for World Bank WGI (World Governance Indicators)
# Previously had been named clean_WGI.r (by JStewart May2013). This script created by JStewartLowndes Mar2014.

# gapfilling: sovereignty (parent-children) gapfilling with add_gapfill_sov.r

# setup
source('/Users/jstewart/github/ohiprep/src/R/ohi_clean_fxns.R') # also fix this directory
dir1 = ('/Users/jstewart/github/ohiprep/Global/WorldBank-WGI_v2013') # also fix this directory
wd = file.path(dir1, 'raw')
setwd(wd)

library(mgcv) # for troubleshooting below
library(reshape2)
library(gdata) # to enable read.xls
options(max.print=5E6)

filein = 'wgidataset.xlsx'
wgisheetNames = gsub(" ", "", sheetNames(filein))
wgisheets = 2:7 # the six WGI indicators, each on a separate sheet
headerID = 'Country/Territory'

# read in two header rows, collapse into one header title, read in the rest of the data with those header names.
d.all =  matrix(nrow=0, ncol=0)
for (s in wgisheets){
  rm(d, d.m, d.c)
  
  hdr = read.xls(filein, sheet=s, blank.lines.skip=F, skip=13, nrows=2, header=F) 
  hdr = apply(hdr, 2, function(x) paste(rev(x),collapse='.'))
  hdr[1:2] = gsub('\\.','',hdr[1:2]) # clean two first column headers
  hdr = gsub('\\-','',hdr) # 
  hdr = gsub('.+ank','PercRank',hdr) # make this consistent: P-Rank and Rank were used in different sheets but are same value. 
  
  d = read.xls(filein, sheet=s, blank.lines.skip=F, skip=15, header=F, col.names=hdr) 
  # head(dat)
  
  # melt data
  d.m = melt(data=d, id.vars=names(d)[1:2], variable.name='stat')
  names(d.m) = c('country','countryID','stat','value')
  #head(d.m)
  
  # change NAs
  d.m$value[d.m$value == '#N/A'] = NA 
  
  # split stat (the combined header variable created above)
  v = strsplit(as.character(d.m$stat), '\\.') 
  d.m[['year']] = sapply(v, function(x) x[2])
  d.m[['metric']] = sapply(v, function(x) x[1])
  
  # add WGI indicator
  d.m$WGI = rep.int(wgisheetNames[s], length(d.m$value))
  #head(d.m)  
  
  
   # Redo capitalization: capitalize just the first letter: function from R help page for sapply
  capwords <- function(s, strict = FALSE) {
    cap <- function(s) paste(toupper(substring(s, 1, 1)),
                  {s <- substring(s, 2); if(strict) tolower(s) else s},
                             sep = "", collapse = " " )
    sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
  }
  
  d.m$country = tolower(d.m$country)
  d.m$country = sapply(d.m$country, function(x) capwords(x))
  d.m$country = as.character(lapply(as.character(d.m$country), function(x) capwords(x)))
  
  d.m$country = gsub('And ', 'and ', d.m$country, fixed=TRUE) # and a few to do by hand
  d.m$country = gsub('Sar', 'SAR', d.m$country, fixed=TRUE)
  d.m$country = gsub('Fyr', 'FYR', d.m$country, fixed=TRUE)
  d.m$country = gsub('Rb', 'RB', d.m$country, fixed=TRUE)
  d.m$country = gsub('Pdr', 'PDR', d.m$country, fixed=TRUE)
  d.m$country = gsub('u.s.', 'U.S.', d.m$country, fixed=TRUE)
  
  # cast
  d.c = dcast(d.m, country + WGI + year ~ metric) 
  # or by year: dcast(d.m, country + WGI + metric ~ year)
  names(d.c) = c("country","WGI","year","Estimate","Lower","NumSrc","P","StdErr","Upper")
  
  
  # concatenate f files
  d.all = rbind(d.all, d.c)
  
}

# prepare as add_rgn_id expects--d.all2 would be final, but all 6 indicators are separate
d.all2 = d.all[c(1,4,3,2)]
names(d.all2)[c(2,4)] = c('score', 'category')

# calculate average score

# transpose with mean aggregate function to average over 6 indicator subcategories
d.all2$score = as.numeric(d.all2$score)
d.t = dcast(d.all2, value.var="score", country ~ year, fun.aggregate = mean, na.rm = T) 

# # check for rows that are all NA: None
# d.t2 = matrix(nrow=0, ncol=0)
# for(i in 1:dim(d.t)[1]){             
#   bb = dim(d.t)[2] - sum(is.na(d.t[i,]))
#   if(bb == 1) { # this means just the countryname and country code name are not NA
#     cat(d.t$country[i])# d.2 = rbind(d.2,d.1[i,])
#   }
# }
  
# remelt for final
d.m2 = melt(data=d.t, id.vars=names(d.all)[1], variable.name='year')
d.m3 = d.m2[order(d.m2$country, d.m2$year),]
names(d.m3)[3] = 'score'

# get it in order add_rgn_id expects
d.m4 = d.m3[c(1,3,2)]
d.m4 <- d.m4[d.m4[,1] != "Jersey, Channel Islands",] # only data for 2011; needs to be gapfilled

# partition Netherland Antilles
ind = d.m4$country %in% c('Netherlands Antilles (former)')
d.m5 = rbind(d.m4[!ind,],
  data.frame(country=c('Sint Maarten', 'Curacao', 'Bonaire', 'Saba', 'Sint Eustasius'), # Aruba reported separately
            score=rep(d.m4$score[ind], 5),
            year=rep(d.m4$year[ind], 5)))

## run add_rgn_id and save
uifilesave = paste(dir1, 'data/', 'GL-WorldBank-WGI_v2011-cleaned.csv', sep='')
add_rgn_id(d.m5, uifilesave)

# ------------- gapfill
## sovereignty (parent-children) gapfilling with add_gapfill_sov.r
cleaned_data = read.csv(uifilesave)
cleaned_data_sov =  add_gapfill_sov(cleaned_data) 

# combine and clean
cleaned_data2 = rbind(cleaned_data, cleaned_data_sov)
cleaned_data2 = cleaned_data2[order(cleaned_data2$rgn_id),]
cleaned_data2[cleaned_data2$rgn_id_2013 != 213,] # remove Antarctica
cleaned_data2$rgn_nam = NULL

# save all (interim file)
print('Final data layer saved: ')
layersave = paste(dir1, 'data/', 'rgn_wb_wgi_all.csv', sep='')
write.csv(cleaned_data2, layersave, na = '', row.names=FALSE)


### save as 2012a and 2013a files:

d_all = read.csv(layersave)
filesave2012a = paste(dir1, 'data/', 'rgn_wb_wgi_2012a.csv', sep='')
filesave2013a = paste(dir1, 'data/', 'rgn_wb_wgi_2013a.csv', sep='')
save_pressure_layers_2012a_2013a(d_all, filesave2012a, filesave2013a)





