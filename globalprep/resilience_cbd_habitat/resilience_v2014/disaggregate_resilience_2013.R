# get disaggregate() function
source('/Volumes/local_edit/src/R/jstewart/disaggregate_bbest.R')

# directories for Nature 2012 (2012n) and new (2013a)
dir.2012n = '/Volumes/data_edit/model/GL-NCEAS-Resilience_Matrix/data' 
dir.2013a = '/Volumes/data_edit/model/GL-NCEAS-Resilience_v2013a/data'

# list of layers with all parameters to run disaggregate()
l = list(
  r_mora_s4 = list(
    csv.in     = file.path(dir.2012n, 'r_mora_s4.csv'), 
    csv.out    = file.path(dir.2013a, 'r_mora_s4_2013a.csv'), 
    flds.id    = c('id'),
    fld.value  = 'value',
    id.exclude = c(),     # c(162) for Antarctica
    weight.csv = NA, 
    weight.fld = NA,
    run        = TRUE),
  r_mora = list(
    csv.in     = file.path(dir.2012n, 'r_mora.csv'), 
    csv.out    = file.path(dir.2013a, 'r_mora_2013a.csv'), 
    flds.id    = c('id'),
    fld.value  = 'value',
    id.exclude = c(),     # c(162) for Antarctica
    weight.csv = NA, 
    weight.fld = NA,
    run        = TRUE))

# flag for printing out info
verbose = T

# loop through odd index of vector to disaggregate
for (lyr in names(l)){ # lyr = names(l)[1]
  
  # skip layer if run is set to FALSE
  if (l[[lyr]][['run']]==FALSE) next
    
  # read in data
  d = read.csv(l[[lyr]][['csv.in']], na.strings='')
  
  # print input csv summary info 
  if (verbose){
    cat(sprintf('\n--------------------\n%s\n\nIN: %s\n csv.in: %s\n dim: %d rows x %d cols\n', lyr, lyr, l[[lyr]][['csv.in']], nrow(d), ncol(d)))
    print(summary(d))
    print(head(d), row.names=F)
  }

  # run disaggregation
  disaggregate(csv.in = l[[lyr]][['csv.in']], csv.out = l[[lyr]][['csv.out']], 
               flds.id = l[[lyr]][['flds.id']], fld.value = l[[lyr]][['fld.value']], id.exclude=l[[lyr]][['id.exclude']], 
               weight.csv = l[[lyr]][['weight.csv']], weight.fld = l[[lyr]][['weight.fld']])

  # print output csv summary info
  if (verbose){
    d = read.csv(l[[lyr]][['csv.out']], na.strings='')
    cat(sprintf('\nOUT: %s\n csv.out: %s\n dim: %d rows x %d cols\n', lyr, l[[lyr]][['csv.out']], nrow(d), ncol(d)))
    print(summary(d))
    print(head(d), row.names=F)
  }  
}  
