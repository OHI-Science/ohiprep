source('src/R/common.R') # set pathnames, load common libraries

setwd(file.path(dir_neptune_data, 'model/GL-NCEAS-SpeciesDiversity_v2013a'))
setwd(file.path(dir_neptune_data, 'git-annex/Global/NCEAS-SpeciesDiversity_v2014'))
setwd(file.path(dir_neptune_data, 'git-annex/globalprep/SpeciesDiversity/v2015'))

setwd('tmp')
setwd('cache/iucn_details')
setwd('data')
setwd('..')
getwd()
head(list.files(), 20)

x <- list.files()
y <- file.info(x)
sum(y$size)

dir_git <- '~/github/ohiprep'
goal       <- 'globalprep/SpeciesDiversity'
prod       <- 'v2015'
dir_loc    <- file.path(dir_git, goal, prod)

y<- data.frame(x)

write.csv(y, file.path(dir_loc, 'iucn_details_v2013a.csv'), row.names = FALSE)

write.csv(y, 'iucn_details_current.csv', row.names = FALSE)
