require(ohi)
# setwd('/var/data/ohi/model/GL-NCEAS-Resilience_Matrix')

ohi.load('global_resilience_layers', dir='manual_output')
ohi.load('global_resilience_matrix', dir='manual_output')

global_resilience_layers
global_resilience_matrix


d <- NULL
for (f in list.files('data', '^r_.*.csv')) {
  k <- gsub('^r_', '', gsub('.csv$', '', f))
  print(k)
  z <- ohi.read.csv(file.path('data', f))
  z$layer <- k
  d <- rbind(d, z)
}
d <- d[,c('layer', 'id', 'value')]
names(d)[2] <- 'region_id'
summary(d)

# extract resilience data
resilience_data <- d[with(d, order(layer, region_id)),]
resilience_data$layer <- tolower(make.names(gsub('_', '.', resilience_data$layer)))
ohi.save('resilience_data')

# extract weight matrix
w <- as.vector(global_resilience_layers$VALUE)
names(w) <- make.names(global_resilience_layers$LAYER)

# filter out data for which we have no weighting value
w <- w[names(w) %in% resilience_data$layer] 
resilience_data <- subset(resilience_data, layer %in% names(w))

# filter out data for which we have no weighting value
w <- w[sort(names(w))]
resilience_data <- resilience_data[with(resilience_data, order(layer, region_id)),]


# perform calculations
r <- acast(region_id ~ layer, data=resilience_data)
names(dimnames(r)) <- c('region_id', 'layer')
stopifnot(all(dimnames(r)$layer == names(w)))

w <- array(w, dim=dim(r), dimnames=dimnames(r))

ohi.model.resilience(r, w)

