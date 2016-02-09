require(ohi)
options(digits=4)
options(width=70)
## Example for computing HAB resilience
# setwd('/var/data/ohi/model/GL-NCEAS-Resilience_Matrix')

# For habitats we have a notion of all the unique combination of habitat presence
# across all countries. Then, the compute a resilience per region we take the straight
# mean across the habitats present. This is a "shortcut" but unfortunately is makes
# the code more obtuse.
r.combo <- ohi.read.csv('/var/data/ohi/model/GL-NCEAS-Resilience_Matrix/data/global_resilience_combos.csv')[,1:2]
r.component <- r.combo$component
names(r.component) <- r.combo$id
r.component

# extract relavant resilience layers for HAB goal
ohi.load('global_resilience_matrix')
names(global_resilience_matrix) <- tolower(names(global_resilience_matrix))
layers <- subset(global_resilience_matrix, goal == 'biodiversity' & component %in% r.component)[,-1]
layers

# extract resilience layers typing information
# XXX: we need to fix the global_resilience_layers to use the correct values
ohi.load('global_resilience_layers')
names(global_resilience_layers) <- tolower(names(global_resilience_layers))
d <- subset(global_resilience_layers, layer %in% layers$layer)[,c('layer', 'type', 'category', 'value')]
d$type <- ifelse(d$type == 'ecological', 'environmental', d$type)
d$type <- ifelse(paste(d$type, d$category) == 'environmental regulations', 'regulatory', d$type)
t <- d$type
names(t) <- d$layer
t

# extract resilience layers weighting information for the per resilience category aggregations
w <- d$value
names(w) <- d$layer
w

# extract resilience layers data for regions
ohi.load('global_v_resilience_layers')
r <- acast(id ~ layer, data=subset(global_v_resilience_layers, id %in% 1:172 & layer %in% layers$layer))
names(dimnames(r)) <- c('region_id', 'layer')
r

# extract data mask
b <- ifelse(!is.na(r),T,F)
b

# run resilience model, one region at a time
# load which layers for each region based on the regions habitat combination
x <- as.double(rep(NA, dim(r)[[1]]))
names(x) <- dimnames(r)$region_id
for (i in dimnames(r)$region_id) { # i=1
  if (i %in% names(r.component)) {
    habitat.combo.i <- r.component[[i]]
    layers.i <- sort(subset(layers, component == habitat.combo.i)$layer)
    b.i <- matrix(T, nrow=1, ncol=length(layers.i), dimnames=list(region_id=i, layer=layers.i))
    w.i <- w[layers.i]
    w.i <- ohi.model.resilience.matrix(b.i, w.i)
    x[[i]] <- ohi.model.resilience(r[i,layers.i, drop=F], t, w.i)
  }
}
x

# test against 2012 resilience scores
ohi.load('results_global_data', dir='/usr/local/ohi/src/model/global2012/doc/')
x.actual <- subset(results_global_data, goal.subgoal=='HAB')[,'resilience']
names(x.actual) <- subset(results_global_data, goal.subgoal=='HAB')[,'id']
x.actual <- x.actual[names(x)]
stopifnot(names(x) == names(x.actual))
stopifnot(all(round(x.actual/100 - x, 3) == 0.000, na.rm=T))

