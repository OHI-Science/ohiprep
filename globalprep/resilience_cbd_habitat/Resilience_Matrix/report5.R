require(ohi)
options(digits=4)
options(width=70)
## Example for computing CW resilience

# load data
ohi.load('global_v_resilience_layers')
ohi.load('global_resilience_layers')
ohi.load('global_resilience_matrix')
names(global_resilience_layers) <- tolower(names(global_resilience_layers))

# extract relavant resilience layers for CW goal
layers <- subset(global_resilience_matrix, GOAL == 'clean-waters' & COMPONENT == 'all')$LAYER
layers

# extract resilience layers typing information
# XXX: we need to fix the global_resilience_layers to use the correct values
d <- subset(global_resilience_layers, layer %in% layers)[,c('layer', 'type', 'category', 'value')]
d$type <- ifelse(d$type == 'ecological', 'environmental', d$type)
d$type <- ifelse(paste(d$type, d$category) == 'environmental regulations', 'regulatory', d$type)
t <- d$type
names(t) <- d$layer
t

# extract resilience layers weighting information
w <- d$value
names(w) <- d$layer
w

# extract resilience layers data for regions
r <- acast(id ~ layer, data=subset(global_v_resilience_layers, id %in% 1:172 & layer %in% layers))
names(dimnames(r)) <- c('region_id', 'layer')
r

# extract data mask
b <- ifelse(!is.na(r),T,F)
b

# run resilience matrix model
w <- ohi.model.resilience.matrix(b, w)
w
x <- ohi.model.resilience(r, t, w)
x

# test against 2012 resilience scores
ohi.load('results_global_data', dir='/usr/local/ohi/src/model/global2012/doc/')
x.actual <- subset(results_global_data, goal.subgoal=='CW')[,'resilience']
names(x.actual) <- subset(results_global_data, goal.subgoal=='CW')[,'id']
x.actual <- x.actual[names(x)]
stopifnot(names(x) == names(x.actual))
stopifnot(all(round(x.actual/100 - x, 3) == 0.000, na.rm=T))

