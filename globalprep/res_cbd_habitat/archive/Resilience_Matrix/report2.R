require(ohi)

ohi.load('global_v_resilience_layers')
ohi.load('global_resilience_layers')

# check that all layers are in global2012/layers_data
ohi.load('layers_data', dir='/usr/local/ohi/src/model/global2012/doc')
stopifnot(all(gsub('-','_',global_resilience_layers$LAYER) %in% layers_data$layer_id))

names(global_resilience_layers) <- tolower(names(global_resilience_layers))

r <- acast(id ~ layer, data=subset(global_v_resilience_layers, id %in% ohi.global.regions.eez))
names(dimnames(r)) <- c('region_id', 'layer')
b <- ifelse(!is.na(r),T,F)

w <- global_resilience_layers$value
names(w) <- global_resilience_layers$layer
w

d <- global_resilience_layers[,c('layer', 'type', 'category')]
d$type <- ifelse(d$type == 'ecological', 'environmental', d$type)
d$type <- ifelse(paste(d$type, d$category) == 'environmental regulations', 'regulatory', d$type)
t <- d$type
names(t) <- d$layer
t <- t[names(w)]
t


w <- ohi.model.resilience.matrix(b, w)
w
ohi.model.resilience(r, t, w)