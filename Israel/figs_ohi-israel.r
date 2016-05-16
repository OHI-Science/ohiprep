# ohi flower plots

source('PlotFlower_small.R')

# # set plotting variables 
# w <- 2.3 #width
# h <- 2.3 #height
# r <- 300 #res
# d <- 0.4 #disk
# lc <- 0.65 #label.cex
# lo <- 0.2 #label.offset
# c <- 0.7 #cex
# cm <- 1.2 #cex.main

# basic flower parameters
w <- 2.35 #width
h <- 2.35 #height
r <- 600 #res
d <- 0.4 #disk
lc <- 1 #label.cex
lo <- 0.2 #label.offset
c <- 1.2 #cex
cm <- 1.2 #cex.main

### Normal OHI ###
scores <- read.csv("scores.csv", stringsAsFactors=FALSE) %>%
  filter(goal != 'CS')
goals <- read.csv("conf/goals.csv", stringsAsFactors=FALSE) %>%
  filter(goal !='CS')
goals_supra = na.omit(unique(goals$parent))
wts = with(subset(goals, !goal %in% goals_supra, c(goal, weight)), setNames(weight, goal))
goal_labels = gsub('\\n', '\n', with(goals, setNames(goal, goal))[names(wts)], fixed=T)

# get goals for flowers, all and specific to weights
goals.all = arrange(goals, order_color)[['goal']]

# get colors for aster, based on 10 colors, but extended to all goals. subselect for goals.wts
cols.goals.all = colorRampPalette(RColorBrewer::brewer.pal(10, 'Spectral'), space='Lab')(length(goals.all))
names(cols.goals.all) = goals.all
# region scores    
x = with(subset(scores, dimension=='score' & goal %in% names(wts)),
         setNames(score, goal))[names(wts)]


## plot
fig2="flower1.png"
rgn_name="ohi-israel"
png(fig2,width=w*2, height=h*1, units="in",res=r)
# nf <- layout(matrix(c(0,1,1,0,2,2,3,3,4,4,5,5,6,6,7,7), 4, 4, byrow=TRUE), respect=FALSE) 
# nf <- layout(matrix(c(1,2), 1, 2, byrow=TRUE), respect=FALSE) 
par(mfrow=c(1,2))
crop=1.5

PlotFlower_small(main = rgn_name,
                 lengths=x,
                 widths=wts,
                 fill.col=ifelse(is.na(x), 
                                 'grey80', 
                                 cols.goals.all[names(wts)]),
                 labels  =ifelse(is.na(x), 
                                 paste(goal_labels, '-', sep='\n'), 
                                 paste(goal_labels, round(x), sep='\n')),
                 center=round(weighted.mean(x, wts, na.rm=T)),
                 max.length = 100, disk=0.4, label.cex=lc/crop, label.offset=lo, cex=c/crop, cex.main=cm/crop)


goals$weight <- reweigh(weights,"equal")
wts = with(subset(goals, !goal %in% goals_supra, c(goal, weight)), setNames(weight, goal))
rgn_name="COHI - Equal"
PlotFlower_small(main = rgn_name,
                 lengths=x,
                 widths=wts,
                 fill.col=ifelse(is.na(x), 
                                 'grey80', 
                                 cols.goals.all[names(wts)]),
                 labels  =ifelse(is.na(x), 
                                 paste(goal_labels, '-', sep='\n'), 
                                 paste(goal_labels, round(x), sep='\n')),
                 center=round(weighted.mean(x, wts, na.rm=T)),
                 max.length = 100, disk=d, label.cex=lc/crop, label.offset=lo, cex=c/crop, cex.main=cm/crop)

dev.off()  
