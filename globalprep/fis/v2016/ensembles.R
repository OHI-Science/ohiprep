#playing with fishensembles package

#get a subset of the SAUP data

source('~/github/ohiprep/src/R/common.R')

#set 5 sample stocks for now

stks <- c("Atlantic bluefin tuna", "Peruvian mojarra", "Sea trout", "African moonfish","Australian spiny lobster")

#rename columns to match x$data output format
catch <- read.csv(file.path(dir_M,'git-annex/globalprep/fis/raw/SAUP_catch_taxon_tons_eezs.csv'))%>%
            filter(species %in% stks)%>%
            mutate(species = gsub(" ", "_", .$species,fixed = TRUE),
                   stock_id = paste(species,rgn_num,sep="_"))
            
library(datalimited)
library(fishensembles) #i cloned this package then used devtools::install() to install it locally so we can call it whenever. I didthis since it;s a private repo

# First we need to run the 4 catch only models on our catch data

## cmsy

c <- cmsy()


x <- make()

rf <- x$model
df <- x$data

#run the random forest model on our data


