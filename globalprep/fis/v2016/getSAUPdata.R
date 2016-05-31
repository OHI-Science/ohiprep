#getting SAUP catch data

source('~/github/ohiprep/src/R/common.R')

install.packages("devtools")
devtools::install_github("SeaAroundUs/rseaaroundus")

library(tidyr)
library(reshape2)
library(dplyr)
library(seaaroundus)
library(parallel)

rgns <-listregions("eez")%>%
        rename(EEZ = title)%>%
          mutate(num = rownames(.))

ctrys <- list(rgns$num)

hs <- listregions("highseas")%>%
        rename(FAOarea = title)%>%
          mutate(num = rownames(.))

#Since you can only get catch data from one country at a time, I've written a function to collate them all into the format we want

saup_catchdata <- function(rgn, ctry){
  
 tons <- catchdata(rgn,ctry,limit=1000)
 
 #for some reason when i tried to mutate(year = rownames(.)), the duplicate column names were not letting me do this. But creating a new column manually like below
 #works fine.... you can try this by setting ctry = 16 (American Samoa) and you'll find 2 Snapper columns... which is
 tons$year = rownames(tons)
 
 out <- tons%>%
            melt(variable.name = 'species',
                 value.name = 'tons')%>%
            mutate(rgn_num = ctry,
                   rgn_name = rgns$EEZ[match(ctry,rgns$num)])
        
 return(out)  
 
}

all <- mclapply(ctrys[[1]],saup_catchdata,rgn = "eez")

d <- do.call(rbind.data.frame, all)

write.csv(d,file=file.path(dir_M,'git-annex/globalprep/fis/raw/SAUP_catch_taxon_tons_eezs.csv'))

#High Seas is not working...
hs <- mclapply(list(hs$num)[[1]],saup_catchdata,rgn='highseas')


