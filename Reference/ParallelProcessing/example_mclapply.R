#getting SAUP catch data

source('~/github/ohiprep/src/R/common.R')

library(tidyr)
library(reshape2)
library(dplyr)
library(seaaroundus)
library(parallel)

rgns <-listregions("eez")%>%
        rename(EEZ = title)%>%
          mutate(num = rownames(.))


#Grab the first 50 so we don't need to wait forever
ctrys <- list(rgns$num)[[1]][1:100]



#Since you can only get catch data from one country at a time, I've written a function to collate them all into the format we want

saup_catchdata <- function(rgn, ctry){
  
#get catch data per rgn, ctry  
 tons <- catchdata(rgn,ctry,limit=1000)
 
 #for some reason when i tried to mutate(year = rownames(.)), the duplicate column names were not letting me do this. But creating a new column manually like below
 #works fine.... you can try this by setting ctry = 16 (American Samoa) and you'll find 2 Snapper columns... which is
 tons$year = rownames(tons)
 
 #from the catch data, turn wide to long format and add rgn_num and rgn_name from the rgns table
 out <- tons%>%
            melt(variable.name = 'species',
                 value.name = 'tons')%>%
            mutate(rgn_num = ctry,
                   rgn_name = rgns$EEZ[match(ctry,rgns$num)])

 return(out)  
 
}

#apply the saup_catchdata function to all rgns in the ctrys list using mclapply which takes advantage of parallel processing
all <- lapply(ctrys,saup_catchdata,rgn = "eez")

system.time(all <- mclapply(ctrys,saup_catchdata,rgn = "eez",mc.cores=16))

d <- do.call(rbind.data.frame, all)




