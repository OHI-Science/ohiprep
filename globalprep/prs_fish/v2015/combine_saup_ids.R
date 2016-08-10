# Combining old and new SAUP regions



library(dplyr)



dir_M             <- c('Windows' = '//mazu.nceas.ucsb.edu/ohi',
                       'Darwin'  = '/Volumes/ohi',    ### connect (cmd-K) to smb://mazu/ohi
                       'Linux'   = '/home/shares/ohi')[[ Sys.info()[['sysname']] ]]

setwd(file.path(dir_M,'git-annex/globalprep/prs_fish'))

# new SAUP data
saup_2015 = file.path(dir_M,'git-annex/globalprep/SAUP_data_2015')

# old eezs

old_rgn = read.csv(file.path(dir_M,'git-annex/Global/NCEAS-SpatialFishCatch_v2014/SAUP/EEZ_numbers.csv'))

# new eezs

saup_2015_eez = read.csv(file.path(saup_2015,'raw/ohi_eez.csv'))

# old saup to new saup - these are the retired and new eezs from SAUP, matched by hand (jamie 6/16/15)

old_to_new = read.csv('v2015/old_saup_to_new_saup.csv')%>%
              select(EEZID,Name=Name_New,AdminCountry,old_saup_id)

new_eezs = unique(old_to_new$EEZID)


# Create one file with all info

all = saup_2015_eez%>%
        mutate(old_saup_id = ifelse(EEZID %in% new_eezs,old_to_new$old_saup_id[match(EEZID,old_to_new$EEZID)],EEZID))%>% #this doesn't capture all aggregated eezs
         select(EEZID,Name,AdminCountry,old_saup_id)%>%
          rbind(.,old_to_new)

# get duplicate rows and remove

d = which(all[])

all = all[!duplicated(all),]

# Add FAO area

write.csv(all,file='v2015/ohi_eezs_plus_old_saup_ids.csv')
