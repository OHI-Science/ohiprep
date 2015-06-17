# Combining old and new SAUP regions



library(dplyr)



dir_N = c('Windows' = '//neptune.nceas.ucsb.edu/data_edit',
          'Darwin'  = '/Volumes/data_edit',
          'Linux'   = '/var/data/ohi')[[ Sys.info()[['sysname']] ]]

setwd(file.path(dir_N,'git-annex/globalprep/Pressures_fishing'))

# new SAUP data
saup_2015 = file.path(dir_N,'git-annex/globalprep/SAUP_data_2015')


# new eezs

saup_2015_eez = read.csv(file.path(saup_2015,'raw/ohi_eez.csv'))

# old saup to new saup - these are the retired and new eezs from SAUP, matched by hand (jamie 6/16/15)

old_to_new = read.csv(file.path(saup_2015,'old_saup_to_new_saup.csv'))

new_eezs = old_to_new$EEZID_New


# Create one file with all info

all = saup_2015_eez%>%
        mutate(old_saup_id = ifelse(EEZID %in% new_eezs,old_to_new$EEZID_old[match(EEZID,old_to_new$EEZID_New)],EEZID))

# Add FAO area

write.csv(all,file='v2015/ohi_eezs_plus_old_saup_ids.csv'))
