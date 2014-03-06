
# set the neptune data_edit share based on operating system
dir_neptune_data = c('Windows' = '//neptune/data_edit',
                     'Darwin'  = '/Volumes/data_edit',
                     'Linux'   = '/var/data/ohi')[[ Sys.info()[['sysname']] ]]

# stop if directory doesn't exist
if (!file.exists(sprintf('%s/',dir_neptune_data))){
  stop(sprintf("The directory for variable dir_neptune_data set in src/R/common.R does not exist. Do you need to mount %s?", dir_neptune_data))
  
}