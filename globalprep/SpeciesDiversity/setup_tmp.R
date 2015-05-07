# tmp file setup and finish - copied from data_prep.R just in case

#   setup.sh -----
# ??? Is this critical? Looks like it's just copying (linking) files to a new location for easy reference?
setup_tmp_rgn <- function(dir_anx, dir_mdl) {
  # mkdirs data tmp
  dir_tmp_rgn <- file.path(dir_anx, 'tmp_rgn')
  
  if(!dir.exists(dir_tmp_rgn)) 
    dir.create(dir_tmp_rgn)
  
  orig_wd <- getwd()
  setwd(dir_tmp_rgn)
  # # link regions shapefile in geographic coordinate system
  # ??? why link, not copy?  Will we be changing these files, if so, should we be 
  # modifying the original version?  or is original likely to be changed elsewhere?  if not, copy rather than link? or:
  # ??? do we need all those bits and pieces, or just the .shp and .tif?
  # ??? can we just read these in directly into memory when we need 'em?
  dbf_ext      <- 'shp' # to link/copy *all* files, use: dbf_ext <- c('shp','dbf','shx','prj','sbn','sbx','shp.xml')
  data_mdl     <- file.path(dir_mdl, 'GL-NCEAS-OceanRegions_v2013a/data')
  # ??? why v2013a? nothing more recent?
  # ??? I can see pulling this from the model directory... general data
  fao_gcs      <- paste('rgn_fao_gcs.', dbf_ext,  sep = '')
  land_gcs     <- paste('land_gcs.',    dbf_ext,  sep = '')
  offshore_mol <- 'rgn_offshore_3nm_mol.tif'
  
  if(!file.exists(fao_gcs)) 
    file.symlink(file.path(data_mdl, fao_gcs),  '.') # ??? file.copy to copy; file.link or file.symlink to create hard or symbolic links
  if(!file.exists(land_gcs)) 
    file.symlink(file.path(data_mdl, land_gcs), '.')
  if(!file.exists(offshore_mol)) 
    file.symlink(offshore_mol, '.')
  
  # get last year's species to check for change in status (especially for those that have gone EXtinct)
  spp_old <- file.path(dir_mdl, 'GL-NCEAS-SpeciesDiversity/ohi_spp/data/spp.csv')
  # ??? should we just copy this over to the git-annex directory and pull from there?
  # ??? also - is this required any more?  I think the ingest_iucn went away from the 'EX but only if not EX in 2012 model'...
  spp_old_rename <- 'spp_2012.csv'
  if(!file.exists('spp_2012.csv')) 
    file.symlink(spp_old, 'spp_2012.csv')
  
  # # spp for 2012
  # for f in cells spp cells cells_spp; do
  # echo $OHI_MODELDIR/GL-NCEAS-SpeciesDiversity_v2012/data/${f}.csv tmp/${f}_2012.csv
  # done
  # ??? do we need? echo in this context outputs the values of these variables to the screen.
  
  setwd(orig_wd)
}
setup_tmp_rgn(dir_anx, dir_mdl)


#   setup_tmp.py -----
setup_tmp <- function(dir_lcl, dir_mdl) {
  # ??? Do we really need this?  Working from local is likely faster, and these are huge files, so maybe.
  # ??? What are the files we are copying and why?  
  #     * spp.db and geodb.gdb from 2013... do we want to copy the 2014 ones maybe?  where are those?
  #     * where do these files get accessed, where do they get modified?
  #     * or are they just for comparisons?
  # ??? This copies huge files from network to local - 3 GB and 12 GB respectively.
  # # move remote databases to local (removing from remote? depends on file.move (original python script) vs file.copy (as currently coded here))
  # 
  data_lcl <- file.path(dir_lcl, 'GL-NCEAS-SpeciesDiversity_v2013a/tmp')
  data_mdl <- file.path(dir_mdl, 'GL-NCEAS-SpeciesDiversity_v2013a/tmp')
  if(!dir.exists(dir_lcl)) 
    dir.create(dir_lcl, recursive = TRUE)
  for (path in c('dummy_spp.txt', 'dummy_geodb.gdb')) {
    # path <- 'spp.db' # path <- 'geodb.gbd' # path <- 'dummy_spp.txt' # path <- 'dummy_geodb.gdb'
    # replace dummy with non-dummy when testing is done
    l = file.path(data_lcl, path)         # ??? create local pathname for one of the two databases
    r = file.path(data_mdl, path)         # ??? create remote pathname
    if (file.exists(r))                   # ??? if remote database exists,
      if (file.exists(l))                 # ???   and local database exists,
        unlink(l)                         # ???       delete the entire local directory tree (directory clean up)
    file.copy(r, data_lcl, recursive = TRUE)   # ???   then (remote exists, local didn't exist or has been removed) copy remote -> local
  }
}
setup_tmp(dir_lcl, dir_mdl)


finish_tmp <- function(dir_lcl, dir_mdl) {
  # ??? This copies huge files from local to network - 3 GB and 12 GB respectively.
  # move local databases to remote  
  # ??? do we want to overwrite the data on Neptune? are we actually changing anything?
  
  data_lcl <- file.path(dir_lcl, 'GL-NCEAS-SpeciesDiversity_v2013a/tmp')
  data_mdl <- file.path(dir_mdl, 'GL-NCEAS-SpeciesDiversity_v2013a/tmp')
  if(!dir.exists(data_mdl)) dir.create(data_loc, recursive = TRUE)
  
  for (path in c('dummy_spp.txt', 'dummy_geodb.gdb')) { 
    # path <- 'spp.db' # path <- 'geodb.gbd'
    l = file.path(data_lcl, path)         # ??? create local pathname for one of the two databases
    r = file.path(data_mdl, path)         # ??? create remote pathname
    if (file.exists(l))                   # ??? if remote database exists,
      if (file.exists(r))                 # ???   and local database exists,
        unlink(r)                         # ???       delete the entire local directory tree (directory clean up)
    file.copy(l, data_mdl, recursive = TRUE)   # ???   then (remote exists, local didn't exist or has been removed) copy remote -> local
    unlink(l)                             # ??? remove the file from the local drive
  }
}
finish_tmp(dir_lcl, dir_mdl)
