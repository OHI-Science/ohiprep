
dir_git <- '~/github/ohiprep'
setwd(dir_git)

source('src/R/common.R') # set pathnames, load common libraries
library(rPython) # to call Python functions and scripts within R

goal       <- 'globalprep/SpeciesDiversity'
prod       <- 'v2015_sandbox'
dir_loc    <- file.path(dir_git, goal, prod)
dir_anx    <- file.path(dir_neptune_data, 'git-annex', goal, prod)
dir_mdl    <- file.path(dir_neptune_data, 'model')


# order of operations:

#   setup.sh -----
  # ??? Is this critical? Looks like it's just copying (linking) files to a new location for easy reference?
    setup <- function(dir_tmp, dir_anx, dir_mdl) {
      # mkdirs data tmp
        if(!dir.exists(dir_tmp)) dir.create(dir_tmp)
        # # link regions shapefile in geographic coordinate system
        # ??? why link, not copy?  Will we be changing these files, if so, should we be 
        # modifying the original version?  or is original likely to be changed elsewhere?  if not, copy rather than link? or:
        # ??? do we need all those bits and pieces, or just the .shp and .tif?
        # ??? can we just read these in directly into memory when we need 'em?
        dbf_ext      <- 'shp' # to link/copy *all* files, use: dbf_ext <- c('shp','dbf','shx','prj','sbn','sbx','shp.xml')
        data_loc     <- 'GL-NCEAS-OceanRegions_v2013a/data'
        fao_gcs      <- paste('rgn_fao_gcs.', dbf_ext,  sep = '')
        land_gcs     <- paste('land_gcs.',    dbf_ext,  sep = '')
        offshore_mol <- 'rgn_offshore_3nm_mol.tif'
        if(!file.exists(file.path(dir_tmp, fao_gcs))) 
           file.symlink(file.path(dir_mdl, data_loc, fao_gcs),  dir_tmp) # ??? file.copy to copy; file.link or file.symlink to create links
        if(!file.exists(file.path(dir_tmp, land_gcs))) 
           file.symlink(file.path(dir_mdl, data_loc, land_gcs), dir_tmp)
        if(!file.exists(offshore_mol)) 
           file.symlink(offshore_mol, dir_tmp)

      # get last year's species to check for change in status (especially for those that have gone EXtinct)
        spp_old <- file.path(dir_mdl, 'GL-NCEAS-SpeciesDiversity/ohi_spp/data/spp.csv')
        spp_old_rename <- 'spp_2012.csv'
        if(!file.exists(file.path(dir_tmp, 'spp_2012.csv'))) 
           file.symlink(spp_old, file.path(dir_tmp, 'spp_2012.csv'))
       
      # # spp for 2012
      # for f in cells spp cells cells_spp; do
      # echo $OHI_MODELDIR/GL-NCEAS-SpeciesDiversity_v2012/data/${f}.csv tmp/${f}_2012.csv
      # done
        # ??? do we need? echo in this context outputs the values of these variables to the screen.
    }
    setup(dir_tmp, dir_mdl, dir_anx)


#   setup_tmp.py -----
    setup_tmp <- function(dir_loc, dir_mdl) {
  # ??? Do we really need this?  Working from local is likely faster, and these are huge files, so maybe.
  # ??? This copies huge files from network to local - 3 GB and 12 GB respectively.
      # # move remote databases to local (removing from remote? depends on file.move (original python script) vs file.copy (as currently coded here))
      # 
      data_loc <- file.path(dir_loc, 'GL-NCEAS-SpeciesDiversity_v2013a/tmp')
      data_mdl <- file.path(dir_mdl, 'GL-NCEAS-SpeciesDiversity_v2013a/tmp')
      if(!dir.exists(data_loc)) dir.create(data_loc, recursive = TRUE)
      
      for (path in c('dummy_spp.txt', 'dummy_geodb.gdb')) {
        # path <- 'spp.db' # path <- 'geodb.gbd' # path <- 'dummy_spp.txt' # path <- 'dummy_geodb.gdb'
        l = file.path(data_loc, path)         # ??? create local pathname for one of the two databases
        r = file.path(data_mdl, path)         # ??? create remote pathname
        if (file.exists(r))                   # ??? if remote database exists,
          if (file.exists(l))                 # ???   and local database exists,
            unlink(l)                         # ???       delete the entire local directory tree (directory clean up)
          file.copy(r, data_loc, recursive = TRUE)   # ???   then (remote exists, local didn't exist or has been removed) copy remote -> local
      }
    }
    setup_tmp(dir_loc, dir_mdl)


#   ingest_aquamaps.R -----
  # ??? convert into functions to be called from this script?
  #     wrap script in function and source/call it from here
    ingest_aquamaps <- function() {
      library(logging)
      
      # Read in csv outputs from running raw\AquaMaps_OHI_082013\export.sql
      #   See raw\AquaMaps_OHI_082013\README.txt on a running instance of AquaMaps MySQL
      
      # set working directory
      old_wd <- getwd()
      wd = 'N:/model/GL-NCEAS-SpeciesDiversity_v2013a' # ??? is this the right place to be?
      setwd(file.path(wd, 'raw/AquaMaps_OHI_082013'))  # ??? set up a variable for it rather than moving?
      
      # setup logging
      basicConfig()
      addHandler(writeToFile, logger='', file=file.path(wd,'cache','ingest_aquamaps.log')) # ??? what does this do?
      
      # read in aquamaps header column names (hdr_*.csv) # ??? are these headers separate from the data? weird.
      loginfo('read in aquamaps header column names (hdr_*.csv)')
      cells.hdr     = read.csv('hdr_hcaf.csv'               , stringsAsFactors=F, header=F)[ , 1]
      cells_spp.hdr = read.csv('hdr_hcaf_species_native.csv', stringsAsFactors=F, header=F)[ , 1]
      spp.hdr       = read.csv('hdr_speciesoccursum.csv'    , stringsAsFactors=F, header=F)[ , 1]
      
      # read in aquamaps data (tbl_*.csv)
      loginfo('read in aquamaps data (tbl_*.csv)\n  cells')
      cells     = read.csv('tbl_hcaf.csv'               , header=F, col.names=cells.hdr    , na.strings='\\N')
      loginfo('  cells_spp')
      cells_spp = read.csv('tbl_hcaf_species_native.csv', header=F, col.names=cells_spp.hdr, na.strings='\\N')
      loginfo('  spp')
      spp       = read.csv('tbl_speciesoccursum.csv'    , header=F, col.names=spp.hdr      , na.strings='\\N')
      
      # write out to tmp
      setwd(file.path(wd, 'tmp')) 
    # ??? create loop for csvs
    #       e.g.  for x in c(cells, cells_spp, spp) {
    #               write file and log
    #             }
      loginfo('write out to tmp\n  am_cells_data.csv')
      write.csv(cells    , 'am_cells_data.csv'    , row.names=F, na='')
      loginfo('  am_cells_spp_data.csv')
      write.csv(cells_spp, 'am_cells_spp_data.csv', row.names=F, na='')
      loginfo('  am_spp_data.csv')
      write.csv(spp      , 'am_spp_data.csv'      , row.names=F, na='')
      loginfo('finished!')
      
      setwd(old_wd)
      
      return(TRUE)
    }

    ingest_aquamaps()
    


#   ingest_iucn.R -----
  # ??? convert into functions to be called from this script?
  #     wrap script in function and source/call it from here, or just source it?



#   ingest.py -----
# call the ingest.py Python script from within this R script, using rPython: 
#   http://www.r-bloggers.com/calling-python-from-r-with-rpython/
    python.load('ingest.py')


#   ingest_intersections.R -----
    ingest_intersections <- function() {

      dir_mdl_tmp <- file.path(dir_mdl, 'GL-NCEAS-SpeciesDiversity_v2013a/tmp')
      dir_mdl_cache <- file.path(dir_mdl, 'GL-NCEAS-SpeciesDiversity_v2013a/cache')
      dir_mdl_intsx <- file.path(dir_mdl_cache, 'iucn_intersections')

      ### read in spp
      spp <- read.csv(file.path(dir_mdl_tmp, 'spp_iucn_marine_global.csv'), stringsAsFactors = FALSE)

      ### get list of dbf files
      dbfs <- list.files(dir_mdl_intsx, glob2rx('range_*_mar_sid*_pts.dbf')) # 2553 before corals

      ### extract sid and group from dbf path name, and assign info into spp data.frame
      sids = as.integer(gsub('range_([a-z]+)_mar_sid([0-9]+)_pts.dbf','\\2', dbfs))
      grps =            gsub('range_([a-z]+)_mar_sid([0-9]+)_pts.dbf','\\1', dbfs)
      
      spp <- spp %>% mutate(
        shp_dbf = NA, # path to dbf
        shp_grp = NA, # group like birds or seacucumbers
        shp_cnt = NA) # number of points counted with intersection of rgn_fao_am_cells_pts_gcs
      idx = match(sids, spp$sid)
      # ??? do this with mutate?
      spp$shp_dbf[idx] = dbfs
      spp$shp_grp[idx] = grps

      ### compile monster table of species per cell. 
      cells_spp = data.frame(cid=integer(0), sid=integer(0)) 
        ### initialize cells_spp
      
      for (i in 1:length(dbfs)){ # i=1
        ### For loop:
        ### * Read in .dbf info for one species
        ### * rename ORIG_FID to cid
        ### * for all rows where the spp$sid matches this index of sids, set count = number of rows in dbf
        ### * to cells_spp, add the cid and sid for this species
        ### ??? This loop is very slow - file reading is slow. Use as.is = TRUE? use readr?
        
        # use 'ptm <- proc.time()' and 'proc.time() - ptm' around code to time execution.
        
        # read dbf data for one species
        dbf_data <- foreign::read.dbf(file.path(dir_mdl_intsx, dbfs[i]))
        if ('ORIG_FID' %in% names(d)){
          dbf_data <- dbf_data %>% rename(cid = ORIG_FID)
        }
        
        # log dbf path, group, and count of points to species record
        # ??? do this with mutate?
        spp$shp_cnt[spp$sid==sids[i]] = nrow(dbf_data) # spp[spp$sid==sids[i], ]
        
        # merge with cells_spp
        cells_spp <- cells_spp %>%
          bind_rows(dbf_data[ , c('cid','sid')])


      }
      
      write.csv(cells_spp, file.path(dir_mdl_tmp,'cells_spp_iucn.csv'), row.names=F, na='')
      write.csv(spp,       file.path(dir_mdl_tmp,'spp_iucn.csv'),       row.names=F, na='')
    }

    ingest_intersections()


#   model.R -----
source(file.path(dir_loc, 'model.R'))


finish_tmp <- function(dir_loc, dir_mdl) {
  # ??? This copies huge files from local to network - 3 GB and 12 GB respectively.
  # move local databases to remote  
  # ??? do we want to overwrite the data on Neptune? are we actually changing anything?
  
  data_loc <- file.path(dir_loc, 'GL-NCEAS-SpeciesDiversity_v2013a/tmp')
  data_mdl <- file.path(dir_mdl, 'GL-NCEAS-SpeciesDiversity_v2013a/tmp')
  if(!dir.exists(data_mdl)) dir.create(data_loc, recursive = TRUE)
  
  for (path in c('dummy_spp.txt', 'dummy_geodb.gdb')) { 
    # path <- 'spp.db' # path <- 'geodb.gbd'
    l = file.path(data_loc, path)         # ??? create local pathname for one of the two databases
    r = file.path(data_mdl, path)         # ??? create remote pathname
    if (file.exists(l))                   # ??? if remote database exists,
      if (file.exists(r))                 # ???   and local database exists,
        unlink(r)                         # ???       delete the entire local directory tree (directory clean up)
    file.copy(l, data_mdl, recursive = TRUE)   # ???   then (remote exists, local didn't exist or has been removed) copy remote -> local
    unlink(l)                             # ??? remove the file from the local drive
  }
}
finish_tmp(dir_loc, dir_mdl)


# TODO: move 'D:/best/tmp/GL-NCEAS-SpeciesDiversity_v2013a/spp.db'
#           r'D:\best\tmp\GL-NCEAS-SpeciesDiversity_v2013a\tmp\geodb.gdb'

# TODO: calculate 2012 SPP with 2013 regions
#       D:\best\docs\data\model\GL-NCEAS-SpeciesDiversity\ohi_spp