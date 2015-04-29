
dir_github <- getwd()
  # should be ~/github/ohiprep if just started
goal       <- 'globalprep/SpeciesDiversity'
dir_local  <- file.path(dir_git, goal, 'v2015_sandbox')

source('src/R/common.R')
  # assume starting in ~/github/ohiprep



# order of operations:

#   setup.sh -----
  # ??? can this be done in R?  Looks like it's just copying (linking) files to a new location for easy reference?
    setup_sh <- function() {
      # mkdirs data tmp
        dir_anx <- '/Volumes/data_edit/git-annex/globalprep/SpeciesDiversity/v2015_sandbox'
          # ??? where do we want these files if we want to keep them at all?
          # ??? store in git-annex rather than model...
        dir_mdl <- '/Volumes/data_edit/model' # ??? for Mac...? Make generic for Neptune drive?
        dir_tmp <- file.path(dir_anx, 'tmp')
        if(!file.exists(dir_tmp)) 
          dir.create(dir_tmp)
        if(!file.exists(file.path(dir_anx, 'data'))) 
          dir.create(file.path(dir_anx, 'data')) # ??? create this later, when we need it? otherwise risk orphaning a folder. Ugh!
        # # link regions shapefile in geographic coordinate system
        # ??? why link, not copy?  Will we be changing these files, if so, should we be 
        # modifying the original version?  or is original likely to be changed elsewhere?  if not, copy rather than link? or:
        # ??? do we need all those bits and pieces, or just the .shp and .tif?
        # ??? can we just read these in directly?
      # ln -sf $OHI_MODELDIR/GL-NCEAS-OceanRegions_v2013a/data/rgn_fao_gcs.{shp,dbf,shx,prj,sbn,sbx,shp.xml} tmp
      # ln -sf $OHI_MODELDIR/GL-NCEAS-OceanRegions_v2013a/data/land_gcs.{shp,dbf,shx,prj,sbn,sbx,shp.xml} tmp
      # ln -sf $OHI_MODELDIR/GL-NCEAS-OceanRegions_v2013a/data/rgn_offshore_3nm_mol.t* tmp
        dbf_ext      <- 'shp' # to link/copy all files, use: dbf_ext <- c('shp','dbf','shx','prj','sbn','sbx','shp.xml')
        fao_dbf      <- paste('GL-NCEAS-OceanRegions_v2013a/data/rgn_fao_gcs.', dbf_ext, sep = '')
        fao_gcs      <- file.path(dir_mdl, fao_dbf)
        land_dbf     <- paste('GL-NCEAS-OceanRegions_v2013a/data/land_gcs.',    dbf_ext, sep = '')
        land_gcs     <- file.path(dir_mdl, land_dbf)
        offshore_mol <- file.path(dir_mdl, 'GL-NCEAS-OceanRegions_v2013a/data/rgn_offshore_3nm_mol.tif')
        if(!file.exists(fao_gcs)) 
           file.symlink(fao_gcs,      dir_tmp) # ??? file.copy to copy; file.link or file.symlink to create links
        if(!file.exists(land_gcs)) 
           file.symlink(land_gcs,     dir_tmp)
        if(!file.exists(offshore_mol)) 
           file.symlink(offshore_mol, dir_tmp)

      # 
      # # get last year's species to check for change in status (especially for those that have gone EXtinct)
      # ln -sf $OHI_MODELDIR/GL-NCEAS-SpeciesDiversity/data/spp.csv tmp/spp_2012.csv
        spp_old <- file.path(dir_anx, 'GL-NCEAS-SpeciesDiversity/ohi_spp/data/spp.csv')
        spp_old_rename <- 'spp_2012.csv'
        if(!file.exists(file.path(dir_tmp, 'spp_2012.csv'))) 
           file.symlink(spp_old, file.path(dir_tmp, 'spp_2012.csv'))
      # 
      # # spp for 2012
      # for f in cells spp cells cells_spp; do
      # echo $OHI_MODELDIR/GL-NCEAS-SpeciesDiversity_v2012/data/${f}.csv tmp/${f}_2012.csv
      # done
        # ??? echo in this context outputs the values of these variables to the screen.
        # If we want to do this in R, great, otherwise delete.
    }
    setup_sh()


#   setup_tmp.py -----
  # ??? Do this in R?  Copy data from remote to local
      # # move remote databases to local
      # import os, shutil
      # 
      # local = r'D:\best\tmp\GL-NCEAS-SpeciesDiversity_v2013a'
        ### ??? data location at Ben's local computer
      # remote = r'N:\model\GL-NCEAS-SpeciesDiversity_v2013a\tmp'
        ### ??? data location at Neptune
      # 
      # if not os.path.exists(local):
      #   os.makedirs(local)
        ### ??? if the local directory doesn't exist, create it.
      # 
      # for path in ('spp.db', 'geodb.gdb'):
      #   l = os.path.join(local, path)   # ??? create local pathname for one of the two databases
      #   r = os.path.join(remote, path)  # ??? create remote pathname
      #   if os.path.exists(r):           # ??? if remote database exists,
      #     if os.path.exists(l):         # ???   and local path exists,
      #       if os.path.isdir(l):        # ???     and local path is a directory?
      #         shutil.rmtree(l)          # ???       delete the entire local directory tree (directory clean up)
      #       else:                       # ???     otherwise, (i.e. local path is not a directory)
      #         os.unlink(l)              # ???       unlink (delete the local file - doesn't work for directories) (file cleanup)
      #     shutil.move(r, l)             # ???   then (remote exists, local didn't exist or has been removed) copy remote -> local



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
  #     wrap script in function and source/call it from here, or just source?



#   ingest.py -----
# call the ingest.py Python script from within this R script, using rPython: 
#   http://www.r-bloggers.com/calling-python-from-r-with-rpython/
    


#   ingest_intersections.R -----
  # ??? convert into functions to be called from this script
      # # "C:\Program Files\R\R-3.0.1\bin\x64\Rscript.exe" N:\model\GL-NCEAS-SpeciesDiversity_v2013a\ingest_intersections.R
      # 
      # library(foreign)
      # library(plyr)
      # 
       dir_mdl <- '/Volumes/data_edit/model/GL-NCEAS-SpeciesDiversity_v2013a' # ???
       dir_mdl_tmp <- file.path(dir_mdl, 'tmp')
       dir_mdl_cache <- file.path(dir_mdl, 'cache')
       dir_mdl_intsx <- file.path(dir_mdl_cache, 'iucn_intersections')
       dir_anx <- '/Volumes/data_edit/git-annex/globalprep/SpeciesDiversity/v2015_sandbox'
      # setwd(wd) # ??? stay put in ohiprep?
      # 
      # # read in spp
      spp <- read.csv(file.path(dir_mdl_tmp, 'spp_iucn_marine_global.csv'))
        # include not as factors? use read_csv?
      # 
      # # show extinction risk category and population trend
      spp1 <- spp %>%
        mutate(category = as.character(category)) # de-factorize in read.csv?
#       spp$category <- factor(as.character(spp$category),
#                              levels=c('DD','LC','NT','VU','EN','CR','EX'), ordered=T)
      # table(spp$category)
      # #   DD   LC   NT   VU   EN   CR   EX 
      # # 2138 4512  530  653  200  122   18
      # table(spp$popn_trend)
      # #            Decreasing Increasing     Stable    Unknown 
      # #        250       1261        164        983       5515
      # 
      # 
      # # get list of dbf files
       dbfs <- list.files(dir_mdl_intsx, glob2rx('range_*_mar_sid*_pts.dbf')) # 2553 before corals
      # 
      # # extract sid and group from dbf path, and assign info
       sids = as.integer(gsub('range_([a-z]+)_mar_sid([0-9]+)_pts.dbf','\\2', dbfs))
       grps =            gsub('range_([a-z]+)_mar_sid([0-9]+)_pts.dbf','\\1', dbfs)
      # spp$shp_dbf = NA # path to dbf
      # spp$shp_grp = NA # group like birds or seacucumbers
      # spp$shp_cnt = NA # number of points counted with intersection of rgn_fao_am_cells_pts_gcs
      # idx = match(sids, spp$sid)
      # spp$shp_dbf[idx] = dbfs
      # spp$shp_grp[idx] = grps
      # 
      # # compile monster table of species per cell
      # cells_spp = data.frame(cid=integer(0), sid=integer(0))
      # for (i in 1:length(dbfs)){ # i=1
      #   
      #   # read data
      #   d = foreign::read.dbf(dbfs[i])
      #   if ('ORIG_FID' %in% names(d)){
      #     d = rename(d, c('ORIG_FID'='cid'))
      #   }
      #   
      #   # log dbf path, group, and count of points to species record
      #   spp$shp_cnt[spp$sid==sids[i]] = nrow(d) # spp[spp$sid==sids[i], ]
      #   
      #   # merge with cells_spp
      #   cells_spp = rbind(cells_spp, d[,c('cid','sid')]) # ??? rbind is deprecated - bind_rows()
      # }
      # 
      # write.csv(cells_spp, file.path(dir_mdl_tmp,'cells_spp_iucn.csv'), row.names=F, na='')
      # write.csv(spp,       file.path(dir_mdl_tmp,'spp_iucn.csv'),       row.names=F, na='')
    


#   model.R -----
# wrap in function and call from this script? or just source?



#   finish_tmp.py -----
#     # move local databases to remote
#     ??? basically the reverse of setup_tmp.py?
#     import os, shutil
#     
#     local = r'D:\best\tmp\GL-NCEAS-SpeciesDiversity_v2013a'
#     remote = r'N:\model\GL-NCEAS-SpeciesDiversity_v2013a\tmp'
#     
#     for path in ('spp.db', 'geodb.gdb'):
#       l = os.path.join(local, path)
#       r = os.path.join(remote, path)
#       if os.path.exists(l):
#         if os.path.exists(r):
#           if os.path.isdir(r):
#             shutil.rmtree(r)
#           else:
#             os.unlink(r)
#         shutil.move(l, r)
#     #shutil.rmtree(local)
#     
    

source('ingest_intersections.R')
    
source('model.R')

# TODO: move 'D:/best/tmp/GL-NCEAS-SpeciesDiversity_v2013a/spp.db'
#           r'D:\best\tmp\GL-NCEAS-SpeciesDiversity_v2013a\tmp\geodb.gdb'

# TODO: calculate 2012 SPP with 2013 regions
#       D:\best\docs\data\model\GL-NCEAS-SpeciesDiversity\ohi_spp