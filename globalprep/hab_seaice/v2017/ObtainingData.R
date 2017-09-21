for (p in poles){ 
  #p='s'  #testing
  
  ######################################################
  ## Create an empty raster stack with appropriate dimensions 
  ## and CRS.
  ######################################################
  
  # extents from NSIDC (http://nsidc.org/data/polar_stereo/ps_grids.html)
  if (p=='n'){
    xMin=-3850000; yMin=-5350000; nr=448; nc=304; prj=prj.n; ub=ub.n
  } else if (p=='s'){
    xMin=-3950000; yMin=-3950000; nr=332; nc=316; prj=prj.s; ub=ub.s
  }
  xMax = xMin + (pixel*nc); yMax = yMin + (pixel*nr) 
  r = raster(nrow=nr, ncol=nc, xmn=xMin, xmx=xMax, ymn=yMin, ymx=yMax)
  projection(r) = prj
  s = stack(r)
  
  
  #########################################################
  ## Collect the data for each month/year from the website
  ## and add to raster stack
  #########################################################
  
  for (yr in years){
    for (mo in months){ 
      
          #yr=1979; mo=1  #testing
      
      ### Getting the proper ftp site based on the time of data collection    
      i.pym = i.pym+1 
      ym = yr*100+mo
      y.m = sprintf('%d-%02d',yr,mo)
      p.y.m = sprintf('%s%d%02d',p,yr,mo)
      if (ym < 198709){
        ss = 'n07'
      } else if (ym >= 198709 & ym < 199201){
        ss = 'f08'
      } else if (ym >= 199201 & ym < 199510){
        ss = 'f11'
      } else if (ym >= 199510 & ym < 200801){
        ss = 'f13'
      } else if (ym >= 200801){
        ss = 'f17'
      }
      
      ### retrieving the data
      min.done = as.numeric(difftime(Sys.time(), t0, units='mins'))
      min.togo = (n.pym-i.pym) * min.done/i.pym
      print(sprintf('Retrieving %s (%d of %d). Minutes done=%0.1f, to go=%0.1f',p.y.m,i.pym,n.pym,min.done,min.togo))
      u = sprintf('%s/nt_%d_%s_v1.1_%s.bin', ub, ym, ss, p)
      con <- file(u, 'rb')  # 'rb' = 'open for reading in binary mode'
      x = readBin(con , "raw", 300)      
      x = readBin(con,"int", size=1, signed=FALSE, 150000)
      close(con)    
      
      ### Place result in raster framework
      r = setValues(r, x)	
      # raster values: 254=land, 253=coast, 251= north pole assumed ice not seen by satellite, 
      # 0 to 250 / 250 = % ice concentration.  see raster::calc
      
      ####################################################################################################
      ### Next function only runs if the pts.shp file does not exist (it should exist in the folder).
      ### First, this takes the ice data and coverts it to a tif file (n_type or s_type) that identifies
      ### the: coast, land, hole, water, shore (25 km offshore from coast) based on codes in each ice data file.
      ### 
      ### Second, the OHI region shapefile is read in (the CRS has been transformed using arcGIS).
      ### Originally, the following was performed using arcGIS using a python script, but these functions are now done in R 
      ### (although, most of this has been converted to R, except for step 1 - which was performed in arcGIS), that:
      ###    1. convert the OHI regions shp file to a raster with the appropriate extent
      ###    3. converts the raster to a points shp file
      ###    4. appends the "type" information generated from the nsdic data (i.e., the n_type or s_type data) to the OHI raster:
      ###        land = 0
      ###        coast = 1
      ###        shore = 2
      ###        water = 3
      ###        hole = 4 (north pole)
      ###    5. saves file as: s_type_rgns_pts.shp or n_type_rgns_pts.shp
      ### 
      #######################################################################################################
      pts.shp = file.path(maps, sprintf('tmp/%s_type_rgns_pts.shp', p))
      
      if (!file.exists(pts.shp)){ #if this file exists in the working directory this is not run.
        ## This takes the raster cells that are identified as something other than ice (i.e., land, water, etc.)
        ## and creates new raster layers with just those cells
        r_coast = r==253
        r_land = r==254
        r_hole = r==251
        r_water = r<=250
        r_coast_na = calc(r_coast, fun=function(x) { x[x==0] = NA; return(x) })  #replaces 0 values with NA in r_coast file
        r_coast_distance = distance(r_coast_na) #calculates distance from coast (units are in meters)
        r_shore = r_water==1 & r_coast_distance<pixel*1.1 #selects one pixel offshore from coast: 25km offshore.  
        r_type = r
        r_type[r_land==1]=0
        r_type[r_coast==1]=1
        r_type[r_water==1]=3
        r_type[r_shore==1]=2        
        r_type[r_hole==1]=4    
        writeRaster(r_type, file.path(dir_M, sprintf("git-annex/globalprep/_raw_data/NSIDC_SeaIce/v2017/tmp/%s_type.tif",p)), overwrite=T)
        r.typ <- raster(file.path(dir_M, sprintf("git-annex/globalprep/_raw_data/NSIDC_SeaIce/v2017/tmp/%s_type.tif",p)))
        
      
        OHIregion <- read_sf(dsn=file.path(dir_M, "git-annex/globalprep/_raw_data/NSIDC_SeaIce/v2015/raw"), layer=sprintf("New_%s_rgn_fao", p))
   
        # clean up regions with no geometry (northern hemisphere)
        OHIregion <- OHIregion %>%
          dplyr::filter(!is.na(st_dimension(OHIregion))) %>%
          st_cast("MULTIPOLYGON")

        OHIregion_raster <- fasterize(OHIregion, r.typ, field="rgn_id") # convert shapefile to a raster
        OHIregion_raster[is.na(OHIregion_raster)] <- 0   #replace missing values with zero
        OHIregion_points <- st_as_sf(rasterToPoints(OHIregion_raster, spatial=TRUE)) #convert raster to points simple feature file
        names(OHIregion_points)[1] <- "rgn_id"
        OHIregion_points <- dplyr::left_join(OHIregion_points, 
                                             OHIregion %>%
                                               st_set_geometry(NULL), 
                                             by="rgn_id") #add some data to raster
        OHIregion_points <- as(OHIregion_points, "Spatial") # hopefully will be able to delete this at some point,
                                                            # currently can't extract using sf objects
        # detach("package:dplyr", unload=TRUE)
        # detach("package:tidyr", unload=TRUE)
        library("raster")
        OHIregion_points@data$type_nsidc <- raster::extract(r.typ, OHIregion_points) #extract data from the ice data created above
        writeOGR(OHIregion_points, dsn=file.path(maps, "tmp") , driver='ESRI Shapefile', layer=sprintf('%s_type_rgns_pts',p)) #save file
      }
      
      #########################################################
      ## Add each downloaded raster to the raster stack and 
      ## extract data from each raster and save to shp point file
      #########################################################
      
      ### If at the start of the data, this reads in the points shp file created above
      # this assumes that the range starts with 1979:
      if (yr==1979 & mo==1){
        pts =  readOGR(dsn=file.path(maps, "tmp"), layer=sprintf("%s_type_rgns_pts", p))
      }
      
      # add raster data (r) to the stack (s) and name:
      s.names = names(s) # class(s) # nlayers(s)
      if (nlayers(s) == 0){ 
        s = stack(r)
        names(s) = p.y.m
      } else {
        s = stack(s, r)
        names(s) = c(s.names, p.y.m)   
      }
      
      
      # extracts data from the downloaded raster and appends it to the shp file
      pts@data[p.y.m] = raster::extract(r, pts) # summary(pts@data[p.y.m]); head(pts@data); table(pts$s198001, pts$rgn)
      
    } # end mo  
    
  } # end yr
  
  # save stack of rasters and pts of shore as rdata file
  save_loc <- file.path(dir_M, sprintf("git-annex/globalprep/_raw_data/NSIDC_SeaIce/%s/%s_rasters_points.rdata", assessYear, p))
  save(s, pts, file=save_loc)
} # end polar
