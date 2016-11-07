## Question about changing sea ice edge in the Arctic Sea.  See what is going on here.....

  p='n'
  
  ########################################################### 
  ### This section is mostly for visualization, but the 
  ### type (r.typ) (land, coast, shore, water) 
  ### and region (r.rgn) rasters that are created are used later
  ### (the r.ice and r.ice.edge rasters aren't used later)
  ###########################################################
  
  # identify the reference rasters for the north and south pole:
  if (p=='n'){
    l='n201203'; w=641; h=709
  } else {
    l='s201209'; w=940; h=671
  }
  
  # load the .rdata file created in the above function.
  # s = raster stack of the ice data for all years/months
  # pts = points data of OHI regions, NSIDC type of land cover, and data extracted from each NSIDC ice layer
  # realized I need to load in the full data (the one for this analysis truncates the sea ice from EEZ zones)
  load(file=sprintf('C:\\Users\\Melanie\\Desktop\\GL-NCEAS-SeaIce_v2013\\tmp\\%s_rasters_points.rdata',p))
  
  
  ########################################################### 
  ### Converts the (s)tack of rasters to (i)ce concentration:
  ###   - exclude land/coast (previously excluded water, but we want the FAO stats)
  ###   - divide raster by 250 to get proportion ice coverage
  ###########################################################
  # convert (s)tack of rasters to (i)ce concentration
  si = s
  
  # using the reference rasters ("l") for the north and south pole 
  r = raster(s,l) #select the "l" (i.e., reference) raster layer from the stack
  #create a NSIDC land cover types raster (used later on):
  r.typ = setValues(r, pts@data[['type_nsidc']]) 
  #create a proprtion ice cover (excluding land/coast) raster (used only to visualize):
  r.ice = r
  r.ice[r.typ < 2] = NA # include: shore(2), water(3), hole(4); exclude: land(0), coast(1)
  r.ice = r.ice/250 # covert ice score to proportion
  #create an ice edge habitat raster (ice between 10-50%) (used only to visualize):
  r.ice.edge = r.ice
  r.ice.edge[r.ice.edge<0.1 | r.ice.edge>0.5] = NA
  #create a region raster that excludes land and coast areas (used later on):
  r.rgn = setValues(r, pts@data[['rgn_id']]) #create a new raster with the regions 
  r.rgn[r.typ < 2] = NA # exclude: land(0), coast(1) from the regions
  
  
  si[is.na(r.rgn) | r.rgn==0] = NA # converting land/coast regions to NA
  si[si>250] = 250      # convert hole (value=251) to max ice
  si = si/250           # to get 0 to 1 ice proportion
  names(si) = names(s)
  
  ########################################################### 
  # Subset ice concentration stacks so that:
  # (s)tack of (i)ce edge (h)abitat based on concentrations within 0.1 and 0.5
  # (s)tack of (i)ce (p)rotection based on shore pixels with concentrations > 0.15 
  #    and located on shoreline 
  ###########################################################
  sih = si >=0.1 & si <=0.5
  sip = si >=0.15 & r.typ==2
  sip[is.na(r.rgn) | r.rgn==0] = NA
  names(sih) = names(s)
  names(sip) = names(s)
  
  
  ########################################################### 
  # Calculate average total ice coverage per year (across all years):
  # Sum pixels across all the layers in the sip and sih stacks
  # and divide by number of years (results in N=1 raster layer).
  #
  # Summarize by region (add all values in each region)
  ###########################################################
  # calculate Reference as average of total annual sea ice
  # sum sea ice of all months and divide by total number of years
  yrs.s = as.factor(substr(names(s),2,5))  # get reference to years by column
  rih.R = sum(sih)/length(levels(yrs.s)) # raster of ice habitat for Reference
  rip.R = sum(sip)/length(levels(yrs.s)) # raster of ice protection for Reference
  
  # summarize Reference by region
  #z.h.R = zonal(rih.R, r.rgn, sum)
  #z.p.R = zonal(rip.R, r.rgn, sum)    
  
  
  ################################################################ 
  # sum pixels across months for each year (results in N= 1 layer for each year)  
  ################################################################
  sih.yr = stackApply(sih, yrs.s, sum)
  sip.yr = stackApply(sip, yrs.s, sum)
  names(sih.yr) = levels(yrs.s)
  names(sip.yr) = levels(yrs.s)
  
  
  OHIregion <- readOGR(dsn="C:\\Users\\Melanie\\Desktop\\GL-NCEAS-SeaIce_v2013\\raw", layer=sprintf("New_%s_rgn_fao", p))
  bg <- raster(sih.yr, year)
  bg[bg>0] <- 0
  Arctic <- OHIregion[OHIregion$rgn_nam=="Arctic Sea", ]
  ANW <- OHIregion[OHIregion$rgn_nam=="Atlantic, Northwest", ]
  ANE <- OHIregion[OHIregion$rgn_nam=="Atlantic, Northeast", ]
  PNW <- OHIregion[OHIregion$rgn_nam=="Pacific, Northwest", ]
  
  
  plot(bg, col=rgb(192, 192, 192, 100, maxColorValue=256))
  plot(OHIregion, add=TRUE, )
  plot(Arctic, col="olivedrab3", add=TRUE)
  plot(ANW, col="blue1", add=TRUE)
  plot(ANE, col="firebrick1",  add=TRUE)
  plot(PNW, col="darkorange", add=TRUE)
  
  for(i in 1:length(names(sih.yr))){
 #i=1
  year <- names(sih.yr)[i]
  plot(raster(sih.yr, year), main=gsub("X", "", year))
  plot(OHIregion, add=TRUE)
}