devtools::install_github("jcheng5/rasterfaster")
devtools::install_github("rstudio/leaflet@raster")

library(shiny)
library(raster)
library(rgdal)
library(leaflet)

#directories
dir_N = c('Windows' = '//neptune.nceas.ucsb.edu/data_edit',
          'Darwin'  = '/Volumes/data_edit',
          'Linux'   = '/var/data/ohi')[[ Sys.info()[['sysname']] ]]


#raster
# - rasters are read in as .tif but the app can only read rasters if they are '.grd' so my 
# quick and dirty trick is to read in .tif, then writeRaster(file,filename='file') <- no .tif 
# extension automatically created the .gri and .grd files 

setwd('globalprep/data_china_raster_vis')

uv    = raster('UV.grd')
nitro = raster('nitro.grd')   


#shapefiles 

china = readOGR(dsn=file.path(dir_N,'git-annex/clip-n-ship/chn/spatial'),layer= 'rgn_offshore_gcs')

showRaster <- function(r, defaultTiles = TRUE, colorFunc = colorBin("RdBu", c(minValue(r), maxValue(r)), 16)) {
  if (!inherits(r, "RasterLayer")) {
    stop("showRaster only works with raster layers")
  }
  if (!identical(r@file@driver, "raster")) {
    stop("showRaster only works with .grd files")
  }
  
  ui <- tagList(
    tags$head(tags$style(type="text/css",
                         "html, body { width: 100%; height: 100%; padding: 0; margin: 0; }"
    )),
    leafletOutput("map", width = "100%", height = "100%")
  )
  
  server <- function(input, output, session) {
    output$map <- renderLeaflet({
      
      l <- leaflet() %>% setView(0, 0, zoom = 1)
      
      opacity <- 1
      if (defaultTiles) {
        l <- l %>% addTiles(options = tileOptions(noWrap = TRUE))
        opacity <- 0.5
      }
      
      l <- l %>% addRaster(r, options = tileOptions(opacity = opacity, noWrap = TRUE, detectRetina = FALSE),
                           colorFunc = colorFunc)%>%
        #addPolygons(data=cal)%>%
        addPolygons(data=china,fillColor=NULL,weight = 1,color='black')
      
      l
    })
  }
  
  shinyApp(ui, server, options = list(launch.browser = rstudio::viewer))
}


# MUST be 1) .grd file, 2) with `numeric` data, and 3) in unprojected WGS84.
# If your raster is in a different file format, use writeRaster() to create
# a .grd version. Performance is quite insensitive to resolution/size, so
# provide the highest resolution data you can.
#r <- raster("globalprep/pressures_decision_tree/shinyapps/decision_tree/data/sst.grd")

# Show the map
showRaster(nitro, TRUE)