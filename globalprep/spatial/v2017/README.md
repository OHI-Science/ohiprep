The changes in the spatial data correct an issue that raster::rasterize has dealing with polygon holes.

Instead of using the raster package we used fasterize to create the raster which has fewer issues
according to our tests.

I also cleaned the polygon file so it is more useful.

Code is updated to use the sf (simple feature) package for dealing with spatial data.