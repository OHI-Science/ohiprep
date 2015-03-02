# LSP notes

The original data and additional tmp files are located on Neptune:
N:\git-annex\globalprep\WDPA_MPA\v2015

model.py: 
* Uses pre-prepared rasters of 3nm offshore and 1 km inland regions to convert WDPA-MPA polygons to a raster. Note: cell size for WDPA-MPA raster is driven by region rasters.
* Extracts data for protected areas within the two coastal regions, outputs as tables (.dbf).
* More information about the .py script is found in the README_dataDescription.html (in the v2014_archive folder)

digest.R: prepares the final data (using processed data within .\tmp) for the Toolbox.

.\tmp: contains processed data (.dbf outputs) from model.py.  .\tmp\new_rasters_500mcell contains the outputs from the most recent analysis based upon 500 m raster cells.

.\data: contains final data (.csv outputs) from digest.R for use in the Toolbox.

.\DataExplore: additional analysis to compare and verify results of basing analysis on finer-resolution rasters (500 m vs 1000 m).  explore.R performs analyses, and reports outputs of these analyses in the other files within this directory.

