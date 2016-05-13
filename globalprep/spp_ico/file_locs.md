File locations:
See file storage notes in git-annex/globalprep/SpeciesDiversity/cco-data_org.md
data:
On Neptune - git-annex:
* SPP git-annex:    <Neptune>/git-annex/globalprep/SpeciesDiversity
* Data:
    * Aqua Maps:        <SPP git-annex>/raw/AquaMaps_*/
    * IUCN shapefiles:  <SPP git-annex>/raw/iucn_shp/
* Cached data files (scraped data and intersections):
    * IUCN details:     <SPP git-annex>/cache/iucn_details/
    * IUCN intersects:  <SPP git-annex>/cache/iucn_intersections/
* Region files: * note, these are currently symbolic links (shortcuts) to the original files
    * FAO regions:      <SPP git-annex>/regions/rgn_fao_gcs.shp
    * land shapefile:   <SPP git-annex>/regions/land_gcs.shp
* Script output files (only large files and collections of files here!)
    * work in progress: <SPP git-annex>/v201x/intermediate   * files to save for archive/reference (e.g. am_cells_data.csv, spp_iucn_marine.csv)
    * temp in progress: <SPP git-annex>/tmp                  * not in year folder! temporary = don't archive
  

On GitHub:
* SPP github: ~github/ohiprep/globalprep/SpeciesDiversity
* Scripts:
    * main script(s):   <SPP github>/  
    * functions etc:    <SPP github>/R
* Script output files: (only SMALL outputs and collections of outputs in GitHub)
    * work in progress: <SPP github>/v201x/intermediate   * files to save for archive/reference (e.g. am_cells_data.csv, spp_iucn_marine.csv)
    * temp in progress: <SPP github>/tmp                  * not in year folder! temporary = don't archive
    * final outputs:    <SPP github>/v201x/data
