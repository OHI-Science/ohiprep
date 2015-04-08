#FAO_commodities
FAO Commodities data are used to determine the Natural Products goal.

##Files and folders:
* `./data_prep.R` script prepares the raw FAO commodities data based upon the most current year's report.  
This preparation includes interpreting various FAO-specific codes and flags, eliminating NAs prior to first 'reporting' year (first non-NA report), filtering commodities by product group, gap-filling, smoothing, and processing status results based on harvest relative to buffered peak value.
    * `ohiprep/src/R/fao_fxn.R` is sourced from `data_prep.R` to clean up FAO-specific flags and codes.
* `commodities2products.csv`: a lookup key to match certain commodities with the six NP products.
* `./R`: contains R scripts related to data processing.
    * `./R/np_fxn.R`: functions called from within ./data_prep.R for gap-filling etc.
* `./v2015`: contains raw, tmp, and data files for processing data posted in 2015.
    * `./v2015/raw`: raw FAO commodities data (see below).
    * `./v2015/tmp`: temporary files created during data processing
    * `./v2015/data`: outputs from `./data_prep.R`, including `np_gapfill_report.csv` and status score outputs.
* `./v2014_test`: similar to `./v2015`; contains raw, tmp, and data files for data posted in March 2014.
* `./gap_fill_explore`: Melanie's exploration of a number of possible gapfilling techniques.
    * `./gap_fill_explore.R`

##FAO Commodities data
The FAO fisheries and aquaculture web page (http://www.fao.org/fishery/topic/166235/en) provides instructions on downloading and installing their FishStatJ software.  Once you've done that, then:

* From the [same web page](http://www.fao.org/fishery/topic/166235/en), under **FishStatJ available workspaces,** download the Global datasets workspace to your computer.
* Start **FishStatJ**.
* Invoke the **Tools -> Import Workspace** command.
* In the Import Workspace dialog, change the current directory to where you have downloaded the workspace(s) and select it.
* Follow the directions to import the workspace (press **Next** a couple of times then **Install Workspace**)
    * It may take a while to import the workspace. Go make a sandwich, get some coffee, drink a beer, learn a new hobby.
* Open the two data sets: *Global commodities production and trade - Quantity* and *- Value*.
    * No need to filter; the `data_prep.R` script does that.
* For each data set, select all (`ctrl-A` or `command-A`), then **Edit -> Save selection (.csv file)...**  Save as these filenames: 
        `FAO_raw_commodities_quant_[start-year]_[end-year].csv` and
        `FAO_raw_commodities_value_[start-year]_[end-year].csv`
    * **Note:** in prior years, people have reported that this may not capture all the rows in the .csv file, so make sure to double-check.
* Put the resulting files in an appropriate folder and have fun!
