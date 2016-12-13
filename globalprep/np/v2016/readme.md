## Ocean Health Index: Natural products

See full data prep details [here](https://rawgit.com/OHI-Science/ohiprep/master/globalprep/np/v2016/np_dataprep.html) for the following layers used to calculate the natural products goal:
* Relative harvest value
* Natural product harvest
* Relative harvest tonnes


If using these data, please see our [citation policy](http://ohi-science.org/citation-policy/).

[alternatively, if you want a specific citation for this resource, you can add that here....]


### Additional information
FAO Commodities data are used to determine the Natural Products goal.

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
