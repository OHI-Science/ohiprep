ohiprep
=======

real deal is [here](http://github.com/ohi-science/ohiprep).
 * just testing
 * again
 * and again


This GitHub repository is intended to store the versioned code and other text files, including csv, used to prepare the data layers for the Ocean Health Index. We have seperate methods for handling files based on type:

1. `text` any scripts, especially R or Python, and other text files, like READMEs or csv, used to generate data. All files must be less than 100 MB and are preferably text. These files will be versioned using **GitHub** and most often generated using **RStudio**. To see why we chose this software, see [Software Features](https://github.com/OHI-Science/ohidata/wiki/Software-Features). 
2. `binary` all other files, particularly binary non-text files which are larger, especially anything 50 MB or larger. These files will be stored on our file server **neptune.nceas.ucsb.edu**, where backups are performed (nightly, weekly, monthly) and not here in the GitHub repository.

For help setting up the software, see [Setup](https://github.com/OHI-Science/ohidata/wiki/Setup).

## Data Products

Folders herein should generally be organizied like so:

    [study area]/[provider]-[product]_[version]

where:

 * `study area` geographic extent of data corresponding to intended OHI study area of analysis 
 * `provider` original provider, which is NCEAS for custom products
 * `product` basic description of what the data product is
 * `version` for NCEAS products: 
    - product version OR the most recent year of the whole data product
    - subsets of this data product can then be put in the data folder annexed by the OHI assessment year

All entries above should be in CamelCase.

### Study Areas

All CamelCase top level folders indicate the study area which correspond to the prefix on neptune's folders:
* `AQ` Antarctica
* `BR` Brazil
* `CC` CaliforniaCurrent
* `FJ` Fiji
* `GL` Global
* `MA` Massachusetts

## Administrative Folders

Other folders are:
* `src` source files for general R and Python code 
* `wiki` files, particularly images, used by the [ohidata wiki](https://github.com/OHI-Science/ohidata/wiki/_pages)
