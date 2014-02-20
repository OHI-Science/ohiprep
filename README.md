ohigit
======

This GitHub repository is intended to store the versioned code and small files used to generate the data layers for the Ocean Health Index. We seperate handling of files via:

1. `git` any scripts, especially R or Python, used to generate data. Also any ancillary files (must be less than 100 MB) used to document the data products. These files will be versioned using **GitHub** and generated using **RStudio**. To see why we chose this software, see [Software Features](https://github.com/OHI-Science/ohidata/wiki/Software-Features). For help setting up the software, see [Setup](https://github.com/OHI-Science/ohidata/wiki/Setup).

1. `big` all other files, particularly binary non-text files which are larger, especially anything 50 MB or larger. These files will be stored on our file server NEPTUNE, where backups get performed (nightly, weekly, monthly) and not here in the GitHub repository. For more details, see [Accessing Big Data Files](https://github.com/OHI-Science/ohidata/wiki/Accessing-Big-Data-Files).

## Data Products

Folders herein should generally be organizied like so:

    [study area]/[provider]-[product]_[version]

where:

 * `study area` geographic extent of data corresponding to intended OHI study area of analysis 
 * `provider` original provider, which is NCEAS for custom products
 * `product` basic description of what the data product is
 * `version` for NCEAS products: v[year]{suffix}. Otherwise version is given by provider

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
