ohidata
=======

This GitHub repository is intened to store the code and small files used to generate the data layers for the Ocean Health Index. We seperate files into these categories:

* `code` any scripts, especially R or Python, used to generate data. Also any ancillary files (must be less than 100 MB) used to document the data products. These files will be versioned using GitHub.
* `data` all other files, particularly binary non-text files which are larger, especially anything 100 MB or larger. These files will be stored on our file server NEPTUNE, where backups get performed (nightly, weekly, monthly) and not here in the GitHub repository.

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
