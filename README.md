ohidata
=======

Ocean Health Index code used to generate data. This especially includes R or Python scripts and ancillary text files to document or use as inputs to generate larger data files, which get stored elsewhere (for now on the neptune file server). 

Folders herein should be organizied like so:

    [study area]/[provider]-[product]_[version]

where:

 * `study area` geographic extent of data corresponding to intended OHI study area of analysis 
 * `provider` original provider, which is NCEAS for custom products
 * `product` basic description of what the data product is
 * `version` for NCEAS products: v[year]{suffix}. Otherwise version is given by provider

All entries above should be in CamelCase.

Top level folders indicate the study area which correspond to the prefix on neptune's folders:
 * `AQ` Antarctica
 * `BR` Brazil
 * `CC` CaliforniaCurrent
 * `FJ` Fiji
 * `GL` Global
 * `MA` Massachusetts

For more, see the [ohidata wiki](https://github.com/OHI-Science/ohidata/wiki/_pages).
