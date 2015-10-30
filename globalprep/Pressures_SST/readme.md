##Sea Surface Temperature Pressure Layer for OHI 2015

If the data produced here is used in publication, please make attribution clear. For citation see the most current release [here](https://github.com/OHI-Science/ohiprep/releases)

### Data

Data comes from [CoRTAD version 5](https://data.noaa.gov/dataset/the-coral-reef-temperature-anomaly-database-cortad-version-5-global-4-km-sea-surface-temperatub705f)

**Native Data Resolution**: 4km   
**Values**: Temperature in Kelvin  
**Time Range**: 1982 - 2012 (weekly averages across all years)  
**Format**: NetCDF  

### Scripts

1. `data_explore.R` looks at the raw data
2. `dataprep.R` takes the raw data and calculates standard deviation across all weeks, and then calculates the number of annual anomalies
3. `create_sst_layer.Rmd`  
    a. calculates mean number of annual SST anomalies for years 1985-1989, 2005-2008, 2006-2009, 2007-2010, 2008-2011 and 2009-2012
    b. calculates the difference between all recent years and the historical mean (1985-1989), only counting positive anomalies (an increase in number of anomalies over time)
    c. rescales each of the rasters to the 99.99th quantile

### Updates from previous assessment

The  data was updated using the same data source as previous assessments, but new years of data. CoRTAD v5 now has data from 1982 through 2012. Previous assessments used SST anomaly data up to 2010. The climatological mean for SST anomalies in previous asssessments used all years 1982-2010, and in this assessment we used the mean across all 30 years (1982-2012).

***

### Methods Overview

For full procedure see [create_sst_layer.html]():

For each week (1 to 53) across all years, the standard deviation of Weekly SST averages is calculated. As an example here is the standard deviation for week 1 across all years:

![](./images/sst_sd_week1.png)

Then, for every year we are interested in knowing how many weeks are anomalous (defined as greater than the standard deviation). Negative anomalies are ignored.

Each week in a year is compared against the standard deviation for that week, and if the cell is greater, it is assigned a 1. This creates a single raster layer for each year from 1982 to 2012 that shows the number of anomalous weeks each cell has (from 0 to 53). Here is an example for 2012:

![](./images/pos_anomalies_2012.png)

***

The SST pressure layer is created by looking at the change in these anomalies between the 5 most recent year (2008-2012)s, and 5 historical years (1985-1989). The years 1985-1989 were chosen to avoid the large El Nino event in 1982-1983.

![](./images/change_sst_08_12_85_90.png)



This layer was rescaled from 0 to 1 using the 99.99th quantile as a reference point.

![](./images/final_sst_layer.png)

**References**

CoRTAD v5

