### AS of the 2015 Antarctica Assessment, we use the data generated for the OHI 2015 pressures,
### All of these methods and data files are no longer relevant.
### Will delete once I get HS redone.

Step 1:
SummarizeZonalData.R = extracts the data from the rasters

Step 2: 
DataSummary.R = formats the extracted data by converting raw data to zero to 1 based on reference points (typically max eez values)

We are currently (Feb 25, 2015) exploring setting the reference point at the raster scale (rather than eez scale)