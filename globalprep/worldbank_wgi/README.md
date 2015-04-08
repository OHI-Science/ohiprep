# README.md

Starting with eez2015, we will access WGI data (from World Bank) using their API and the R package `WGI`, available at:
https://github.com/vincentarelbundock/WDI

Data in the `raw` folder was archived after access in April 2015: write.csv(d, file.path(dir_wgi, 'raw', 'worldbank_wgi_from_wdi_api.csv'), row.names=F)