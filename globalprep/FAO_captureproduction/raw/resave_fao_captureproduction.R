#resave_fao_captureproduction.r


# resave FAO capture production since the original file downloaded has duplicate columns: `Species (ASFIS species)` is for both scientific name and for common name. This has been removed using `resave_fao_captureproduction.r` and the data file to be used is called `FAO_captureproduction_1950_2013.csv`. 

dir_d = '~/github/ohiprep/globalprep/FAO_captureproduction/raw'

d = read.csv(file.path(dir_d, 'FAO_captureproduction_1950_2013_dup_columns.csv', 
             check.names=F, strip.white=T)
names(d)
names(d)[3] = 'Common_Name_ASFIS_species'
names(d)[4] = 'Scientific_Name_ASFIS_species'

write.csv(d, file.path(dir_d, 'FAO_captureproduction_1950_2013.csv'), row.names=F)
