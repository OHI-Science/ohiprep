MySQL steps:
installed MySQL 5.6 from Windows Installer (http://dev.mysql.com/downloads/installer/)
stopped service
copied N:\model\GL-NCEAS-SpeciesDiversity_2013\raw\OHI_082013\*.frm, *.MYD, *.MYI files into 
       C:\ProgramData\MySQL\MySQL Server 5.6\data\test
Gave user "NETWORK SERVICE" permissions to D:\best\tmp\aquamaps_tmp_out
Ran the following SQL:
	SELECT * INTO 
	OUTFILE 'N:/model/GL-NCEAS-SpeciesDiversity_2013/raw/AquaMaps_OHI_082013/tbl_speciesoccursum.csv' 
	FIELDS TERMINATED BY ',' OPTIONALLY ENCLOSED BY '"' 
	LINES TERMINATED BY '\n' 
	FROM test.speciesoccursum;

	SELECT * INTO 
	OUTFILE 'N:/model/GL-NCEAS-SpeciesDiversity_2013/raw/AquaMaps_OHI_082013/tbl_hcaf_species_native.csv' 
	FIELDS TERMINATED BY ',' OPTIONALLY ENCLOSED BY '"' 
	LINES TERMINATED BY '\n'
	FROM test.hcaf_species_native;

	SELECT * INTO 
	OUTFILE 'N:/model/GL-NCEAS-SpeciesDiversity_2013/raw/AquaMaps_OHI_082013/tbl_hcaf.csv' 
	FIELDS TERMINATED BY ',' OPTIONALLY ENCLOSED BY '"' 
	LINES TERMINATED BY '\n'
	FROM test.hcaf;
Copied to N:\model\GL-NCEAS-SpeciesDiversity_2013\raw\AquaMaps_OHI_082013\MySQL_files
