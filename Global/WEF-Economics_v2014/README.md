README.md
========================================================

Final file created, ready for Toolbox: 

* rgn_wef_gci_2014a_rescaled.csv


Original data downloaded as a pdf from [World Economic Forum](http://www.weforum.org/issues/global-competitiveness) at this [link] (http://www3.weforum.org/docs/WEF_GlobalCompetitivenessReport_2013-14.pdf). This pdf is stored on Neptune on data_edit: git-annex/Global/WEF-Economics_v2014/WEF_GlobalCompetitivenessReport_2013-14.pdf

To create .csv files from these pdf tables, follow these steps 
TODO: make a python script for this:

* To copy data, use Adobe Acrobat Pro 9.0 
* Table was copied into WEF_GCI_2013-2014_Table3.txt and WEF_TTCI_2013-2014_Table1.txt 
* Table saved as WEF_GCI_2013-2014_Table3_reformatted.csv and WEF_TTCI_2013-2014_Table1_reformatted.csv with TextWrangler: 
  + add quotes around countries with commas (~5 of these) 
  + add commas to make it csv: 
    - search '(\w) (\d)' replace '\1,\2'; 
    - search '(\d) (\d)' replace '\1,\2'; 
    - search ' (n\/a)' replace ',NA' 
    - simplify/rename headers with commas

WEF_GCI_2013-2014_Table3_reformatted.csv is processed further by data_prep.R in ohiprep/Global/WEF-Economics_v2014/

