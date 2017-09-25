### process_WEF.R: 
### Do not run stand-alone - source from main data_prep.R for TourismRecreation.
###
### reformat and add rgn_ids to World Economic Forum (WEF) data 
### 
###   Provenance:
###     Jun2015 Casey O'Hara - updated for 2015, removed gapfilling, set up for .csv instead of .pdf
###     Mar2014 JStewartLowndes; updated from 'clean_WEF.R' by JStewart in May 2013
###     May2013 'clean_WEF.R' by JStewart
###
###   Data: 
###     TTCI: Travel and Tourism competitiveness:
###       * download .xlsx: http://www3.weforum.org/docs/TT15/WEF_TTCR_Dataset_2015.xlsx
###     * note: only 2015 is represented here.  
###     * read report online: http://reports.weforum.org/travel-and-tourism-competitiveness-report-2015/
###       * table 1: http://reports.weforum.org/travel-and-tourism-competitiveness-report-2015/
###           index-results-the-travel-tourism-competitiveness-index-ranking-2015/
###
###     GCI: Global Competitiveness (not used in 2015 for TR goal; see data_prep_GCI.R in globalprep/WEF-Economics)
###       * download .xlsx: http://www3.weforum.org/docs/GCR2014-15/GCI_Dataset_2006-07-2014-15.xlsx
###     * note: contains data for each year from 2006/2007 to 2014/2015
###     * read report: http://reports.weforum.org/global-competitiveness-report-2014-2015/
###       * table 3 in this .pdf: http://reports.weforum.org/global-competitiveness-report-2014-2015/
###           wp-content/blogs.dir/54/mp/files/pages/files/tables3-7-wef-globalcompetitivenessreport-2014-15-2.pdf
###
###   read in individual files
###   call name_to_rgn() from ohicore



##############################################################################=
### WEF TTCI formatting ----
##############################################################################=
# read in files
ttci_raw <- read.csv(dir_wef, 
                     skip = 3, check.names = FALSE, stringsAsFactors = FALSE)
### NOTE: check.names = FALSE because of Cote d'Ivoire has an accent circonflex over the 'o' (probably other issues in there too)

ttci <- ttci_raw[ , names(ttci_raw) != '']
### first row is index scores for 2015.
### After column 150, a bunch of unnamed columns that throw errors

ttci <- ttci %>%
  filter(Series == "Global Competitiveness Index") %>%
  filter(Attribute == "Value") %>%
  select(-(1:2), -(4:8), year = Edition) %>%
  gather(country, value, -year) %>%
  mutate(score = as.numeric(value)) %>%
  select(year, country, score)

  
ttci <- ttci %>%
    mutate(country = as.character(country)) %>%
    mutate(country = ifelse(country == "Congo, Democratic Rep.", "Democratic Republic of the Congo", country)) %>%
    mutate(country = ifelse(country == "Côte d'Ivoire", "Ivory Coast", country))
  
  
ttci_rgn <- name_2_rgn(df_in = ttci, 
                       fld_name='country', 
                       flds_unique=c('country','year'))

ttci_rgn <- ttci_rgn %>%
  arrange(country, year) %>%
  select(rgn_id, rgn_name, year, score)

head(ttci_rgn, 10)
#        rgn_id year score    rgn_name
#     1      14 2015  4.35      Taiwan
#     2      15 2015  3.63 Philippines
#     3      16 2015  4.98   Australia
#     4      20 2015  4.37 South Korea
#     5      24 2015  3.24    Cambodia
#     6      25 2015  4.26    Thailand
#     7      31 2015  4.00  Seychelles
#     8      37 2015  3.90   Mauritius
#     9      40 2015  3.80   Sri Lanka
#     10     41 2015  2.81  Mozambique
### Save TTCI data file
write_csv(ttci_rgn, 'intermediate/wef_ttci.csv')
