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
###     GCI: Global Competitiveness (not used in 2015, left for reference)
###       * download .xlsx: http://www3.weforum.org/docs/GCR2014-15/GCI_Dataset_2006-07-2014-15.xlsx
###     * note: contains data for each year from 2006/2007 to 2014/2015
###     * read report: http://reports.weforum.org/global-competitiveness-report-2014-2015/
###       * table 3 in this .pdf: http://reports.weforum.org/global-competitiveness-report-2014-2015/
###           wp-content/blogs.dir/54/mp/files/pages/files/tables3-7-wef-globalcompetitivenessreport-2014-15-2.pdf
###
###   read in individual files
###   call name_to_rgn() from ohicore


##############################################################################=
### setup ----
##############################################################################=
### Libraries and such are set up within data_prep.R

dir_wef  <- file.path(dir_anx, '../WEF-Economics')


##############################################################################=
### WEF TTCI formatting ----
##############################################################################=
# read in files
ttci_raw <- read.csv(file.path(dir_wef, scenario, 'raw', 'WEF_TTCR_Dataset_2015.csv'), 
                     skip = 3, check.names = FALSE, stringsAsFactors = FALSE)
### NOTE: check.names = FALSE because of Cote d'Ivoire has an accent circonflex over the 'o' (probably other issues in there too)

ttci <- ttci_raw[1, names(ttci_raw) != '']
### first row is index scores for 2015.
### After column 150, a bunch of unnamed columns that throw errors

ttci <- ttci %>%
  select(-(1:2), -(4:9), year = Edition) %>%
  gather(country, value, -year)

### Rescale all scores (out of 7) to range from 0 - 1. 
ttci <- ttci %>%
  mutate(score = as.numeric(value)/7)

ttci_rgn <- name_to_rgn(ttci, fld_name='country', 
                        flds_unique=c('country', 'year'), fld_value='score', 
                        collapse_fxn = 'mean', add_rgn_name = T) %>%
  arrange(rgn_id, year)

stopifnot(max(ttci_rgn$score, na.rm = T) < 1)

head(ttci_rgn, 10)
#        rgn_id year     score    rgn_name
#     1      14 2015 0.6214286      Taiwan
#     2      15 2015 0.5185714 Philippines
#     3      16 2015 0.7114286   Australia
#     4      20 2015 0.6242857 South Korea
#     5      24 2015 0.4628571    Cambodia
#     6      25 2015 0.6085714    Thailand
#     7      31 2015 0.5714286  Seychelles
#     8      37 2015 0.5571429   Mauritius
#     9      40 2015 0.5428571   Sri Lanka
#     10     41 2015 0.4014286  Mozambique

### Save TTCI data file
ttci_file <- file.path(dir_git, scenario, 'intermediate/wef_ttci_2015.csv')
write_csv(ttci_rgn, ttci_file)



##############################################################################=
### WEF GCI formatting ----
##############################################################################=
### This data set was exploratory for previous years.  Not used in 2015.
{
# read in files ----
gci_raw <- read.csv(file.path(dir_wef, scenario, 'raw', 'GCI_Dataset_2006-07-2014-15.csv'), 
                    skip = 3, check.names = FALSE, stringsAsFactors = FALSE)
### NOTE: check.names = FALSE because of Cote d'Ivoire has an accent circonflex over the 'o' (probably other issues in there too)

### Possible filters:
### * Placement == 158
### * GLOBAL ID == GCI
gci <- gci_raw %>%
  filter(`GLOBAL ID` == 'GCI') %>%
  select(-(1:2), -(4:7), edition = Edition, attribute = Attribute) %>%
  gather(country, value, -(edition:attribute)) %>%  
    # convert to long format by country and value
  filter(attribute == 'Value') %>%
  select(-attribute)

### delete summary statistic rows
summary_categories <- c('Average GCR', 'Latin America and the Caribbean',	'Emerging and Developing Asia',	'Middle East, North Africa, and Pakistan',	
                        'Sub-Saharan Africa',	'Commonwealth of Independent States',	'Emerging and Developing Europe',	'Advanced economies',	
                        'Low income',	'Lower middle income',	'Upper middle income',	'High income: OECD',	'High income: nonOECD',	'ASEAN',	
                        'Stage 1',	'Transition from 1 to 2',	'Stage 2',	'Transition from 2 to 3',	'Stage 3')
gci <- gci %>% filter(!(country %in% summary_categories))

### Clean up:
### * Rescale all scores (out of 7) to range from 0 - 1. 
### * Convert 'edition' to 'year'.
### * clean up South Korea name in prep for name_to_rgn function
gci <- gci %>%
  mutate(score = as.numeric(value)/7,
         year  = as.integer(substr(edition, 1, 4)),
         country = str_replace(country, 'South Korea, Rep.', 'South Korea')) %>%
  select(-edition)

gci_rgn <- name_to_rgn(gci, fld_name='country', 
                       flds_unique=c('country', 'year'), fld_value='score', 
                       collapse_fxn = 'mean', add_rgn_name = T) %>%
  arrange(rgn_id, year)

stopifnot(max(gci_rgn$score, na.rm = T) < 1)

head(gci_rgn %>% filter(year == 2006), 10)
#        rgn_id year     score    rgn_name
#     1      14 2006 0.7637536      Taiwan
#     2      15 2006 0.5679714 Philippines
#     3      16 2006 0.7397451   Australia
#     4      20 2006 0.7241150 South Korea
#     5      24 2006 0.4914073    Cambodia
#     6      25 2006 0.6795859    Thailand
#     7      31 2006        NA  Seychelles
#     8      37 2006 0.5967377   Mauritius
#     9      40 2006 0.5495849   Sri Lanka
#     10     41 2006 0.4517653  Mozambique
### NOTE: retains NA values for regions that were not assessed in a given year

### Save GCI data file
gci_file <- file.path(dir_git, scenario, 'intermediate/wef_gci_2006_2014.csv')
write_csv(gci_rgn, gci_file)

}

