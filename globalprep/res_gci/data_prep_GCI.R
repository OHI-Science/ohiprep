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
### setup -----
##############################################################################=
library(ohicore) # devtools::install_github('ohi-science/ohicore') # may require uninstall and reinstall

setwd('~/github/ohiprep')
source('src/R/common.R')
library(readr)

goal     <- 'globalprep/WEF-Economics'
scenario <- 'v2015'
dir_anx  <- file.path(dir_neptune_data, 'git-annex', goal) 
dir_git  <- file.path('~/github/ohiprep', goal)
dir_data <- file.path(dir_git, scenario, 'data')
dir_int  <- file.path(dir_git, scenario, 'intermediate')


##############################################################################=
### WEF GCI formatting ----
##############################################################################=
# read in files ----
gci_raw <- read.csv(file.path(dir_anx, scenario, 'raw', 'GCI_Dataset_2006-07-2014-15.csv'), 
                    skip = 3, check.names = FALSE, stringsAsFactors = FALSE)
### NOTE: check.names = FALSE to avoid problems with accents - e.g. Cote d'Ivoire has 
###   an accent circonflex over the 'o' (probably other issues in there too)

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
         year  = as.integer(substr(edition, 6, 9)), ### pull off last four digits for data year
         country = str_replace(country, 'South Korea, Rep.', 'South Korea')) %>%
  select(-edition)

gci_rgn <- name_to_rgn(gci, fld_name='country', 
                       flds_unique=c('country', 'year'), fld_value='score', 
                       collapse_fxn = 'mean', add_rgn_name = T) %>%
  arrange(rgn_id, year)

stopifnot(max(gci_rgn$score, na.rm = T) <= 1)

head(gci_rgn %>% filter(year == 2013), 10)
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

## georegional gapfilling with gapfill_georegions.r ----
georegions <- read.csv('../ohi-global/eez2013/layers/rgn_georegions.csv', na.strings='') %>%
  spread(level, georgn_id)

georegion_labels <- read.csv('../ohi-global/eez2013/layers/rgn_georegion_labels.csv') %>%
  mutate(level = sprintf('%s_label', level)) %>%
  spread(level, label) %>%
  left_join(
    read.csv('../ohi-global/eez2013/layers/rgn_labels.csv') %>%
      select(rgn_id, v_label=label),
    by='rgn_id') %>%
  arrange(r0_label, r1_label, r2_label, v_label)


### loop over scenarios to save multiple layers

scenarios <- c('eez2012' = 2012, 'eez2013' = 2013, 'eez2014' = 2014, 'eez2015' = 2015)

for (i in 1:length(scenarios)) { # i <- 3
  sc_year <- scenarios[i]
  sc_name <- names(scenarios)[i]
  
  layersave = file.path(dir_data, sprintf('rgn_wef_gci_%d.csv',      sc_year))
  attrsave  = file.path(dir_data, sprintf('rgn_wef_gci_%d_attr.csv', sc_year))
  
  # library(devtools); load_all('../ohicore')
  # source('../ohicore/R/gapfill_georegions.R')
  d_g_a = gapfill_georegions(
    data = gci_rgn %>%
      filter(year == sc_year) %>%
      filter(!rgn_id %in% c(213,255)) %>%
      select(rgn_id, score),
    fld_id = 'rgn_id',
    georegions = georegions,
    georegion_labels = georegion_labels,
    r0_to_NA = TRUE, 
    attributes_csv = attrsave) # don't chain gapfill_georegions or will lose head(attr(d_g_a, 'gapfill_georegions')) ability

  ## last step: give North Korea the minimum value and save ----
  d_g = d_g_a %>%
    select(rgn_id, score) %>%
    arrange(rgn_id); head(d_g)
  
  # find minimum  
  s_min = min(d_g %>%
                select(score) %>%
                filter(!is.na(score)))
  
  # replace North Korea (rgn_id == 21) in gapfilled_data
  d_g$score[d_g$rgn_id == 21] = s_min 
  
  # save
  stopifnot(anyDuplicated(d_g[,c('rgn_id')]) == 0)
  write.csv(d_g, layersave, na = '', row.names=FALSE)

  ## also change attributes table ---- 
  d_attr = read.csv(attrsave) %>%
    filter(id != 21)
  
  d_nk = d_attr %>%
    filter(id == 21) %>%
    mutate(
      z_level = 'XH',
      
      r2_v = s_min, 
      r1_v = s_min, 
      r0_v = s_min, 
      z    = s_min, 
      
      r2         = NA,
      r1         = NA,
      r0         = NA,
      r2_n_notna = NA,
      r1_n_notna = NA,
      r0_n_notna = NA,
      z_ids      = NA,
      r2_n       = NA,
      r1_n       = NA,
      r0_n       = NA,
      z_n        = NA,
      z_n_pct    = NA,
      z_g_score  = NA); d_nk
  
  d_attr_fin = rbind(d_attr, d_nk) %>%
    arrange(r0_label, r1_label, r2_label, v_label) 
  write.csv(d_attr_fin, attrsave, na = '', row.names=F)
}


### Save GCI data file
gci_file <- file.path(dir_git, scenario, 'intermediate/wef_gci_2007_2015.csv')
write_csv(gci_rgn, gci_file)

