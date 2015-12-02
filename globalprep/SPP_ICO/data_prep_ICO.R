##############################################################################=
### ohiprep/globalprep/SPP_ICO/data_prep_ICO.R
###
### GOAL: Obtain iconics data for global
### Jun 19, 2015 - CCO.  Updating ICO to automatically gather country info
###   from scraped IUCN red list pages, and to incorporate parent/subpop 
###   extinction risk categories.
##############################################################################=

library(readr)      # for read_csv()
library(XML)

setwd('~/github/ohiprep')
source('src/R/common.R')

goal     <- 'globalprep/SPP_ICO'
scenario <- 'v2015'
dir_anx  <- file.path(dir_neptune_data, 'git-annex', goal) 
dir_git  <- file.path('~/github/ohiprep', goal)
dir_data_am    <- file.path(dir_neptune_data, 'git-annex/globalprep/_raw_data', 'aquamaps/v2014') 
dir_data_iucn  <- file.path(dir_neptune_data, 'git-annex/globalprep/_raw_data', 'iucn_spp') 

source(file.path(dir_git, 'R/spp_fxn.R'))
source(file.path(dir_git, 'R/ico_fxn.R'))
# SPP-specific and ICO-specific functions

##############################################################################=
### get master list of Iconic Species -----
##############################################################################=

ico_list <- get_ico_list()

### comname | sciname | ico_gl | category | iucn_sid | trend | parent_sid | subpop_sid | ico_rgn_id
### * ico_rgn_id: rgn_id in which species is iconic by regional/national lists, separately from other lists.


##############################################################################=
### Find ICO regions for all species on ICO list -----
##############################################################################=
### NOTE: since this is pulling country lists from IUCN site, all ICO species
### must have an IUCN SID.  Not a problem from 2011 list so far.

ico_spp_list <- ico_list %>%
  select(sciname, iucn_sid) %>%
  unique()

ico_rgn_list <- get_ico_details_all(ico_spp_list, reload = TRUE)
### | sid | rgn_name | rgn_type | rgn_id

ico_rgn_all <- ico_list %>%
  left_join(ico_rgn_list, 
            by = c('iucn_sid' = 'sid'))

### Deal with the regionally and possibly extinct - mutate the category here.
### Regionally extinct are essentially counted as an extinct subpop.
### Possibly extinct are considered functionally extinct in this, so same as regionally extinct.
ico_rgn_all <- ico_rgn_all %>%
  mutate(category = ifelse(rgn_type == 'possibly extinct',   'EX', category),
         category = ifelse(rgn_type == 'regionally extinct', 'EX', category),
         trend    = ifelse(rgn_type == 'possibly extinct',    NA,  trend),
         trend    = ifelse(rgn_type == 'regionally extinct',  NA,  trend))

ico_rgn_all <- ico_rgn_all %>%
  filter((ico_gl == TRUE & is.na(ico_rgn_id) | ico_rgn_id == rgn_id))

write_csv(ico_rgn_all, file.path(dir_anx, scenario, 'intermediate/ico_rgn_all.csv'))

##############################################################################=
### Report and summarize regional iconic species status -----
##############################################################################=
ico_rgn_all <- read.csv(file.path(dir_anx, scenario, 'intermediate/ico_rgn_all.csv'), stringsAsFactors = FALSE)

# Report out for toolbox format (rgn_id | sciname | category or popn_trend for each species within a region).
# Note: in toolbox, group_by(rgn_id, sciname) and then summarize(category = mean(category)) to
#   average any parent/subpop species listings before aggregating to overall average per region.
ico_status <- ico_rgn_all %>%
  select(rgn_id, sciname, category) %>%
  arrange(rgn_id, sciname)
ico_trend <- ico_rgn_all %>%
  select(rgn_id, sciname, popn_trend = trend) %>%
  arrange(rgn_id, sciname)
write_csv(ico_status, file.path(dir_git, scenario, 'data/ico_status.csv'))
write_csv(ico_trend,  file.path(dir_git, scenario, 'data/ico_trend.csv'))

# Report out for finalized status and trend values per region.
ico_rgn_sum <- process_ico_rgn(ico_rgn_all)
### rgn_id | mean_cat | mean_trend | status

ico_status_sum <- ico_rgn_sum %>%
  select(rgn_id, score = mean_cat) %>%
  arrange(rgn_id)
ico_trend_sum <- ico_rgn_sum %>%
  select(rgn_id, score = mean_trend) %>%
  arrange(rgn_id)
write_csv(ico_status_sum, file.path(dir_git, scenario, 'data/ico_status_sum.csv'))
write_csv(ico_trend_sum,  file.path(dir_git, scenario, 'data/ico_trend_sum.csv'))

