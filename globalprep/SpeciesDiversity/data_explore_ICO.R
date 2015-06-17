##############################################################################=
### ohiprep/globalprep/SpeciesDiversity/data_prep.R
###
### GOAL: Obtain species diversity data and iconics data for global
### Jun 1, 2015 - CCO.  Combining many different scripts into one data_prep.R
###   that calls functions and sources code within R/spp_fxn.R
##############################################################################=
library(readr)      # for read_csv()
#library(RCurl)
library(XML)


setwd('~/github/ohiprep')
source('src/R/common.R')

goal     <- 'globalprep/SpeciesDiversity'
scenario <- 'v2015'
dir_anx  <- file.path(dir_neptune_data, 'git-annex', goal) 
dir_git  <- file.path('~/github/ohiprep', goal)

source(file.path(dir_git, 'R/spp_fxn.R'))
source(file.path(dir_git, 'R/ico_fxn.R'))
# SPP-specific and ICO-specific functions


##############################################################################=
### ICO -----
##############################################################################=

##############################################################################=
### get master list of Iconic Species -----
##############################################################################=
# this creates ico_list_full.csv
get_ico_list2 <- function() {
  ico_list_file <- file.path(dir_anx, 'ico/ico_global_list.csv')
  cat(sprintf('Reading raw iconic species list master file from: \n  %s\n', ico_list_file))
  ico_list_raw <- read.csv(ico_list_file, stringsAsFactors = FALSE) %>%
    select(rgn_name   = Country, 
           comname    = Specie.Common.Name,  
           sciname    = Specie.Scientific.Name, 
           ico_flag   = Flagship.Species,
           ico_local  = Priority.Species_Regional.and.Local,
           ico_global = Priority.Species_Global,
           ico_rgn    = Nation.Specific.List,
           ico_cat_long   = Current.Red.List.Category..Status.,
           ico_trend  = Population.Increasing..Decreasing..Stable.or.Unknown...over.3.generations.
    )
  # clean up names
  ico_list_raw <- ico_list_raw %>%
    mutate(rgn_name = str_trim(rgn_name),
           sciname = str_trim(sciname),
           comname = str_trim(comname)) %>%
    filter(sciname != '')
  
  sum(!is.na(ico_list_raw$ico_global) | !is.na(ico_list_raw$ico_flag))
  # 1673 out of 2592 are globally iconic &/or flagship species
  sum(!is.na(ico_list_raw$ico_local)  &  is.na(ico_list_raw$ico_global) & is.na(ico_list_raw$ico_flag))
  # 63 are locally iconic but not on global or flagship lists; all are minke whales.  The rest of the locally important list is all humpback whales.
  sum((ico_list_raw$ico_rgn != '') & is.na(ico_list_raw$ico_local)  &  is.na(ico_list_raw$ico_global) & is.na(ico_list_raw$ico_flag))
  # 45 are on nation-specific list that aren't included elsewhere
  
  
  # convert global, flagship, and local iconic flags into single global iconic flag
  # note 'local' is just humpbacks and minkes, across 50-60 countries each. Just call it global.
  ico_list <- ico_list_raw %>%
    mutate(ico_gl = (!is.na(ico_global) | !is.na(ico_local) | !is.na(ico_flag))) %>%
    select(-ico_global, -ico_local, -ico_flag) 
  
  
  # convert long text IUCN categories into letter codes.
  cat_lookup  <- data.frame(ico_category = c("LC", "NT", "VU", "EN", "CR", "EX", "DD"), 
                            ico_cat_long = c('least concern', 'near threatened', 'vulnerable', 'endangered', 'critically endangered', 'extinct', 'data deficient'))
  ico_list <- ico_list %>%
    mutate(ico_cat_long = tolower(ico_cat_long)) %>%
    left_join(cat_lookup, by = 'ico_cat_long') %>%
    select(-ico_cat_long)
  
  if(!exists('spp_all')) 
    spp_all <- read.csv(file.path(dir_anx, scenario, 'intermediate/spp_all_cleaned.csv'), 
                        stringsAsFactors = FALSE)
  
  
  # join to spp_all and update category/trend info if available from IUCN spreadsheet
  ico_list <- ico_list %>%
    left_join(spp_all %>%
                select(iucn_sid, am_sid, sciname, popn_trend, iucn_category, spatial_source, parent_sid, subpop_sid), 
              by = 'sciname') 
# This is code to override cat and trend with IUCN.  We will save that for later.
#     %>%
#     mutate(popn_trend    = tolower(popn_trend),
#            iucn_category = ifelse(is.na(iucn_category), 
#                                   as.character(ico_category), 
#                                   as.character(iucn_category)),
#            trend         = ifelse(is.na(popn_trend) | popn_trend == 'unknown', 
#                                   as.character(ico_trend), 
#                                   as.character(popn_trend))) %>%
#     select(-ico_category, -ico_trend, -popn_trend)
  
  # convert regional iconic status into a rgn_id, and join to ico_list.
  # - consider only observations with ico_rgn indicator or without spatial data.
  # - for these, attach an "ico_rgn_id" variable to track which countries to 
  #   count for these species
  # - then from all lines, remove the "country" variable, losing the country
  #   specific info from the spreadsheet for all but locally iconic species and
  #   those species without IUCN or AM spatial data.  The spatial data will
  #   instead be used to fill out the countries for those.
  rgn_name_file <- '~/github/ohi-global/eez2013/layers/rgn_global.csv'
  rgn_names <- read_csv(rgn_name_file)
#   ico_list <- ico_list %>%
#     left_join(ico_list %>%
#                 filter(ico_rgn != '' | is.na(spatial_source)) %>%
#                 select(rgn_name, sciname) %>%
#                 left_join(rgn_names, by = c('rgn_name' = 'label')),
#               by = c('rgn_name', 'sciname')) %>% 
  ico_list <- ico_list %>%
    left_join(ico_list %>%
                filter(ico_rgn != '' | is.na(spatial_source)) %>%
                select(rgn_name, sciname) %>%
                left_join(rgn_names, by = c('rgn_name' = 'label')),
              by = c('rgn_name', 'sciname')) %>% 
    select(-ico_rgn, ico_rgn_id = rgn_id) %>% 
    left_join(rgn_names, by = c('rgn_name' = 'label')) %>%
#  select(-rgn_name, -ico_rgn, ico_rgn_id = rgn_id) %>% 
#    filter(iucn_category != 'DD') %>% let's leave in for now...
    unique
# technically could get rid of rgn_name and comname now if this is all correct...
# fix with patches here, rather than later.
# the rgn_id list is still flaky.
  write_csv(ico_list, file.path(dir_git, 'tmp/ico_list_full.csv'))
  return(ico_list)
}

ico_list <-   read.csv(file.path(dir_git, 'tmp/ico_list_full.csv'), stringsAsFactors = FALSE)

mismatch_rgn <- ico_list %>% 
  filter(is.na(rgn_id)) %>% select(rgn_name) %>% unique()
# 393 mismatches out of 3152 observations, across 41 countries.
# fix country names that don't match the OHI list.
rgn_iucn2ohi <- read.csv(file.path(dir_anx, 'rgns/rgns_iucn2ohi.csv'), stringsAsFactors = FALSE)
ico_list1 <- ico_list %>%
  rename(rgn_name = country) %>%
  left_join(rgn_iucn2ohi, 
            by = c('rgn_name' = 'iucn_rgn_name')) %>%
  mutate(rgn_id = ifelse(is.na(rgn_id), ohi_rgn_id, rgn_id)) %>%
  select(-ohi_rgn_id, -ohi_rgn_name)

mismatch_rgn <- ico_list1 %>% 
  filter(is.na(rgn_id)) %>% select(rgn_name) %>% unique()

ico_list1 <- ico_list1 %>%
  filter(!is.na(rgn_id))

# fix these country names -----
      # C<aa>te d'Ivoire
      # French Southern Territories (the)
      # Libyan Arab Jamahiriya
      # Netherlands Antilles
      # British Indian Ocean Territory (Chagos Archipelago)
      # Disputed Territory (Paracel Is., Spratly Is.)
      # French Polynesia (Tuamotu)
      # Micronesia, Federated States of
      # United States Minor Outlying Islands (Wake Is.)
      # Lao People's Democratic Republic
      # Province of China
      # Dominican Republic Ecuador
      # Ethiopia
      # Sao Tom_ and Principe
# -----
##############################################################################=
### Determine species lists by region based on IUCN and AM spatial data -----
##############################################################################=

get_ico_rgn_iucn2 <- function(ico_list, reload = FALSE) {
  ico_rgn_iucn_file <- file.path(dir_git, 'tmp/ico_rgn_iucn.csv')
  if(!file.exists(ico_rgn_iucn_file) | reload) {
    ico_list_iucn <- ico_list %>%
      filter(str_detect(spatial_source, 'iucn')) %>%
      filter(ico_category != 'EX') %>%
      select(-rgn_name, -rgn_id) %>%
      unique()
    cat(sprintf('ico_list_iucn has %d elements\n', nrow(ico_list_iucn)))
    
    ### Load IUCN species per cell tables : takes a while
    iucn_cells_spp <- get_iucn_cells_spp()
    
    iucn_ico_names <- unique(ico_list_iucn$sciname)
    if(!exists('rgn_cell_lookup'))  rgn_cell_lookup <- extract_cell_id_per_region(reload = FALSE)
    
    ### filter is faster than join.  Filter iucn_cells_spp by sciname; then join to LOICZID <-> rgn table
    ico_cells_iucn <- iucn_cells_spp %>%
      filter(sciname %in% iucn_ico_names) %>%
      left_join(rgn_cell_lookup %>%
                  select(rgn_id, rgn_name, loiczid),
                by = c('LOICZID' = 'loiczid'))
    ico_rgn_iucn <- ico_cells_iucn %>%
      group_by(rgn_id, sciname, id_no) %>%
      summarize(ncells = n()) %>%
      inner_join(ico_list_iucn, by = 'sciname')
    ico_rgn_iucn <- ico_rgn_iucn %>%
      filter(ico_gl == TRUE | rgn_id == ico_rgn_id) %>%
      filter(!is.na(rgn_id)) %>%
      select(-id_no, -ncells, -ico_gl, -ico_rgn_id) %>%
      unique()
    
    cat(sprintf('Writing regional presence of iconic species from IUCN spatial data. \n  %s\n', ico_rgn_iucn_file))
    write_csv(ico_rgn_iucn, ico_rgn_iucn_file)
    
  } else {
    cat(sprintf('Reading regional presence of iconic species from IUCN spatial data from:\n  %s\n', ico_rgn_iucn_file))
    ico_rgn_iucn <- read_csv(ico_rgn_iucn_file)
  }
  
  return(ico_rgn_iucn)
}

get_ico_rgn_am2   <- function(ico_list, reload = FALSE) {
  ico_rgn_file <- file.path(dir_git, 'tmp/ico_rgn_am.csv')
  if(!file.exists(ico_rgn_file) | reload) {
    ico_list_am <- ico_list1 %>%
      filter(str_detect(spatial_source, 'am')) %>%
      filter(ico_category != 'EX') %>%
      mutate(am_sid = as.character(am_sid)) %>%
      select(-rgn_name, -rgn_id) %>%
      unique()
    cat(sprintf('ico_list_am has %d elements\n', nrow(ico_list_sp)))
    
    cells_spp <- get_am_cells_spp() %>%
      select(-proportionArea, -cell_area)
    
    ico_am_sid <- unique(ico_list_sp$am_sid)
    
    
    ### filter is faster than join.  Filter iucn_cells_spp by sciname; then join to LOICZID <-> rgn table
    ico_cells <- cells_spp %>%
      filter(am_sid %in% ico_am_sid)
    ico_rgn <- ico_cells %>%
      group_by(rgn_id, am_sid) %>%
      summarize(ncells = n()) %>%
      inner_join(ico_list_am, by = 'am_sid')
    ico_rgn <- ico_rgn %>%
      filter(ico_gl == TRUE | rgn_id == ico_rgn_id) %>%
      filter(!is.na(rgn_id)) %>% # ???
      select(-ncells, -ico_gl, -ico_rgn_id) %>%
      unique()
    
    cat(sprintf('Writing regional presence of iconic species from AM spatial data. \n  %s\n', ico_rgn_file))
    write_csv(ico_rgn, ico_rgn_file)
    
  } else {
    cat(sprintf('Reading regional presence of iconic species from AM spatial data from:\n  %s\n', ico_rgn_file))
    ico_rgn <- read_csv(ico_rgn_file)
  }
  return(ico_rgn)
}

ico_rgn_iucn <- get_ico_rgn_iucn2(ico_list1, reload = FALSE) 
### rgn_id | sciname | comname | iucn_sid | am_sid | iucn_category |
### spatial_source | parent_sid | subpop_sid | trend

ico_rgn_am <- get_ico_rgn_am2(ico_list, reload = FALSE)
### rgn_id | am_sid | comname | sciname | iucn_sid | trend |
### iucn_category | spatial_source | parent_sid | subpop_sid |

### Fill in species lists by region based on original spreadsheet
ico_rgn_other <- ico_list %>% 
  filter(is.na(spatial_source)) %>%
  select(rgn_id = ico_rgn_id, sciname, comname, iucn_category, trend)
### rgn_id | sciname | comname | iucn_category | trend

##############################################################################=
### Combine ICO lists from all spatial sources, add parent/subpop regions -----
##############################################################################=
ico_rgn_all <- bind_rows(ico_rgn_iucn, ico_rgn_am, ico_rgn_other)

# ico_list_subpops <- ico_rgn_all %>% 
#   filter(!is.na(parent_sid) | !is.na(subpop_sid)) %>%
#   select(sciname, iucn_sid) %>%
#   unique()
# 
# ico_subpop_rgn_ids <- get_countries_all(ico_list_subpops, reload = TRUE)
# 
# ico_rgn_all <- ico_rgn_all %>%
#   left_join(ico_subpop_rgn_ids %>%
#               select(sid, rgn_id) %>%
#               mutate(present = TRUE),
#             by = c('iucn_sid' = 'sid', 'rgn_id'))
# 
# ico_rgn_all <- ico_rgn_all %>%
#   filter(!((str_detect(spatial_source, 'parent') | str_detect(spatial_source, 'subpop')) & is.na(present))) %>%
#   select(-present, -parent_sid, -subpop_sid) %>%
#   unique()

ico_rgn_all <- ico_rgn_all %>% filter(!str_detect(spatial_source, 'subpop'))

# write_csv(ico_rgn_all, file.path(dir_anx, scenario, 'intermediate/ico_rgn_all.csv'))

##############################################################################=
### Summarize regional iconic species status -----
##############################################################################=
# ico_rgn_all <- read.csv(file.path(dir_anx, scenario, 'intermediate/ico_rgn_all.csv'), stringsAsFactors = FALSE)
ico_rgn_sum <- process_ico_rgn(ico_rgn_all)
### rgn_id | mean_cat | mean_trend | status

ico_status <- ico_rgn_sum %>%
  select(rgn_id, score = status)
ico_trend <- ico_rgn_sum %>%
  select(rgn_id, score = mean_trend)
write_csv(ico_status, file.path(dir_git, scenario, 'data/ico_status_no_subpops.csv'))
write_csv(ico_trend,  file.path(dir_git, scenario, 'data/ico_trend_no_subpops.csv'))
# write_csv(ico_status, file.path(dir_git, scenario, 'data/ico_status.csv'))
# write_csv(ico_trend,  file.path(dir_git, scenario, 'data/ico_trend.csv'))


##############################################################################=
### Some check functions -----
##############################################################################=
x <- ico_rgn_source_compare()
# For each region, checks whether the presence of a particular species is
# flagged due to spatially-explicit data (AM or IUCN range maps), the original
# spreadsheet, or both.
# NOTE: doesn't account for parent/subpop designations.




### IUCN species with parent/subpopulations:
# for parents and subpops, get country lists from iucn_details - see BB's script
#   function: get countries
#     str_split, turn into vector
#   for (list of parents/subpops by iucn_sid) get countries for each species ID
#     don't bind to the main table; just leave as lookup of iucn_sid:rgn_id
# convert the country lists into region ID numbers; create a lookup table for
#   parent_sid and subpop_sid vs rgn_id
# create a 'present' flag for the iucn list: each line is rgn_id matched to sciname (and thus all the other fields).
#   for parent/subpops, each will have a distinct iucn_sid (even for AM)
#   for each line, if rgn_id %in% country vector for that iucn_sid, flag present = TRUE
#   * captures both parent and subpop presence...?
# then: for the whole list, filter for (spatial_source == 'iucn' | present == TRUE)
# then: for this list, filter for global == TRUE or region specific == rgn_id

# 25 duplicated rows - species duplicated within a region.  Species: 
# Different IUCN species IDs, but no IUCN ID available for spatial information, 
# so based upon an undifferentiated IUCN shapefile.  Look up each and assign to regions by hand.
# Check in the shapefile itself - MAMMMARINE.shp or whatever.
#   Balaena mysticetus-----
#     - PARENT: http://www.iucnredlist.org/details/2467/0, LC, unknown, Canada; Greenland; United States
#         am_sid iucn_sid            sciname am_category iucn_category popn_trend
#   1 ITS-180533     2468 Balaena mysticetus          LC            NT    Increasing 
#     - http://www.iucnredlist.org/details/2468/0
#     - Bering-Chukchi-Beaufort Seas, Hudson Bay-Foxe Basin and Baffin Bay-Davis Strait
#     - Native: Canada; Russian Federation; United States
#   2 ITS-180533     2469 Balaena mysticetus          LC            EN    Unknown
#     - http://www.iucnredlist.org/details/2469/0
#     - Sea of Okhotsk from Shantarskiye Zaliv east to Zaliv Shelikova, Gizhiginskaya Guba and 
#       Penzhinskaya Guba (Moore and Reeves 1993, Rice 1998). 
#       Native:  Russian Federation
#   3 ITS-180533     2472 Balaena mysticetus          LC            CR    Unknown 
#     - http://www.iucnredlist.org/details/2472/0
#     - Svalbard-Barents Sea (Spitsbergen) subpopulation, This subpopulation ranges from the east 
#       coast of Greenland (Denmark) across the Greenland Sea, the Barents Sea, and the Kara Sea to 
#       Severnaya Zemlya (Russian Federation), and south at least occasionally to northern Iceland 
#       and the coast of Finnmark (Norway) and Jan Mayen (Norway) (Rice 1998).
#     - Native: Greenland; Svalbard and Jan Mayen (Norway)

#   Tursiops truncatus -----
#     - PARENT: http://www.iucnredlist.org/details/22563/0, LC, unknown; worldwide range
#   4 ITS-180426   194300 Tursiops truncatus          LC            CR Decreasing 
#     - Fiordland subpopulation - http://www.iucnredlist.org/details/194300/0
#     - Native: New Zealand 
#   5 ITS-180426 16369383 Tursiops truncatus          LC            VU Decreasing 
#     - Med subpop - http://www.iucnredlist.org/details/16369383/0
#     - Native: Albania; Algeria; Bosnia and Herzegovina; Croatia; Cyprus; France; Gibraltar; 
#       Greece; Israel; Italy; Lebanon; Libya; Malta; Monaco; Montenegro; Morocco; Serbia (Serbia); 
#       Slovenia; Spain; Syrian Arab Republic; Tunisia; Turkey
#   # another, not on our list: Black Sea, ssp ponticus, EN/unknown - http://www.iucnredlist.org/details/133714/0
#
# this might be a case for not removing the parent populations.  Bottlenose dolphins should
# generally be LC.  Check the IUCN shapefiles and see what other info is available.


### Aquamaps ICO species with parents/subpopulations:
# Different IUCN species IDs, but no IUCN spatial information, so based upon
# an undifferentiated Aquamaps map.  Look up each and assign to regions by hand.
# Check on IUCN site by iucn_sid, might have country ranges for each subpop.
# * Dermochelys coriacea: LC, CR -----
#   - PARENT: http://www.iucnredlist.org/details/6494/0, VU, decreasing; large range
#     am_sid  iucn_sid              sciname am_category iucn_category popn_trend
#   Rep-2331  46967807 Dermochelys coriacea          CR            CR Decreasing - East Pacific Ocean subpopulation
#   - http://www.iucnredlist.org/details/46967807/0, CR, dec
#   - Native: Chile; Colombia; Costa Rica; Ecuador; El Salvador; France (Clipperton I.); Guatemala; 
#     Honduras; Mexico; Nicaragua; Panama; Peru; United States (Hawaiian Is.)
#   Rep-2331  46967817 Dermochelys coriacea          CR            CR Decreasing - West Pacific Ocean subpopulation
#   - http://www.iucnredlist.org/details/46967817/0
#   - large range
#   Rep-2331  46967827 Dermochelys coriacea          CR            LC Increasing - Northwest Atlantic Ocean subpopulation
#   - http://www.iucnredlist.org/details/46967827/0, LC, inc, large range
#   Rep-2331  46967838 Dermochelys coriacea          CR            CR Increasing - Southwest Atlantic Ocean subpopulation
#   - http://www.iucnredlist.org/details/46967838/0, CR, inc
#   - Angola (Angola); Argentina; Benin; Brazil; Cameroon; Congo; Congo, The Democratic 
#     Republic of the; Côte d'Ivoire; Equatorial Guinea; Gabon; Gambia; Guinea; Guinea-Bissau; 
#     Liberia; Namibia; Nigeria; Saint Helena, Ascension and Tristan da Cunha; Sao Tomé and Principe; 
#     Senegal; Sierra Leone; South Africa; Togo; Uruguay
#   Rep-2331  46967863 Dermochelys coriacea          CR            CR Decreasing - Southwest Indian Ocean subpopulation
#   - Angola (Angola); Comoros; French Southern Territories (Mozambique Channel Is.); Kenya; Madagascar; 
#     Mauritius; Mayotte; Mozambique; Namibia; Seychelles; South Africa; Tanzania, United Republic of
#   NOTE: couple of data-deficient ones in there too.  See PARENT url.

# * Lamna nasus:          CR, EN -----
#   PARENT: http://www.iucnredlist.org/details/11200/0, VU, dec
#   Fis-22768    39343          Lamna nasus          VU            CR Decreasing - Northeast Atlantic subpopulation
#   - http://www.iucnredlist.org/details/39343/0
#   - Denmark; France; Gibraltar; Iceland; Ireland; Norway; Portugal (Azores, Madeira); 
#     Russian Federation; Spain; Sweden; United Kingdom
#   Fis-22768    39344          Lamna nasus          VU            EN Decreasing - Northwest Atlantic subpopulation
#   - Bermuda; Canada (Newfoundland I, Nova Scotia); Greenland; United States (Maine, Massachusetts, New Jersey, New York, Rhode Island)
#   Fis-22768    61420          Lamna nasus          VU            CR Decreasing - Mediterranean subpopulation
#   - http://www.iucnredlist.org/details/61420/0
#   - Albania; Algeria; Bosnia and Herzegovina; Croatia; Cyprus; Egypt; France; Greece; Israel; Italy; 
#     Lebanon; Libya; Malta; Monaco; Montenegro; Morocco; Slovenia; Spain; Syrian Arab Republic; Tunisia; Turkey

# * Isurus oxyrinchus:    VU, NT -----
# - PARENT: http://www.iucnredlist.org/details/39341/0, VU, dec
#   Fis-58485   161749    Isurus oxyrinchus          VU            VU Decreasing - Atlantic subpopulation
#   - http://www.iucnredlist.org/details/161749/0
#   - wide range!
#   Fis-58485   161750    Isurus oxyrinchus          VU            VU Decreasing - Indo-west Pacific subpopulation
#   - wide range
#   Fis-58485   161751    Isurus oxyrinchus          VU            NT Decreasing - Eastern North Pacific subpopulation
#   - Chile (Antofagasta, Coquimbo, Santiago, Valparaíso); Costa Rica (Costa Rica (mainland)); 
#     Ecuador (Ecuador (mainland)); Guatemala; Honduras (Honduras (mainland)); Mexico; 
#     Nicaragua (Nicaragua (mainland)); Peru; United States (California, Washington)
# -----


