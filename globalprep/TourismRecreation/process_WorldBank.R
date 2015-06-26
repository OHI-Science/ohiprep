# process_WorldBank.R  
# Do not run stand-alone - source from main data_prep.R for TourismRecreation.
#
# Reformat and add rgn_ids for World Bank statistics data.
# Provenance:
#   Jun2015 Casey O'Hara updated for 2015 assessment
#   Jun2014 BBest improved functions
#   Mar2014 JStewartLowndes created from clean_BWstats.R
#   May2013 JStewart: clean_BWstats.r. 
#   Data: 
#       * gdp: Gross Domestic Product (current \$USD)
#         + http://data.worldbank.org/indicator/NY.GDP.MKTP.CD
#       * uem: Unemployment, total (% of total labor force) 
#         + http://data.worldbank.org/indicator/SL.UEM.TOTL.ZS
#       * tlf: Total Labor force, total (# people)
#       	+ http://data.worldbank.org/indicator/SL.TLF.TOTL.IN
#       * ppppcgdp: GDP adjusted by per capita by PPP
#       	+ http://data.worldbank.org/indicator/NY.GDP.PCAP.PP.CD
#       * pop: country total population
#         + http://data.worldbank.org/indicator/SP.POP.TOTL
# 
#   add OHI region_ids with name_to_rgn_id.r          ** differs from data_prep.old
#   georegional gapfilling with gapfill_georegions.R  ** differs from data_prep.old
#
# TODO: manually fix any missing GDP with Wikipedia lookups

##############################################################################=
### setup ----
##############################################################################=
### Libraries and such are set up within data_prep.R


##############################################################################=
### read in and process files ----
##############################################################################=
dir_wb <- file.path(dir_git, '../WorldBank-statistics', scenario, 'raw')

wb_file_list <- list.files(path = dir_wb, pattern=glob2rx('*csv'), full.names = TRUE)

data_all <- data.frame()  # Initialize data_all data frame to be empty

for (wb_file in wb_file_list) {  # wb_file = wb_file_list[1]
  cat(sprintf('processing %s\n  %s\n', basename(wb_file), wb_file))
  
  wb_data <- read.csv(wb_file, skip=1, check.names = FALSE, stringsAsFactors = FALSE)
  head(wb_data) ### do not add the stupid X in front of the numeric column names
      
  ### gather wide-format data into year/value variables
  wb_data <- wb_data %>%
    select(country = `Country Name`, matches('\\d')) %>% 
      # get first column as country and all other year columns that are \\digits in regex speak
    gather(year, value, -country) %>%
    arrange(country, year) %>%    
    # remove NAs
    filter(!is.na(value))

  ### add layer column, overwriting if gdp.pcap.pp; convert year from factor to integer.
  wb_data <- wb_data %>%
    mutate(layer = ifelse(str_detect(basename(wb_file), 'gdp.pcap.pp'),
                          'gdppcppp',
                          str_split_fixed(basename(wb_file), fixed('.'), 3)[2]),
           year  = as.integer(as.character(year)))
  
  ### bind this data set to the full set of WorldBank data
  data_all <- bind_rows(data_all, wb_data)
}

##############################################################################=
### spot checks and a little cleanup----
##############################################################################=
filter(data_all, country=='Albania' & year==1990)  # ? what's the point of this one?

# remove Channel Islands and Isle of Man
data_all <- data_all %>%
  filter(!country %in% c('Channel Islands', 'Isle of Man'))
  
# Print out all the unique indicators
print('These are all the variables with year counts included in the cleaned file: ')
print(table(data_all$layer))
#       gdp gdppcppp      pop      tlf      uem 
#     10162     5172    13129     5229     4738 


##############################################################################=
### Separate out and save population and data files ----
##############################################################################=
### create separate population file and remove from data_all
### - this file gets used later in the name_to_rgn function, to collapse by
###   population for some data sets.

wb_pop   <- data_all %>% 
  filter(layer == 'pop') %>%
  select(country, year, w_popn=value)

data_all <- data_all %>% filter(layer != 'pop')

### Save population and data_all data sets.
pop_file  <- file.path(dir_int, 'wb_country_total_pop.csv')
cat(sprintf('Writing population data to: \n  %s\n', pop_file))
write_csv(wb_pop, pop_file)

data_file <- file.path(dir_int, 'wb_data_all.csv')
data_layers <- paste(unique(data_all$layer), collapse = ', ')
cat(sprintf('Writing %s data to: \n  %s\n', data_layers, data_file))
write_csv(data_all, data_file)


##############################################################################=
### Process data sets using name_to_rgn function ----
##############################################################################=

### name_to_rgn with collapse_fxn = sum_na for all but gdppcppp and uem ----
data_rgn1 <- name_to_rgn(data_all %>%
                           filter(!layer %in% c('gdppcppp', 'uem')), 
                         fld_name='country', flds_unique=c('country','year','layer'), 
                         fld_value='value', add_rgn_name=T, 
                         collapse_fxn = 'sum_na') 

### name_to_rgn with collapse_fxn = sum_weight_by_pop for gdppcppp and uem ----
pop_file <- file.path(dir_int, 'wb_country_total_pop.csv')
data_rgn2 <- name_to_rgn(data_all %>%
                           filter(layer %in% c('gdppcppp', 'uem')), 
                         fld_name='country', flds_unique=c('country','year','layer'), 
                         fld_value='value', add_rgn_name=T,
                         collapse_fxn='weighted.mean', collapse_csv = pop_file)

# rbind together from name_to_rgn calls ----
data_rgn <- bind_rows(data_rgn1, data_rgn2)

# remove Antarctica[213] and DISPUTED[255]
data_rgn <- data_rgn %>%
  filter(!rgn_id %in% c(213,255))

##############################################################################=
### Save out all layers to data folder ----
##############################################################################=
for (l_name in unique(data_rgn$layer)) {  # l_name = unique(data_rgn$layer)[1]
  l_units <- switch(l_name,
                    tlf = 'count',
                    gdp = 'usd',
                    gdppcppp = 'intl_dollar',
                    uem = 'percent')

  tmp_data <- data_rgn %>% 
    filter(layer == l_name)
  names(tmp_data)[names(tmp_data) == 'value'] <- l_units
  
  layer_file <- file.path(dir_int, sprintf('wb_rgn_%s.csv', l_name))
  cat(sprintf('Writing complete layer %s (%s) data to:\n  %s\n', l_name, l_units, layer_file))
  write_csv(tmp_data, layer_file)
}


##############################################################################=
### More spot checks - commented out but left for reference ----
##############################################################################=
# # spot check Qatar and China
# data_rgn2 %>% group_by(layer, year) %>%  
#   mutate(
#     rank = row_number(desc(value))) %>%
#   filter(year==2013 & rgn_name %in% c('China','Qatar','United States'))
# 
# data_rgn2 %>%  
#   filter(layer == 'uem', year>2008 & rgn_name %in% c('China','Qatar','United States'))
# 
# # spot check uem China 
# data_all %>%  
#   filter(layer == 'uem' & 
#            year > 2008 & country %in% c('China', 'Hong Kong SAR, China', 'Macao SAR, China'))
# data_all %>%  
#   filter(layer == 'uem' & 
#            country %in% c('Guam', 'Northern Mariana Islands'))
# 
# data_all %>%  
#   filter(layer == 'uem' & 
#            country %in% c('Taiwan'))



##############################################################################=
### georegional gapfilling with gapfill_georegions.r - not used in 2015 ----
##############################################################################=
### This section formerly contained some gapfilling.  Removed for clarity.
### Gapfilling scripts can be found in older versions of script, e.g.
### ohiprep/Global/WorldBank-Statistics_v2012.



##############################################################################=
### final GDPpcPPP rescaling - not used in TR? ----
##############################################################################=

## rescale GDPpcPPP by the max of each year
# 
# ppp <- read.csv(file.path(dir_d, 'data', 'rgn_wb_gdppcppp_2014a_ratio-gapfilled.csv')); head(ppp)
# 
# p <- ppp %>%
#   left_join(ppp %>%
#               group_by(year) %>%
#               summarize(max_intl_dollar = max(intl_dollar, na.rm=T)),
#             by='year') %>% 
#   mutate(value = intl_dollar/max_intl_dollar); head(p); summary(p)
# 
# ## save
# scenarios <- list('2012a'=2011,
#                  '2013a'=2012,
#                  '2014a'=2013)
# 
# for (scen in names(scenarios)){ # scen = names(scenarios)[1]
#   
#   yr <- scenarios[[scen]]
#   cat(sprintf('\nScenario %s using year == %d\n', scen, yr))
#   
#   p_yr <- p %>%
#     filter(year <= yr) %>% # remove any years greater than the scenario
#     select(rgn_id, year, value)
#   
#   csv <- sprintf('rgn_wb_gdppcppp_rescaled_%s.csv', scen)
#   write.csv(p_yr, file.path(dir_d, 'data', csv), row.names=F)
#   
# }


## translate total labor force to cntry_key; save for each scenario
# this makes obsolete the troublesome (because of NAs and unknown origin) file on Neptune: model/GL-NCEAS-LayersDisaggregated_v2013a/data/le_workforcesize_adj.csv 

# fname <- 'rgn_wb_tlf_2014a_ratio-gapfilled.csv'
# f <- read.csv(file.path(dir_d, 'data', fname)); head(f)
# cp <- read.csv(file.path(dir_neptune_data, 'model/GL-NCEAS-CoastalPopulation_v2013/data', 
#                                  'cntry_popsum2013_inland25mi_complete.csv'))
# 
# lf1 <- f %>%
#   select(rgn_id, year, count) %>%
#   left_join(cp,
#             by = 'rgn_id') 
# 
# lf <- lf1 %>%
#   left_join(lf1 %>%
#               group_by(rgn_id, year) %>%
#               summarize(rgn_popsum = sum(cntry_popsum2013_inland25mi)), 
#             by = c('rgn_id', 'year')) %>%
#   mutate(pop_ratio = cntry_popsum2013_inland25mi/rgn_popsum,
#          jobs = count * pop_ratio) %>% # debug: lf %>% filter(pop_ratio != 1) see below
#   select(cntry_key, year, jobs) %>%
#   arrange(cntry_key, year); head(lf); summary(lf)

# show which rgn_ids and cntry_keys are split with pop_ratio
# lf %>% filter(pop_ratio != 1)
# 13  GUM
# 13  MNP
# 116 PRI
# 116 VIR
# 137 ECU
# 137 Galapagos Islands
# 140 MTQ
# 140 GLP
# 163 Hawaii
# 163 USA
# 163 Alaska
# 171 Trindade
# 171 BRA
# 224 Easter Island
# 224 CHL

# lf %>% filter(rgn_id == 163, year == 2012)
# lf %>% filter(rgn_id == 171, year == 2012)


# save for each scenario 

# scenarios <- list('2012a'= max(lf$year, na.rm=T)-2,
#                  '2013a'= max(lf$year, na.rm=T)-1,
#                  '2014a'= max(lf$year, na.rm=T))
# 
# for (scen in names(scenarios)){ # scen = names(scenarios)[1]
#   
#   yr <- scenarios[[scen]]
#   cat(sprintf('\nScenario %s using year == %d\n', scen, yr))
#   
#   lf_yr <- lf %>%
#     filter(year <= yr) # remove any years greater than the scenario
#   stopifnot(anyDuplicated(lf_yr[,c('cntry_key', 'year')]) == 0)
#   
#   csv <- sprintf('cntry_wb_tlf_%s_ratio-gapfilled.csv', scen) # file_path_sans_ext(fname)
#   write.csv(lf_yr, file.path(dir_d, 'data', csv), row.names=F)
#   
# }
# 

# --- fin ---
