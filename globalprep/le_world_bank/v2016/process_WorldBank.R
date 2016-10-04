# process_WorldBank.R  
# Do not run stand-alone - source from main data_prep.R for TourismRecreation.
#
# Reformat and add rgn_ids for World Bank statistics data.
# Provenance:
#   Jun2015 Casey O'Hara updated for 2015 assessment
#   Jun2014 BBest improved functions
#   Mar2014 JStewartLowndes created from clean_BWstats.R
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
### read in and process files ----
##############################################################################=

data_all <- data.frame()  # Initialize data_all data frame to be empty

for (wb_file in wb_file_list) {  # wb_file = wb_file_list[1]
  cat(sprintf('processing %s\n  %s\n', basename(wb_file), wb_file))
  
  wb_data <- read.csv(file.path(wb_file), skip=4, check.names = FALSE, stringsAsFactors = FALSE)
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
    mutate(layer = ifelse(str_detect(basename(wb_file), 'GDP.PCAP.PP'),
                          'GDPPCPPP',
                          str_split_fixed(basename(wb_file), fixed('.'), 3)[2]),
           year  = as.integer(as.character(year)))
  
  ### bind this data set to the full set of WorldBank data
  data_all <- bind_rows(data_all, wb_data)
}

##############################################################################=
### spot checks and a little cleanup----
##############################################################################=
filter(data_all, country=='Albania' & year==1991)  # see if all the expected data are t

# Channel Islands: Jersey and Guernsey
jg <- data.frame(country="Channel Islands", country2=c("Jersey", "Guernsey")) %>%
  mutate(country2 = as.character(country2)) %>%
  mutate(country = as.character(country))
  
data_all <- data_all %>%
  left_join(jg, by="country") %>%
  mutate(country2 = ifelse(is.na(country2), country, country2)) %>%
  select(country = country2, year, layer, value) %>%
  filter(country != "Channel Islands")

## tried to change this in synonym list...but keeps causing issues:
data_all <- data_all %>%
  mutate(country = ifelse(country == "Korea, Dem. People’s Rep.", "North Korea", country))

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
  filter(layer == 'POP') %>%
  select(country, year, w_popn=value)


##############################################################################=
### Process data sets using name_to_rgn function ----
##############################################################################=

### name_to_rgn with collapse_fxn = sum_na for all but gdppcppp and uem ----

data_rgn <- name_2_rgn(data_all, 
                       fld_name='country', 
                       flds_unique=c('year', 'layer'))


# remove Antarctica[213] and DISPUTED[255]
data_rgn <- data_rgn %>%
  filter(!rgn_id %in% c(213,255))

## summarize duplicates:
data_rgn <- data_rgn %>%
  left_join(wb_pop, by=c("country", "year")) 

data_rgn_wm <- data_rgn %>%
  filter(layer %in% c('GDPPCPPP', 'UEM')) %>%
  group_by(rgn_id, rgn_name, year, layer) %>%
  summarize(value = weighted.mean(value, w_popn, na.rm=TRUE))
data_rgn_sum <- data_rgn %>%
  filter(layer %in% c('TLF', 'POP')) %>%
  group_by(rgn_id, rgn_name, year, layer) %>%
  summarize(value = sum(value, na.rm=TRUE))

data_rgn <- rbind(data_rgn_wm, data_rgn_sum)

##############################################################################=
### Save out all layers to data folder ----
##############################################################################=
for (l_name in unique(data_rgn$layer)) {  # l_name = unique(data_rgn$layer)[1]
  l_units <- switch(l_name,
                    TLF = 'count',
                    POP = 'count',
                    GDPPCPPP = 'intl_dollar',
                    UEM = 'percent')

  tmp_data <- data_rgn %>% 
    filter(layer == l_name)
  names(tmp_data)[names(tmp_data) == 'value'] <- l_units
  
  layer_file <- file.path(dir_int, sprintf('wb_rgn_%s.csv', l_name))
  cat(sprintf('Writing complete layer %s (%s) data to:\n  %s\n', l_name, l_units, layer_file))
  write_csv(tmp_data, layer_file)
}


