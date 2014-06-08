# data_prep.R

# Prepare FAO commodities data for Natural Products goal. 
# By JSLowndes/bbest Jun2014; File was originally clean_FAOcommodities.r:(by JStewart Apr2013)

#   read in quant/value files
#   remove Totals, Yugoslavia rows
#   translate FAO data codes (F, ..., -, 0 0)
#   carry previous year's value forward if value for max(year) is NA
#   merge with commodities and collapse to categories
#   add rgn_id using new cbind_rgn function (@bbest updated add_rgn_id())
#   save single files for each commodity 

# setup ----

# load libraries (dplyr last)
library(reshape2)
library(stringr)
library(plyr)
library(ohicore) # devtools::install_github('ohi-science/ohicore') # may require uninstall and reinstall
library(dplyr)   # NOTE: I think dplyr needs to be loaded AFTER plyr to override functions, which prob was reason behind wierdness with group_by() and summarize() giving 1 value.

# get paths
# NOTE: The default path should already be your ohiprep root directory for the rest to work.
#       Otherwise, presume that scripts are always working from your default ohiprep folder
source('src/R/common.R') # set dir_neptune_data
dir_d = 'Global/FAO-Commodities_v2011' 
# NOTE: Set output paths to here, but do not use setwd().
#       This way, any scripts and code in ohiprep will still work, b/c always in root ohiprep dir.

# get functions
source('src/R/ohi_clean_fxns.R') # cbind_rgn

# lookup for converting commodities to categories
com2cat = read.csv(file.path(dir_d, 'commodities2categories.csv'), na.strings='')

# read in and process files ----
for (f in list.files(file.path(dir_d, 'raw'), pattern=glob2rx('*.csv'), full.names=T)){ # f=list.files(file.path(dir_d, 'raw'), pattern=glob2rx('*.csv'), full.names=T)[1]
  
  # data read in
  d = read.csv(f, check.names=F,strip.white=TRUE)
  
  # melt and clean up
  m = d %.%    
    rename(c(
      'Country (Country)'       = 'country',
      'Commodity (Commodity)'   = 'commodity',
      'Trade flow (Trade flow)' = 'trade')) %.%
    melt(id=c('country','commodity','trade'), variable='year') %.%
    filter(!country %in% c('Totals', 'Yugoslavia SFR')) %.%
    mutate(  
      value = str_replace(value, fixed( ' F'),    ''), # FAO denotes with F when they have estimated the value using best available data
      value = str_replace(value, fixed('0 0'), '0.1'), # FAO denotes something as '0 0' when it is > 0 but < 1/2 of a unit. 
      value = str_replace(value, fixed(  '-'),   '0'), # FAO's 0
      value = str_replace(value, fixed('...'),    NA), # FAO's NA
      value = as.numeric(value),
      year  = as.numeric(as.character(year))) %.%       # search in R_inferno.pdf for "shame on you." hah!
    filter(trade == 'Export') %.%
    select(-trade) %.%
    arrange(country, commodity, year)
  
  # check for commodities in data not found in lookup, per category by keyword
  commodities = sort(as.character(unique(m$commodity)))
  keywords = c(
    'sponges'     = 'sponge', 
    'fish_oil'    = 'oil', 
    'seaweeds'    = 'seaweed', 
    'ornamentals' = 'ornamental', 
    'corals'      = 'coral', 
    'shells'      = 'shell')
  for (i in 1:length(keywords)){ # i=1
    cat = names(keywords)[i]
    keyword = keywords[i]
    d_missing_l = setdiff(
      commodities[str_detect(commodities, ignore.case(keyword))], 
      subset(com2cat, category==cat, commodity, drop=T))
    if (length(d_missing_l)>0){
      cat(sprintf("\nMISSING in the lookup the following commodites in category='%s' having keyword='%s' in data file %s:\n    %s\n", 
                  cat, keyword, basename(f), paste(d_missing_l, collapse='\n    ')))
    }
  }

  # check for commodities in lookup not found in data
  l_missing_d = anti_join(com2cat, m, by='commodity')
  if (length(l_missing_d)>0){
    cat(sprintf('\nMISSING: These commodities in the lookup are not found in the data %s:\n    ', basename(f)))
    print(l_missing_d)
  }
  
  # antilles: clean up Netherlands Antilles by breaking into its 6 regions
  m_ant = m %.%
    filter(country == 'Netherlands Antilles') %.%
    mutate(
      value            = value/6,
      'Aruba'          = value,
      'Bonaire'        = value,
      'Curacao'        = value,
      'Saba'           = value,
      'Sint Maarten'   = value,
      'Sint Eustatius' = value) %.%
    select(-value, -country) %.%
    melt(id=c('commodity','year'), variable.name='country')
  m_a = m %.%
    filter(country != 'Netherlands Antilles') %.%
    rbind_list(m_ant)    
  
  # gapfill: Carry previous year's value forward if value for max(year) is NA.
  #   This gives wiggle room for regions still in production but not able to report by the FAO deadline.
  m_a_g = m_a %.%
    group_by(country, commodity) %.%
    mutate(
      year_max   = max(year),
      year_prev  = lag(year, order_by=year),
      value_prev = lag(value, order_by=year),
      value      = ifelse(is.na(value) & year==year_max & year_prev==year-1, value_prev, value))  
  
  # collapse: join with commodities and summarize to category
  m_c = m_a_g %.%
    inner_join(com2cat, by='commodity') %.%
    group_by(country, category, year) %.%
    summarize(value = sum(value, na.rm=T))

  # units: rename value field to units based on filename
  units = c('tons','usd')[str_detect(f, c('quant','value'))] # using American English, lowercase
  m_c_u = rename(m_c, setNames(units, 'value'))
  
  # rgn_id: add  # source('src/R/ohi_clean_fxns.R') # cbind_rgn
  m_c_u_r = cbind_rgn(m_c_u, fld_name='country') # @jules32: new function to replace add_rgn_id in src/R/ohi_clean_fxns.R!
  
  # check for duplicates
  stopifnot( sum(duplicated(m_c_u_r[,c('country', 'category', 'year')])) == 0 )
  
  # output
  f_out = sprintf('%s/data/%s_%s.csv', dir_d, basename(dir_d), units)
  write.csv(m_c_u_r, f_out)
}