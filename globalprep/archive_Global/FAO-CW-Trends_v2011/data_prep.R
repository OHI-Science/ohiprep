# data_prep.R

# Prepare FAO fertilizer/pesticides data for CW trends. 
# By JSLowndes Apr2014; File was originally clean_FAOtrends.r:(by JStewart Jul2013)

# for trends in pesticides and fertilizers. Note that for 2014a, only pesticides are updated (fertilizer 2011 data are all 0's)
# Data are in a new format since 2013a. Available from http://faostat3.fao.org/faostat-gateway/go/to/download/R/*/E

#   read in individual files
#   remove Totals row
#   remove/translate FAO data codes (F, ..., -, 0 0)
#   add identifier column
#   concatenate data from each file into a single file
#   run name_to_rgn.r (function by J. Stewart, B. Best)
#   save single file


# setup ----

# load libraries
library(gdata)
library(biglm)
# library(ohicore) # devtools::install_github('ohi-science/ohicore') # may require uninstall and reinstall
library(devtools); load_all('../ohicore')
library(ohicore)
library(dplyr)

# get paths.  NOTE: Default path should be ohiprep root directory.
dir_neptune_data = c('Windows' = '//neptune.nceas.ucsb.edu/data_edit',
                     'Darwin'  = '/Volumes/data_edit',
                     'Linux'   = '/var/data/ohi')[[ Sys.info()[['sysname']] ]]
dir_d = file.path('../ohiprep/Global/FAO-CW-Trends_v2011')


## identify how many years subtract from max year for each scenario 
scenario = c('2012' = 2, 
             '2013' = 1, 
             '2014' = 0)

## read in and process files ----

for (k in list.files(path = file.path(dir_d, 'raw'), pattern=glob2rx('*csv'), full.names=T)) { 
  # k = "../ohiprep/Global/FAO-CW-Trends_v2011/raw/FAO_fertilizers_thru2011.csv"
  # k = "../ohiprep/Global/FAO-CW-Trends_v2011/raw/FAO_pesticides_thru2011.csv" 
  
  d_tmp = read.csv(k, header=F); head(d_tmp)
  v = unlist(strsplit(as.character(d_tmp[1,1]), ' '))[1] 
  
  # clean up data
  d = d_tmp %>%
    select(country = V2, 
           category = V3, 
           year = V5, 
           tonnes = V6) %>%
    group_by(country, year) %>%
    summarise(tonnes = sum(tonnes)); head(d) # d[duplicated(d[, c('country', 'year')]),] 
  
  
  ## add rgn_ids with name_to_rgn ----
  dn = ohicore::name_to_rgn(d, fld_name='country', flds_unique=c('country', 'year'), fld_value='tonnes', add_rgn_name=T) 
  
  ##   clean up data: described in Global SOM 2013: section 5.19 ----
  # Fertilizers have weird 0's and Pesticides don't 
  filter(dn, tonnes == 0)
  dn = dn %>%         
    filter(tonnes != 0) 
  
  # for each scenario
  for (i in 1:length(names(scenario))) { # i=1
    
    yr_max = max(dn$year, na.rm=T) - as.numeric(as.character(factor(scenario[i])))
    yr_min = yr_max - 4 # yr_min:max(f$year) is 5 years
    
    message(sprintf('\n  for %s %sa, calculate trend using yr_min == %d and yr_max == %d', v, names(scenario)[i], yr_min, yr_max))
    
    dn2 = dn %>%
      filter(year %in% yr_min:yr_max)
    
    rgns_to_remove = dn2 %>%
      group_by(rgn_id) %>%
      summarize(count = n()) %>%
      filter(count < 2)
    
    dn3 = dn2 %>%
      filter(!rgn_id %in% rgns_to_remove$rgn_id)
    
    ## calculate trend and gapfill for both fertilizers and pesticides:: KLo style. ----
    # See readme.md and Global SOM 2013 section 5.19. Approach by Katie Longo, September 2013:
    # github/ohiprep/Global/FAO-CW-Trends_v2011/raw/Fertilizer_Pesticide_trend_KLongo2013.R
    # trend years: 2012a (2005:2009) and 2013a (2006:2010). Note:: error in KLo
    # 2013 approach: trend was created through multiplying slope by 4 instead of 5
    
    #   1) calculate fert and pest trend ----
    d_mdl = dn3 %>%
      filter(!is.na(tonnes)) %>%
      select(-rgn_name) %>%
      group_by(rgn_id) %>%
      do(
        mdl = lm(tonnes ~ year, data=.)) %>%
      summarize(
        rgn_id = rgn_id, 
        year_ix0  = coef(mdl)['(Intercept)'],
        year_coef = coef(mdl)['year']) %>%
      mutate(
        trend_tmp = year_coef / (year_coef * yr_min + year_ix0) * 5, # Save these as separate steps for error checking
        trend_min = pmin(trend_tmp, 1, na.rm = T),
        trend_max = pmax(trend_min, -1)) %>%
      arrange(rgn_id) %>%
      select(rgn_id, 
             trend.score = trend_max); head(d_mdl); summary(d_mdl) 
    
    # make sure there are no NA's--these often occur if a region only has 1 year of data
    stopifnot(sum(is.na(d_mdl$trend.score)) == 0)
    

    ## 2) calculate pop trend for each scenario, for fert and pest ----
    # no gapfilling of pop required as was done in 2013; the pop file called is complete (missing rgn_ids are unpopulated).
    
    pop_file = file.path(dir_neptune_data, 'model/GL-NCEAS-CoastalPopulation_v2013/data/', 'rgn_popsum2005to2015_inland25mi.csv') # dir_neptune_data defined in common.R                  
    pop = read.csv(pop_file) %>%
      filter(rgn_id < 255, 
             year %in% yr_min:yr_max); head(pop)
    
    ## calculate pop trends for past years:
    p_mdl_tmp = pop %>%
      select(rgn_id, year, 
             value = popsum) %>%
      group_by(rgn_id) %>%
      do(
        mdl = lm(value ~ year, data=.)) %>%
      summarize(
        rgn_id = rgn_id, 
        year_ix0  = coef(mdl)['(Intercept)'],
        year_coef = coef(mdl)['year']) %>%
      mutate(
        trend_tmp = year_coef / (year_coef * yr_min + year_ix0) * 5)
        
    # treat NAs and non-NAs differently. ave these as separate steps for error checking
    p_mdl = 
      rbind(p_mdl_tmp %>%
              filter(is.na(trend_tmp)) %>% # regions with entire timeseries of 0 are NAs. don't let them be capped in the same manner as below.
              mutate(trend_min = 0,
                     trend_max = 0),
            p_mdl_tmp %>%
              filter(!is.na(trend_tmp)) %>% 
              mutate(trend_min = pmin(trend_tmp, 1, na.rm = T),
                     trend_max = pmax(trend_min, -1))) %>%
      arrange(rgn_id) %>%
      select(rgn_id, 
             trend.score = trend_max); head(p_mdl) 
    
#  debug   check p_mdl_tmp %>% filter(is.na(trend_tmp))
# pop   %>% filter(rgn_id == 149)
# p_mdl %>% filter(rgn_id == 149)
# 
# pop   %>% filter(rgn_id == 158)
# p_mdl %>% filter(rgn_id == 158)
    
    ## 3) join fert and pest trends with appropriate pop trends, add whence bookkeeping ----
    # identify which rgn_ids aren't represented in pest and fert files (using anti_join)
    
    dp = rbind(d_mdl,
               p_mdl %>%
                 anti_join(d_mdl, by='rgn_id')) %>%
      arrange(rgn_id); head(dp); summary(dp)
    
    stopifnot(sum(is.na(dp$trend.score)) == 0)

    ## 4) any regions that did not have a population should have trend=NA ----
    
    rgns = read.csv('src/LookupTables/eez_rgn_2013master.csv') %>%
      select(rgn_id = rgn_id_2013,
             rgn_name = rgn_nam_2013)  %>%
      filter(rgn_id < 255) %>%
      arrange(rgn_id); head(rgns)
    
    dp_fin = rbind(dp, 
                   rgns %>%
                     anti_join(dp, by = 'rgn_id') %>%
                     mutate(trend.score = NA) %>%
                     select(-rgn_name)) %>%
      arrange(rgn_id); head(dp_fin); summary(dp_fin)
    
    
    ## 5) save as poth pressure trend and CW scores ----
    filesave = paste('rgn_cw_', tolower(v), '_trends_', names(scenario)[i], 'a.csv', sep='')
    write.csv(dp_fin, 
              file.path(dir_d, 'data', filesave), row.names = F)
  }  
}


# --- fin

