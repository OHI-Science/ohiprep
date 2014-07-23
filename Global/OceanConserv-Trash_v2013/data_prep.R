#  data_prep.r

#  formerly clean.r: reformat Ocean Conservancy trash data (by JStewart Apr2013)
#  read in identified trash file
#  name_to_rgn.r
#  read in newly accessed 2012 file, but also older 2010 and 2011 files and concatenate them all. 
#  calculate pounds_per_mile
#  georegional gapfilling with gapfill_georegions.r

# Note: some countries have data for pounds but not miles; currently pounds_per_mile
# is calculated as NA, but could gapfill miles separately, weighted by coastline first


# the 2014 report has data from 2013. This is a fourth year of data available.  NOTE: data are available for 3 separate years. Things are confusing because the year identified in the report and in the data are different: So the 2012 report (pdf) has csv files named 2011. But the xlsx files used in OHI2012 are labeled 2010 and 2011. JS has checked and made sure that the most recent available data (2011 csv) is different from 2011 xlsx and 2010 xlsx. So in the coding and beyond, the data from the 2011 csv file is renamed and treated as 2012. 

# Reports are all in neptune_data:git-annex/Global/OceanConserv-Trash_v2013/reports
# Data are all in    neptune_data:git-annex/Global/OceanConserv-Trash_v2013/raw [some are .xls; some are .csv)
# 2013 data: 
#         2014 report has 2013 data: icc-data-2014.pdf has the same data as '!!Copy of 2013-14_US-Global_Summary_PPM.xlsx'
# 2012 data: 
#       2013 report 2013-trash-free-seas-report.pdf has the same data as '2013 ICC Data Release/International-PPM_OpenAccess.csv'
#       report from: http://www.oceanconservancy.org/our-work/international-coastal-cleanup/2013-trash-free-seas-report.pdf (googled) 
#       data from: http://www.oceanconservancy.org/our-work/international-coastal-cleanup/2012-ocean-trash-index.html
# 2011 data: 
#       2012 report has 2011 data: 2012-icc-data-pdf.pdf has the same data as 'ICC 2011_OTI-PPM_FINAL.xlsx'
# 2010 data:
#       2011 report has 2010 data: Marine_Debris_2011_Report_OC.pdf has the same data as OC2011_total.xlsx * mislabeling 2011 instead of 2010?

# libraries
library(gdata)
library(stringr)
library(zoo)
library(ohicore) # devtools::install_github('ohi-science/ohicore') # may require uninstall and reinstall

# get paths
source('src/R/common.R') # set dir_neptune_data
source('src/R/ohi_clean_fxns.R') # has functions: cbind_rgn(), sum_na()
dir_d = 'Global/OceanConserv-Trash_v2013' 
dir_a = file.path(dir_neptune_data, 'git-annex/Global/OceanConserv-Trash_v2013/raw') #* mislabeling 2011 instead of 2010?

## identify files ----
files = list(  
  f2013 = file.path(dir_a, '!!Copy of 2013-14_US-Global_Summary_PPM.xlsx'),
  f2012 = file.path(dir_a, '2013 ICC Data Release', 'International-PPM (1).xlsx'),
  f2011 = file.path(dir_a, 'ICC 2011_OTI-PPM_FINAL.xlsx'),
  f2010 = file.path(dir_a, 'OC2011_total.xlsx'))

## read in 2013 data ----
ft = read.xls(files$f2013, sheet = 2, header=T); head(ft); 
ft = ft[1:3,] # just keep People, Pounds, Miles
rownames(ft) = ft$Country
country = names(ft)[2:length(names(ft))]
f = as.data.frame(t(ft[,-1]), row.names = NULL) 
row.names(f) = 1:dim(f)[1]
f = cbind(country, f) %>%
  select(country, 
         pounds = Pounds,
         miles  = Miles) %>%
  filter(country != 'Totals') %>%
  mutate(year = 2013, 
         country = str_replace_all(country, fixed('.'), ' '), # replace . with spaces
         pounds  = str_replace_all(pounds,  ',', ''),         # replace , with nothing
         miles   = str_replace_all(miles,   ',', ''))        # replace , with nothing
f$pounds = as.numeric(as.character(factor(f$pounds)))
f$miles = as.numeric(as.character(factor(f$miles))); head(f); summary(f)

prob = f %>% 
  filter(miles <= 1) %>%
  mutate(pounds_per_mile = pounds/miles)

# deal with Netherlands Antilles
f = f %>%
  mutate(
    country = str_replace(country, 'Saba  Netherlands', 'Saba'),
    country = str_replace(country, 'Sint Maarten  Dutch West Indies', 'Sint Maarten'),
    country = str_replace(country, 'U S  Virgin Islands', 'US Virgin Islands')); head(f); summary(f)

## read in 2012 data ----
pt = read.xls(files$f2012, skip=1, na.strings=''); head(pt)
pt2 = pt %>%
  select(country = X, 
         island  = X.1,
         pounds  = Pounds.2,
         miles   = Miles.2) %>%
  filter(!country %in% c('Country or Location', 'Total')) %>%
  mutate(year = 2012,
         pounds = str_replace(pounds, '-', 'NA'),
         miles  = str_replace(as.numeric(miles), '-', 'NA'),)
pt2$pounds = as.numeric(as.character(pt2$pounds))
pt2$miles = as.numeric(as.character(pt2$miles)); head(pt2); summary(pt2)

# keep only 'Total' when multiple entries for individual islands
pt2$country = na.locf(pt2$country)
tot = pt2 %>%
  filter(island == 'Total')
pt3 = rbind(tot,
            pt2 %>%
              filter(!country %in% tot$country)) %>%
  arrange(country) %>% 
  select(-island); head(pt3)

# explore Netherland Antilles
ant = pt3 %>% 
  filter(country %in% 
           c('Netherlands Antilles', 'Sint Maarten', 'Curacao', 'Bonaire','Saba', 'Sint Eustasius', 'Aruba'))
ant  # returns Aruba, Bonaire, and Saba; no 'Netherlands Antilles' so no partitioning needed

p = pt3; head(p); summary(p)

## read in 2011 data ----
dt = read.xls(files$f2011, skip=1, na.strings=''); head(dt)
dt2 = dt %>%
  select(country = X, 
         island  = X.1,
         pounds  = Pounds.2,
         miles   = Miles.2) %>%
  filter(!country %in% c('Country or Location', 'Total')) %>%
  mutate(year = 2011,
         pounds = str_replace(pounds, '-', 'NA'),
         miles  = str_replace(as.numeric(miles), '-', 'NA'),)
dt2$pounds = as.numeric(as.character(dt2$pounds))
dt2$miles = as.numeric(as.character(dt2$miles)); head(dt2); summary(dt2)

# keep only 'Total' when multiple entries for individual islands
dt2$country = na.locf(dt2$country)
tot = dt2 %>%
  filter(island == 'Total')
dt3 = rbind(tot,
            dt2 %>%
              filter(!country %in% tot$country)) %>%
  arrange(country) %>% 
  select(-island); head(dt3)


# partition Netherland Antilles
ant = dt3 %>%  
  filter(country %in% 
           c('Netherlands Antilles', 'Sint Maarten', 'Curacao', 'Bonaire','Saba', 'Sint Eustasius', 'Aruba'))
ant # returns only 1 match for 'Netherlands Antilles'

d = rbind(dt3 %>%
            filter(!country %in% ant$country),
          data.frame(country = c('Sint Maarten', 'Curacao', 'Bonaire','Saba', 'Sint Eustasius', 'Aruba'),
                     pounds   = rep(ant$pounds/6),
                     miles    = rep(ant$miles/6),
                     year     = rep(2011))); head(d); summary(d)


## read in 2010 data ----
mt = read.xls(files$f2010) 
mt2 = mt %>%
  select(country = Country,
         pounds  = Pounds,
         miles   = Miles) %>%
  mutate(year = 2010,
         pounds = str_replace(pounds, '-', 'NA'),
         miles  = str_replace(as.numeric(miles), '-', 'NA'),)
mt2$pounds = as.numeric(as.character(mt2$pounds))
mt2$miles = as.numeric(as.character(mt2$miles)); head(mt2); summary(mt2)

# partition Netherland Antilles
ant = mt2 %>%  
  filter(country %in% 
           c('Netherlands Antilles', 'Sint Maarten', 'Curacao', 'Bonaire','Saba', 'Sint Eustasius', 'Aruba'))
ant # returns only 'Aruba': no partitioning for 'Netherlands Antilles'

m = mt2; head(m); summary(m)

## combine all years, collapse UK regions ----
tt = rbind(f, p, d, m) 

t = rbind(tt %>%
            filter(!country %in% c('United Kingdom', 'Northern Ireland','Scotland', 'Wales')),
          tt %>%
            filter(country %in% c('United Kingdom', 'Northern Ireland','Scotland', 'Wales')) %>%
            group_by(year) %>%
            summarize(pounds = sum(pounds),
                      miles  = sum(miles)) %>%
            mutate(country = 'United Kingdom') %>%
            select(country, year, pounds, miles))%>%
  arrange(country, year)

## calculate trash density: pounds/miles ----

t = t %>%
  mutate(pounds_per_mile = pounds/miles) %>%
  select(country, year, pounds, miles, pounds_per_mile)
  
# checking for miles that are super tiny
# t %>% filter(miles <= 1)
# #          year == 2013) %>%
# t %>% filter(country == 'Australia') # could fix this with an average of miles from other years
# t %>% filter(country == 'Ghana')     # that fix would help here too
# t %>% filter(country == 'Curacao')   # not here

# narrow selection
t = t %>%
  filter(miles != 0) %>%
  select(country, year, pounds_per_mile); head(t); summary(t)


# anyDuplicated(t[,c('country','year')])

## add rgn_ids with name_to_rgn ---- 
# source('../ohiprep/src/R/ohi_clean_fxns.R')
t_f = name_to_rgn(t, fld_name='country', flds_unique=c('country', 'year'), fld_value='pounds_per_mile', add_rgn_name=T) %>%
  arrange(rgn_id, year)

write.csv(t_f, file.path(dir_d, 'data', 'rgn_oc_trash_2014a_notgapfilled.csv'),
          na = '', row.names=FALSE)

## georegional gapfilling with gapfill_georegions.r ----
georegions = read.csv('../ohi-global/eez2013/layers/rgn_georegions.csv', na.strings='') %.%
  dcast(rgn_id ~ level, value.var='georgn_id')

georegion_labels = read.csv('../ohi-global/eez2013/layers/rgn_georegion_labels.csv') %.%    
  mutate(level_label = sprintf('%s_label', level)) %.%
  dcast(rgn_id ~ level_label, value.var='label') %.%
  left_join(
    read.csv('../ohi-global/eez2013/layers/rgn_labels.csv') %.%
      select(rgn_id, v_label=label),
    by='rgn_id') %.%
  arrange(r0_label, r1_label, r2_label, v_label); head(georegion_labels)


layersave = file.path(dir_d, 'data', 'rgn_oc_trash_2014a.csv')
attrsave  = file.path(dir_d, 'data', 'rgn_oc_trash_2014a_attr.csv')

# library(devtools); load_all('../ohicore')
# source('../ohicore/R/gapfill_georegions.r')
t_g_a = gapfill_georegions(
  data = t_f %.%
    filter(!rgn_id %in% c(213,255)) %.%
    select(rgn_id, year, pounds_per_mile),
  fld_id = 'rgn_id',
  georegions = georegions,
  georegion_labels = georegion_labels,
  r0_to_NA = TRUE, 
  attributes_csv = attrsave) # don't chain gapfill_georegions or will lose head(attr(d_g_a, 'gapfill_georegions')) ability

# investigate attribute tables
head(attr(t_g_a, 'gapfill_georegions'))  # or to open in excel: system(sprintf('open %s', attrsave))

# save
t_g = t_g_a %.%
  select(rgn_id, year, pounds_per_mile) %.%
  arrange(rgn_id, year); head(t_g); summary(t_g)

write.csv(t_g, layersave, na = '', row.names=FALSE)


## model trash; finalize layer ----
# from dir_neptune_data: model/GL-NCEAS-Pressures_v2013a/model_trash.R

# write out files using reference years
scenarios = list('2012a'=2011,
                 '2013a'=2012,
                 '2014a'=2013)

scen_earliest = scenarios[[names(scenarios)[1]]]

h_scen = t_g %>%
  filter(year >= scen_earliest)

ppm_max = max(h_scen$pounds_per_mile, na.rm=T)
h_max = h_scen %>% filter(pounds_per_mile == ppm_max)
print(h_scen %>%
        filter(pounds_per_mile == ppm_max))

for (scen in names(scenarios)){ # scen = names(scenarios)[1]
  
  yr = scenarios[[scen]]
  cat(sprintf('\nScenario %s using year == %d\n', scen, yr))
  
  h = t_g %>%
    filter(year == yr) %>%
    mutate(score_raw = log(pounds_per_mile + 1) / log(ppm_max),
           pressure_score = score_raw / (max(score_raw, na.rm=T) * 1.1)); head(h); summary(h)
  
  h_fin = h %>%
    select(rgn_id, pressure_score)
  stopifnot(anyDuplicated(h_fin[,c('rgn_id')]) == 0)
  
  csv = file.path(dir_d, 'data', sprintf('po_trash_%s.csv', scen))
  write.csv(h_fin, csv, row.names=F, na='')
}

## --- fin ---



