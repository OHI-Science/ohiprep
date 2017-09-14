## Code to get Global competiveness index data

## describe location of raw data:
dir_wef  <- file.path(dir_M, 'git-annex/globalprep/_raw_data/WEF-Economics/d2017/GCI_Dataset_2006-2016.csv')

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
  mutate(year = as.numeric(as.character(substring(year, 1, 4)))) %>%
  select(year, country, score)


ttci <- ttci %>%
  mutate(country = as.character(country)) %>%
  mutate(country = ifelse(country == "Congo, Democratic Rep.", "Democratic Republic of the Congo", country)) %>%
  mutate(country = ifelse(country == "Côte d'Ivoire", "Ivory Coast", country))


ttci_rgn <- name_2_rgn(df_in = ttci, 
                       fld_name='country', 
                       flds_unique=c('country','year'))

weight_data <- data.frame(country = c("China", "Hong Kong SAR"),
                          population = c(1379000000, 7347000))

ttci_rgn <- ttci_rgn %>%
  arrange(country, year) %>%
  left_join(weight_data, by = "country") %>%
  mutate(population = ifelse(is.na(population), 1, population)) %>%
  group_by(rgn_id, rgn_name, year) %>%
  summarize(score = weighted.mean(score, population)) %>%
  select(rgn_id, rgn_name, year, score)

head(ttci_rgn, 10)

### Save TTCI data file
write_csv(ttci_rgn, 'intermediate/wef_ttci.csv')

## compare across years
tmp <- read.csv('intermediate/wef_ttci.csv', stringsAsFactors = FALSE) %>%
  spread(year, score)

plot(tmp$`2016`, tmp$`2015`)
abline(0,1, col="red")

plot(tmp$`2015`, tmp$`2014`)
abline(0,1, col="red")
