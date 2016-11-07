####  Exploring the bes way to fill in the exposure values for regions/products without corresponding habitat data

library(zoo)  
library(ohicore) # devtools::install_github('ohi-science/ohicore') # may require uninstall and reinstall

dir_np <- 'globalprep/FAO_commodities'
data_year <- 'v2014_test'
scenario  <- 'eez2014_test'

dir_d <- sprintf('%s/%s', dir_np, data_year)
### NOTE: Set output paths to here, but do not use setwd().
###       This way, any scripts and code in ohiprep will still work, b/c always in root ohiprep dir.

### set dir_neptune_data and load common libraries (tidyr, dplyr, stringr) 
source('src/R/common.R') 
### NOTE: The default path should already be your ohiprep root directory for the rest to work.
###       Otherwise, presume that scripts are always working from your default ohiprep folder

### access functions specific to FAO data cleanup
source('src/R/fao_fxn.R')

### Load NP-specific user-defined functions
source(sprintf('%s/R/np_fxn.R', dir_np))


data <- read.csv('globalprep/FAO_commodities/ExposureExplore/np_exp.csv')
NAdata <- filter(data, is.na(exposure))

summary(NAdata)

h <- data %>% add_georegion_id()

# corals 
mod <- lm(exposure ~ as.factor(georgn_id), data=subset(h, product=="corals" & year==2010))
summary(mod)


# corals (georegion explains ~ 50% of the variation in exposure)
mod <- lm(exposure ~ as.factor(georgn_id), data=subset(h, product=="seaweeds" & year==2010))
summary(mod)

library(nlme)
library(ggplot2)
data <- h %>%
  filter(product=="corals") %>%
  filter(!is.na(exposure))

mod1 <- lme(exposure ~ year, random= ~1|rgn_id, data=data, method="REML")
mod2 <- lme(exposure ~ year + as.factor(georgn_id), random= ~1|rgn_id, data=data, method="REML")

AIC(mod1, mod2)

ggplot(data, aes(exposure)) +
  geom_histogram(fill="gray", color="black") +
  labs(title = "Coral: Distribution of exposure (all years)")
quantile(data$exposure)
mean(data$exposure)


### seaweeds
data <- h %>%
  filter(product=="seaweeds") %>%
  filter(!is.na(exposure))

mod1 <- lme(exposure ~ year, random= ~1|rgn_id, data=data, method="REML")
mod2 <- lme(exposure ~ year + as.factor(georgn_id), random= ~1|rgn_id, data=data, method="REML")

AIC(mod1, mod2)

ggplot(data, aes(exposure)) +
  geom_histogram(fill="gray", color="black") +
  labs(title = "Seaweeds: Distribution of exposure (all years)")
quantile(data$exposure)
mean(data$exposure)


##### Explorign different quantiles for setting the "max" harvest/area values in Exposure
#####  This is from the 2013 assessment

data <- read.csv(file.path(dir_np, "ExposureExplore/exp_explore.csv"))
library(ggplot2)

### histograms for each product separately
for (prod in (unique(data$product))) {
  data_prod <- data %>% 
    filter(product == prod)
  ggplot(data_prod, aes(x=exposure)) +
    geom_histogram(color="black", fill="gray") +
    theme_bw() +
    labs(title=paste("NP Exposure:", prod), x="Exposure Score", y="Number of observations")
  ggsave(file.path(dir_np, paste("ExposureExplore/", prod, "_exp.jpg", sep = '')))
}

### quantiles for each product individually
for (prod in (unique(data$product))) {
  data_prod <- data %>% 
    filter(product == prod)
  print(sprintf('Quantiles for exposure product == %s based on max value', prod))
  print(round(quantile(data_prod$exposure, na.rm = TRUE), 5))
  if(prod != 'fish_oil') {
    print(sprintf('Quantiles for exposure product == %s based on 99th quantile', prod))
    print(round(quantile(data_prod$x_99, na.rm = TRUE), 5))
    print(sprintf('Quantiles for exposure product == %s based on 95th quantile', prod))
    print(round(quantile(data_prod$x_95, na.rm = TRUE), 5))
  }
}


### histogram combining all products except fish oil
all_but_fish <- data %>% filter(product != 'fish_oil')
ggplot(all_but_fish, aes(x=exposure, fill = product)) +
  geom_histogram() +
  theme_bw() +
  labs(title="NP: Exposure all but fish oil", x="Exposure Score", y="Number of observations")
ggsave(file.path(dir_np, "ExposureExplore/all_but_fish_oil_exp.jpg"))

### facet wrapped histograms for all products at current exposure
ggplot(data, aes(x=exposure, fill = product)) +
  geom_histogram() +
  facet_wrap(~product, nrow=2) +
  theme_bw() +
  labs(title="NP: Exposure by product, calculated from maximum by product", x="Exposure Score", y="Number of observations")
ggsave(file.path(dir_np, "ExposureExplore/hist_all_100_facet.jpg"))

### facet wrapped histograms for all products at exposure normalized to 99th quantile
ggplot(all_but_fish, aes(x=x_99, fill = product)) +
  geom_histogram() +
  facet_wrap(~product, nrow=2) +
  theme_bw() +
  labs(title="NP: Exposure by product, calculated from 99th quantile", x="Exposure Score", y="Number of observations")
ggsave(file.path(dir_np, "ExposureExplore/hist_all_99_facet.jpg"))

### facet wrapped histograms for all products at exposure normalized to 95th quantile
ggplot(all_but_fish, aes(x=x_95, fill = product)) +
  geom_histogram() +
  facet_wrap(~product, nrow=2) +
  theme_bw() +
  labs(title="NP: Exposure by product, calculated from 95th quantile", x="Exposure Score", y="Number of observations")
ggsave(file.path(dir_np, "ExposureExplore/hist_all_95_facet.jpg"))

ggplot(all_but_fish, aes(x=exposure, y=x_99, color=product)) +
  geom_point(size=3) +
  geom_abline(slope=1, intercept=0) +
  labs(title="NP: Comparing exposure calculated from max vs 99th quantile",
       x = 'exposure 100th quantile', y = 'exposure 99th quantile') +
  theme_bw()
ggsave(file.path(dir_np, "ExposureExplore/scatter_100vs99.jpg"))

ggplot(all_but_fish, aes(x=exposure, y=x_95, color=product)) +
  geom_point(size=3) +
  geom_abline(slope=1, intercept=0) +
  labs(title="NP: Comparing exposure calculated from max vs 95th quantile",
       x = 'exposure 100th quantile', y = 'exposure 95th quantile') +
  theme_bw()
ggsave(file.path(dir_np, "ExposureExplore/scatter_100vs95.jpg"))


ggplot(data, aes(x=exposure, fill=product)) +
  geom_histogram() +
  theme_bw() 

### Examining specific cases - looking at max harvest intensity for each product
### Corals: max intensity in Singapore 1990, and close second S. Africa in 1998.
data_trim <- data %>%
  select(rgn_name, rgn_id, product, year, tonnes, km2, expos_raw)

co_peak1 <- data_trim %>% 
  filter(product == 'corals' & rgn_name == 'South Africa') %>%
  mutate(product = 'corals_zaf')

co_peak2 <- data_trim %>% 
  filter(product == 'corals' & rgn_name == 'Singapore') %>%
  mutate(product = 'corals_sgp')

### Seaweeds: max intensity in Chile 2010
sw_peak <- data_trim %>% 
  filter(product == 'seaweeds' & rgn_name == 'Chile') %>%
  mutate(product = 'seaweeds_chl')

### Ornamentals: max intensity in Spain, 2010
or_peak <- data_trim %>% 
  filter(product == 'ornamentals' & rgn_name == 'Spain' & year >= 1990) %>%
  mutate(product = 'ornamentals_esp')

### Sponges: max intensity in Spain, 2010
sp_peak <- data_trim %>% 
  filter(product == 'sponges' & rgn_name == 'Spain') %>%
  mutate(product = 'sponges_esp')

### Shells: max intensity in Faeroe Islands, 2003
sh_peak <- data_trim %>% 
  filter(product == 'shells' & rgn_name == 'Faeroe Islands') %>%
  mutate(product = 'shells_fro')

maxima <- bind_rows(list(co_peak1, co_peak2, sw_peak, or_peak, sp_peak, sh_peak))

ggplot(maxima, aes(x=year, y=expos_raw, color=product)) +
  geom_line(size=3) +
  labs(title="NP: product trends for region with max harvest intensity",
       x = 'year', y = 'harvest intensity (tonnes/km^2') +
  theme_bw()
ggsave(file.path(dir_np, "ExposureExplore/peak_exp.jpg"))

