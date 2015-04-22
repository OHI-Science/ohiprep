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

ggplot(data, aes(x=x_99)) +
 geom_histogram(color="black", fill="gray") +
  theme_bw() +
  labs(title="NP: Exposure 99th quantile", x="Exposure Score", y="Number of observations")

ggplot(data, aes(x=x_95)) +
  geom_histogram(color="black", fill="gray") +
  theme_bw() +
  labs(title="NP: Exposure 95th quantile", x="Exposure Score", y="Number of observations")


ggplot(data, aes(x=exposure, fill=product)) +
  geom_histogram() +
  theme_bw() 

ggplot(data, aes(x=exposure, y=x_95, color=product, shape=product)) +
  geom_point(size=3) +
  geom_abline(slope=1, intercept=0) +
  labs(title="Comparing exposure calculated from max vs 95th quantile") +
  theme_bw()


ggsave(file.path(dir_np, "ExposureExplore/oldExpvs99Exp.png"))
  