### Create box plot for each scenario

# get list of files (for all scenarios) - status (then eventually trend)
# define base status as IUCN pref, 0.4 aquamaps thresh

# smash together into a data frame of:
# rgn_id | base status | scenario 1 status | scen 2 st | scen 3 st |

# then box plot those bastards

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

dir_git  <- '~/github/ohiprep/globalprep/SPP_ICO/vAM_IUCN'
st_base  <- read.csv(file.path(dir_git, 'data/spp_status_global_IUCNpref_prob0.4.csv'),  stringsAsFactors = FALSE)
st_scen1 <- read.csv(file.path(dir_git, 'data/spp_status_global_IUCNpref_prob0.01.csv'), stringsAsFactors = FALSE)
st_scen2 <- read.csv(file.path(dir_git, 'data/spp_status_global_AMpref_prob0.4.csv'),    stringsAsFactors = FALSE)
st_scen3 <- read.csv(file.path(dir_git, 'data/spp_status_global_AMpref_prob0.01.csv'),   stringsAsFactors = FALSE)

### scenario 1: prefer IUCN data; drop AM probability threshold to 0.01
### scenario 2: prefer AM data; keep AM prob thresh at 0.40
### scenario 3: prefer AM data; drop AM prob thresh to 0.01

st_all <- st_base %>% rename(st_b = score) %>%
  left_join(st_scen1 %>% rename(st_1 = score), by = 'rgn_id') %>%
  left_join(st_scen2 %>% rename(st_2 = score), by = 'rgn_id') %>%
  left_join(st_scen3 %>% rename(st_3 = score), by = 'rgn_id')
  
st_all <- st_all %>% mutate(absdiff1 = (st_1 - st_b)*100,
                            absdiff2 = (st_2 - st_b)*100,
                            absdiff3 = (st_3 - st_b)*100,
                            reldiff1 = absdiff1/st_b,
                            reldiff2 = absdiff2/st_b,
                            reldiff3 = absdiff3/st_b) %>%
  gather(scenario, difference, absdiff1:reldiff3)
  
bplot_abs <- ggplot(data = st_all %>% filter(str_detect(scenario, 'abs')),
                    aes(x = scenario, y = difference)) +
  theme(text = element_text(family = 'Helvetica', color = 'gray30', size = 12),
        plot.title = element_text(size = rel(1.5), hjust = 0, face = 'bold'),
        legend.position = 'none') +
  geom_point(position = position_jitter(w = .1), aes(color = scenario), alpha = .5) +
  geom_boxplot(size = .75, fill = NA) +
  labs(title = 'SPP status difference by scenario',
       x = 'Scenario',
       y = 'Change in status') +
  scale_x_discrete(labels = c('IUCN/AM, 1%', 'AM/IUCN, 40%', 'AM/IUCN, 1%'))

bplot_abs
ggsave(file.path(dir_git, 'graphs/boxplot_abs_diff.png'))

bplot_rel <- ggplot(data = st_all %>% filter(str_detect(scenario, 'rel')),
                    aes(x = scenario, y = difference)) +
  theme(text = element_text(family = 'Helvetica', color = 'gray30', size = 12),
        plot.title = element_text(size = rel(1.5), hjust = 0, face = 'bold'),
        legend.position = 'none') +
  geom_point(position = position_jitter(w = .1), aes(color = scenario), alpha = .5) +
  geom_boxplot(size = .75, fill = NA) +
  labs(title = 'SPP status percent change by scenario',
       x = 'Scenario',
       y = '% Change in status') +
  scale_x_discrete(labels = c('IUCN/AM, 1%', 'AM/IUCN, 40%', 'AM/IUCN, 1%'))

bplot_rel
ggsave(file.path(dir_git, 'graphs/boxplot_rel_diff.png'))
