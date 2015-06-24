# Data Prep for Wastewater Data Layer 
# The Ocean Health Index
### Omar Malik, 28 May 2015, Santa Barbara, CA

# Install libraries:

devtools::install_github('ohi-science/ohicore@dev') # may require uninstall and reinstall
getwd()
setwd("./globalprep/EPI_wastewater")

library(ohicore); # devtools::install_github('ohi-science/ohicore@dev'); 
library("dplyr")
library("readr")
library("tidyr")

# STEP: Define variables, and pull out what is necessary:

# NOTE: waste <- read.csv("YaleEPI_WastewaterTreatment_2014.csv", head = TRUE); head(waste) # raw file with time series for treatment % only; it's not normalized as index score;

waste <- read.csv("./raw/WASTECXN_2014.csv", head = TRUE); head(waste)

waste_sum_a <- waste %>% 
  select(iso, country, WASTECXN.2012) %>% 
  mutate(year = 2014, wastedec = WASTECXN.2012*.01) # Taking out country code and index score; making into long format; 

numc <- length(waste_sum_a[1,])
numr <- length(waste_sum_a[,1])
head(waste_sum_a)

# STEP: adding column names
colnames(waste_sum_a) <- c("ISO", "Country", "WasteIndex", "Year", "WasteIndexCoeff")

# STEP: removing NA rows, truncating text

waste_sum_b <- waste_sum_a[1:232,]
head(waste_sum_b)

# STEP: removing -9999s

waste_sum_b <- filter(waste_sum_b, WasteIndex > -1)

waste_sum_c <- select(waste_sum_b, ISO, Country, Year, WasteIndex, WasteIndexCoeff)

head(waste_sum_c); summary(waste_sum_c)

# NOTE: Rescaling - NA here unlesss you choose to use the raw data CSV instead; however, WasteIndex scores are normalized already from 0 - 1.
#waste_sum_e = waste_sum_d %>% select(iso, country, year, Treated) %>% mutate(rescale = (Treated)/max(Treated))
#  head(waste_sum_e);

# STEP: Remove Hong Kong: China-at-large represents a balanced approach already. 

waste_sum_ci <- waste_sum_c[-74,]

# STEP: Clean region IDs, using ohicore function 'name_to_rgn()'. *USER: You should choose whether to use WasteIndex or WasteIndexCoeff:

waste_sum_d <- ohicore::name_to_rgn(waste_sum_ci, fld_name= 'Country', fld_value = c('WasteIndexCoeff')); summary(waste_sum_d)# 
# m_d = name_to_rgn(waste_sum_c, fld_name='Country',
#                   fld_value='WasteIndexCoeff', add_rgn_name=T, collapse_fxn = 'weighted.mean', 
#                   collapse_csv = "../../../ohiprep/src/LookupTables/Pop_weight_chn_hkg_mtq_glp.csv",
#                   dir_lookup = "../../../ohiprep/src/LookupTables");head(m_d); summary(m_d) 

## !ISSUE: after applying this code, a problem occured where China has > 100. This is probably because it's summing Hong Kong and/or Taiwan into the data. 
## Fixing the above issue through the below method of creating a lookup table:

population_weights <- read.csv('../../../ohiprep/src/LookupTables/Pop_weight_chn_hkg_mtq_glp.csv'); tail(population_weights)

# !ISSUE: now getting error, `Error: length(fld) == 1 is not TRUE`  
#^ once the above is fixed, apply the below:

# m_d[m_d$rgn_id == 140,] # check that these look correct.
# m_d[m_d$rgn_id == 209,]
# 
# m_d_unique <- m_d %>%
#   select(rgn_id, rgn_name) %>%
#   unique() %>%
#   arrange(rgn_id)
# 

# !ISSUE-history: summary(waste_sum_d)
# rgn_id      WasteIndex      
# Min.   :  7   Min.   :  0.0000  
# 1st Qu.: 66   1st Qu.:  0.5312  
# Median :129   Median : 10.8688  
# Mean   :124   Mean   : 27.5250  
# 3rd Qu.:185   3rd Qu.: 49.4981  
# Max.   :255   Max.   :105.9668  # > 100 error

## STEP: For pressures analysis, invert values to 1 - x:

waste_sum_e <- mutate(waste_sum_d, Inverse = (1 - WasteIndexCoeff)) %>% select(rgn_id, Inverse)

# STEP: Save file in this directory - multiple files for different scenarios

write.csv(waste_sum_d, "cw_wastewater_gl2015.csv"); # Normal version where higher value = better score

write.csv(waste_sum_e, "po_wastewater_gl2015.csv"); # Inverse version where higher value = poorer score. Also, UPDATE: Removed Hong Kong manually per judgment that China data point accounts for urban and non-urban.

#### II. Analysis

#dim(waste_sum_d)
#summary(waste_sum_d) 
 
# 141 regions matching.
# 
# 40 landlocked countries were removed:
# 
# 4 duplicates -> # Question - how does it collapse China, Hong Hong?
#   
#   tmp_name                   landlocked
# Afghanistan                       1
# Andorra                           1
# Armenia                           1
# Austria                           1
# Belarus                           1
# Bhutan                            1
# Bolivia                           1
# Botswana                          1
# Burkina Faso                      1
# Burundi                           1
# Central African Republic          1
# Chad                              1
# Czech Republic                    1
# Ethiopia                          1
# Hungary                           1
# Kazakhstan                        1
# Kyrgyzstan                        1
# Laos                              1
# Lesotho                           1
# Luxembourg                        1
# Macedonia                         1
# Malawi                            1
# Mali                              1
# Moldova                           1
# Mongolia                          1
# Nepal                             1
# Niger                             1
# Paraguay                          1
# Rwanda                            1
# Serbia                            1
# Slovakia                          1
# Swaziland                         1
# Switzerland                       1
# Tajikistan                        1
# Turkmenistan                      1
# Uganda                            1
# Uzbekistan                        1
# Zambia                            1
# Zimbabwe                          1
#   
#   DUPLICATES found. Resolving by collapsing rgn_id with collapse_fxn: sum_na after first removing all NAs from duplicates...
# rgn_id
# fld_name     140 209
# China        0   1
# Guadeloupe   1   0
# Hong Kong    0   1
# Martinique   1   0

# View(waste_sum_d)

## Testing how it would look to have scores of CW status weighted by "Wastewater Index":

library(ggplot2)
test <- read.csv("scores_test.csv")
colnames(test) = c("goal","dimension","rgn_id","score")
head(test);
waste_by_scores <- left_join(test,waste_sum_d);

waste_by_scores_b <- subset(waste_by_scores, goal == "CW" & dimension == "status"); head(waste_by_scores_b)
#View(waste_by_scores_b)

scenario_a <- waste_by_scores_b %>% mutate(scorewaste = score*WasteIndexCoeff);

a <- as.data.frame(subset(scenario_a, !is.na(scorewaste == TRUE)))
head(a)
summary(a)
ai <- mutate(a, diff = (score - scorewaste), percentchg = (score - scorewaste)/(score)*100); 
aii <- arrange(ai, percentchg)
View(aii);

p <- qplot(score, scorewaste, data = a) # Compare CW status score against (statusscore*wasteindex));

g <- ggplot(a, aes(x = score, y = scorewaste, label = rgn_id)) + geom_point(position="jitter") + geom_text(aes(label=rgn_id),hjust=0, vjust=0) + geom_jitter(color="blue")

# Countries that appear robust to this test include (top 3 % change): 185, Monaco, 208, Singapore, 177, Netherlands;
# Countries that appear to have been down-scores by this test include (bottom 3 % change): Angola, Bangladesh, Myanmar;

## Testing it against JMP/WHO data

#old: test2 <- read.csv("Waste_ACSAT_exploration.csv"); head(test2)
test2 <- read.csv("ACSAT_2014.csv"); head(test2)
test2i <- left_join(test2, waste); head(test2i)

popath <- read.csv("./raw/compare/po_pathogens.csv"); head(popath)
ponutr <- read.csv("./raw/compare/po_nutrients.csv"); head(ponutr)

testing1 <- test2i %>% select(country, ACSAT.2011, WASTECXN.2012)
head(testing1)

testing1i <- filter(testing1, ACSAT.2011 > -1, WASTECXN.2012 > 1)

qplot(ACSAT.2011, WASTECXN.2012, data = testing1i);
g <- ggplot(testing1i, aes(x = ACSAT.2011, y = WASTECXN.2012, label = country)) + geom_point() + geom_text(aes(label=country),hjus=0,vjust=0) + geom_jitter(color="green")

### COMPARING PO_PATHOGENS to WASTE_SUM_E:

waste_by_po <- left_join(popath, waste_sum_e)
#View(waste_by_po)
g <- ggplot(waste_by_po, aes(x = pressure_score, y = Inverse, label = rgn_id)) + geom_point() + geom_text(aes(label=rgn_id),hjus=0,vjust=0) + geom_jitter(color="brown")
summary(waste_by_po)

#### What's the difference between the new po_pathogens value (inverse) vs the given po_pathogens value?

waste_by_po_i <- mutate(waste_by_po, diff = pressure_score - Inverse, abs_perchg = abs(((Inverse)-(pressure_score))/(Inverse)))
waste_by_po_ii <- arrange(waste_by_po_i, by = perchg)
head(waste_by_po_ii); View(waste_by_po_ii)

# TOP 5 PERFORMERS - little change
# rgn_id	pressure_score	Inverse	diff	perchg
# 1	178	0.4026932021	0.39272727	0.009965929	0.02537621
# 2	62	0.5768837213	0.60606750	-0.029183779	0.04815269
# 3	209	0.7784220895	0.81820088	-0.039778788	0.04861739
# 4	203	0.8410586264	0.89512397	-0.054065341	0.06039984
# 5	52	0.3291913693	0.35730972	-0.028118353	0.07869462

# includes (in desc order): Poland, Morocco, China, India, Bahrain

# BOTTOM 5 PERFORMERS - more change
# 135  210	0.0000000000	0.28740000	-0.287400000	1.00000000
# 136	222	0.0000000000	0.12142857	-0.121428571	1.00000000
# 137	223	0.0000000000	0.22946657	-0.229466573	1.00000000
# 138	77	0.4414537882	0.17829146	0.263162331	1.47602322
# 139	184	0.2728055322	0.08562404	0.187181487	2.18608555

# includes (in desc order): Mexico, Guatemala, Ecuador, Peru, Venezuela

g <- ggplot(waste_by_po_ii, aes(x = pressure_score, y = Inverse, label = rgn_id)) + geom_point() + geom_text(aes(label=rgn_id),hjus=0,vjust=0) + geom_jitter(color="green")

# 184 and 77 are outliers if you compare x = pressure_score, y = perchg: 184 = italy, 77 = Syria

### COMPARING PO_NUTRIENTS to WASTE_SUM_E:

waste_by_nutr <- left_join(ponutr, waste_sum_e)
#View(waste_by_nutr)
g <- ggplot(waste_by_nutr, aes(x = pressure_score, y = Inverse, label = rgn_id)) + geom_point() + geom_text(aes(label=rgn_id),hjus=0,vjust=0) + geom_jitter(color="red")

plot(ponutr); summary(ponutr)

sqrt((.5*.15))

### SAMPLE EQUATION. CREATING DATA LAYER TO TEST; ANALYSIS BELOW:

# [u(W) + r(P)] = composite pathogens pressure score
# u = proportion of (coastal, or total) population that is urban
# r = proportion of (coastal, or total) population that is rural
# 
# where (u + r) = 1
# S = (1 - wasteindex) = (waste_sum_e) #as a proxy for urban water quality
# A = (pathogens pressure) = (popath) #as a  proxy for rural water quality

## First scenario: 50/50 urban an rural
u = .5
r = 1-(u)

testdf1 <- waste_by_po %>% mutate(po_adj = (pressure_score*r), wi_adj = (Inverse*u), score_new=wi_adj+po_adj, u, r)
testdf1i <- arrange(testdf1, by = score_new); View(testdf1i); summary(testdf1i)
score5050 <- select(testdf1i, rgn_id, score5050 = score_new); head(score5050)

## TOP PERFORMERS: 
# rgn_id pressure_score    Inverse       po_adj      wi_adj       score
# 1      185   0.0000000000 0.00000000 0.0000000000 0.000000000 0.000000000
# 2      208   0.0000000000 0.00350000 0.0000000000 0.001750000 0.001750000
# 3      177   0.0000000000 0.01181818 0.0000000000 0.005909091 0.005909091
# 4      180   0.0000000000 0.02072905 0.0000000000 0.010364525 0.010364525
# 5      176   0.0000000000 0.04819460 0.0000000000 0.024097300 0.024097300

## BOTTOM PERFORMERS
# 135 195	0.7637226608	0.99400000	0.3818613304	0.497000000	0.878861330	0.5	0.5
# 136	114	0.7971103656	1.00000000	0.3985551828	0.500000000	0.898555183	0.5	0.5
# 137	196	0.8571024911	0.98918284	0.4285512456	0.494591419	0.923142664	0.5	0.5
# 138	204	0.8617221786	1.00000000	0.4308610893	0.500000000	0.930861089	0.5	0.5
# 139	98	0.8674064665	1.00000000	0.4337032332	0.500000000	0.933703233	0.5	0.5
# 140	99	0.9090909091	0.99991600	0.4545454545	0.499958000	0.954503455	0.5	0.5

## Second scenario: 25 urban, 75 rural:
u = .25
r = 1-(u)

testdf2 <- waste_by_po %>% mutate(po_adj = (pressure_score*r), wi_adj = (Inverse*u), score=wi_adj+po_adj, u, r)
testdf2i <- arrange(testdf2, by = score); View(testdf2i)
score2575 <- select(testdf2i, rgn_id, score2575 = score); head(score2575)

# rgn_id pressure_score    Inverse       po_adj      wi_adj       score
# 1      185   0.0000000000 0.00000000 0.0000000000 0.000000000 0.000000000
# 2      208   0.0000000000 0.00350000 0.0000000000 0.000875000 0.000875000
# 3      177   0.0000000000 0.01181818 0.0000000000 0.002954545 0.002954545
# 4      180   0.0000000000 0.02072905 0.0000000000 0.005182263 0.005182263
# 5      176   0.0000000000 0.04819460 0.0000000000 0.012048650 0.012048650

##BOTTOM PERFORMERS
# 135 114	0.7971103656	1.00000000	0.5978327742	0.250000000	0.847832774
# 136	203	0.8410586264	0.89512397	0.6307939698	0.223780992	0.854574962
# 137	196	0.8571024911	0.98918284	0.6428268683	0.247295709	0.890122578
# 138	204	0.8617221786	1.00000000	0.6462916339	0.250000000	0.896291634
# 139	98	0.8674064665	1.00000000	0.6505548498	0.250000000	0.900554850
# 140	99	0.9090909091	0.99991600	0.6818181818	0.249979000	0.931797182

## Third scenario: 75 urban, 25 rural:

u = .75
r = 1-(u)

testdf3 <- waste_by_po %>% mutate(po_adj = (pressure_score*r), wi_adj = (Inverse*u), score=wi_adj+po_adj, u, r)
testdf3i <- arrange(testdf3, by = score); View(testdf3i)
score7525 <- select(testdf3i, rgn_id, score7525 = score); head(score7525)

##TOP PERFORMERS:
# rgn_id  pressure_score	Inverse	po_adj	wi_adj	score
# 1	185	0.0000000000	0.00000000	0.000000e+00	0.000000000	0.000000000
# 2	208	0.0000000000	0.00350000	0.000000e+00	0.002625000	0.002625000
# 3	177	0.0000000000	0.01181818	0.000000e+00	0.008863636	0.008863636
# 4	180	0.0000000000	0.02072905	0.000000e+00	0.015546788	0.015546788
# 5	176	0.0000000000	0.04819460	0.000000e+00	0.036145950	0.036145950

##BOTTOM PERFORMERS:
# 35  195	0.7637226608	0.99400000	1.909307e-01	0.745500000	0.936430665
# 136	114	0.7971103656	1.00000000	1.992776e-01	0.750000000	0.949277591
# 137	196	0.8571024911	0.98918284	2.142756e-01	0.741887128	0.956162751
# 138	204	0.8617221786	1.00000000	2.154305e-01	0.750000000	0.965430545
# 139	98	0.8674064665	1.00000000	2.168516e-01	0.750000000	0.966851617
# 140	99	0.9090909091	0.99991600	2.272727e-01	0.749937000	0.977209727

## Fourth scenario: 90 urban, 10 rural:

u = .90
r = 1-(u)

testdf4 <- waste_by_po %>% mutate(po_adj = (pressure_score*r), wi_adj = (Inverse*u), score=wi_adj+po_adj, u, r)
testdf4i <- arrange(testdf4, by = score); View(testdf4i)
score9010 <- select(testdf4i, rgn_id, score9010 = score); head(score9010)

# STEP: Aggregate differences in weightings test:

weighttest <- cbind(score5050, score2575, score7525, score9010); head(weighttest)
weighttesti <- weighttest[,-3]; weighttestii <- weighttesti[,-4]; weighttestiii <- weighttestii[,-5]; head(weighttestiii)
weight_test <- write.csv(weighttestiii, "po_adjusted_weight-test.csv")

# STEP: Graph absolute value of the difference per region using a 50-50 scenario:
g <- ggplot(testdf1v, aes(x = rgn_id, y = diff, label = rgn_id)) + geom_point() + geom_text(aes(label=rgn_id),hjus=0,vjust=0) + geom_jitter(color="red") 


# ##TOP PERFORMERS:

# rgn_id	pressure_score	Inverse	po_adj	wi_adj	score	u	r
# 1	185	0.0000000000	0.00000000	0.000000e+00	0.00000000	0.00000000	0.9	0.1
# 2	208	0.0000000000	0.00350000	0.000000e+00	0.00315000	0.00315000	0.9	0.1
# 3	177	0.0000000000	0.01181818	0.000000e+00	0.01063636	0.01063636	0.9	0.1
# 4	180	0.0000000000	0.02072905	0.000000e+00	0.01865615	0.01865615	0.9	0.1
# 5	176	0.0000000000	0.04819460	0.000000e+00	0.04337514	0.04337514	0.9	0.1

# ##BOTTOM PERFORMERS:
# 135 195	0.7637226608	0.99400000	7.637227e-02	0.89460000	0.97097227
# 136	196	0.8571024911	0.98918284	8.571025e-02	0.89026455	0.97597480
# 137	114	0.7971103656	1.00000000	7.971104e-02	0.90000000	0.97971104
# 138	204	0.8617221786	1.00000000	8.617222e-02	0.90000000	0.98617222
# 139	98	0.8674064665	1.00000000	8.674065e-02	0.90000000	0.98674065
# 140	99	0.9090909091	0.99991600	9.090909e-02	0.89992440	0.99083349




#Finding:
#  * consistently, top performers are stable
#  * bottom performers vary more across treaments: incl. 195, 114, 196, 204, 98, 99. 203 appears Scenario 2 (25 urban, 75 rural); 

## STEP: Make the composite data layer file for use in the Toolbox. Choose from the scenarios above.

u = .5
r = 1-(u)

testdf1 <- waste_by_po %>% mutate(po_adj = (pressure_score*r), wi_adj = (Inverse*u), score=wi_adj+po_adj, u, r)
testdf1i <- arrange(testdf1, by = score);
testdf1ii <- select(testdf1i, rgn_id, score); head(testdf1ii)
colnames(testdf1ii) = c("rgn_id","pressure_score")
testdf1iii <- select(testdf1ii, rgn_id, pressure_score); head(testdf1iii)
po_pathogens_index_gl2014 <- write.csv(testdf1iii, "po_pathogens_index_gl2014.csv")

## ANSLYSIS: comparing po_pathogens_index to standard po_pathogens data:

#### Question: What is the % change to this pressure layer, if we include the new composite?

head(testdf1i)
testdf1iv <- mutate(testdf1i, diff = abs(pressure_score - score), perchg = abs(((score)-(pressure_score))/(score)))
testdf1v <- arrange(testdf1iv, by = diff)
head(testdf1v); View(testdf1v)
po_pathogens_adj_5050 <- write.csv(testdf1v, "po_pathogens_adj_5050.csv")

#### Answer: Here are the top and bottom five.

# TOP FIVE, LEAST CHANGE
# rgn_id pressure_score   Inverse    po_adj    wi_adj     score   u   r         diff     perchg
# 1    178      0.4026932 0.3927273 0.2013466 0.1963636 0.3977102 0.5 0.5  0.004982965 0.01252913
# 2     62      0.5768837 0.6060675 0.2884419 0.3030337 0.5914756 0.5 0.5 -0.014591889 0.02467031
# 3    209      0.7784221 0.8182009 0.3892110 0.4091004 0.7983115 0.5 0.5 -0.019889394 0.02491433
# 4    203      0.8410586 0.8951240 0.4205293 0.4475620 0.8680913 0.5 0.5 -0.027032670 0.03114035
# 5     52      0.3291914 0.3573097 0.1645957 0.1786549 0.3432505 0.5 0.5 -0.014059176 0.04095893

# includes (in descending order): Poland, Morocco, China, India, Bahrain

# BOTTOM FIVE, GREATEST CHANGE

# 135  8	0.0000000000	0.60000000	0.0000000000	0.300000000	0.300000000	0.5	0.5	-0.300000000	1.00000000
# 136	68	0.0000000000	0.62985455	0.0000000000	0.314927273	0.314927273	0.5	0.5	-0.314927273	1.00000000
# 137	71	0.0000000000	0.71140000	0.0000000000	0.355700000	0.355700000	0.5	0.5	-0.355700000	1.00000000
# 138	50	0.0000000000	0.71455641	0.0000000000	0.357278204	0.357278204	0.5	0.5	-0.357278204	1.00000000
# 139	81	0.0000000000	0.90559592	0.0000000000	0.452797959	0.452797959	0.5	0.5	-0.452797959	1.00000000

# includes (in descending order): Palau, Malta, Bulgaria, Saudi Arabia, Cyprus

# graph it:

head(testdf1v)
g <- ggplot(testdf1v, aes(x = pressure_score, y = score, label = rgn_id)) + geom_point() + geom_text(aes(label=rgn_id),hjus=0,vjust=0) + geom_jitter(color="red") 

# graph absolute value of the difference per region 
g <- ggplot(testdf1v, aes(x = rgn_id, y = diff, label = rgn_id)) + geom_point() + geom_text(aes(label=rgn_id),hjus=0,vjust=0) + geom_jitter(color="red") 


### 


## FINAL STEP: Run this data layer in place of "po_pathogens" in the Toolbox, either for Global or Regional. See elsewhere for results. 
## comment: Global Toolbox is giving errors. Cannot sandbox it? Use a temporary regional one, like 
## comment: Using BENIN regional toolbox - the bottom performer in terms of % change as affected by adding wasteindex. This means we can see the extreme outcome of results, and calibrate from there.

