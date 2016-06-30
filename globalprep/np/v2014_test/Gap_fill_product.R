### summarizing the gap-fill commodity data to get an estimate of the proportion of data that was gap-filled for each
### region/product/year
### (products weighted by USD weights)
### MRF: April 20 2015

library(ggplot2)

weights <- read.csv('globalprep/FAO_commodities/v2014_test/data/np_harvest_usd_product-peak_eez2014_test-year-max-2011.csv')
## weights only include 2006:2011 data

gapData <- read.csv('globalprep/FAO_commodities/v2014_test/data/v2014_test_np_gapfill_report.csv')
table(gapData$gapfill)


# STEP 1: Figure out proportion of each region/year/product that was gap-filled (Each type of gap-filling counts as a 1, otherwise 0)
# STEP 2: Weight proportion of gap-filling by proportion of max USD for each product/region during past 6 years to get estimate of gap-filling across all products

productData <- gapData %>%
  group_by(rgn_id, product, year) %>%
  summarize(totalProducts=n(),
              totalGapfill=sum(!(gapfill %in% "none"))) %>%
  mutate(ProportionGapfill = totalGapfill / totalProducts) %>%
  left_join(weights, by=c("rgn_id", "product", "year")) %>%
  filter(!is.na(weight)) %>%
  ungroup() %>%
  group_by(rgn_id, year) %>%
  summarize(weighted_gapfilling=weighted.mean(ProportionGapfill, weight))
  
ggplot(subset(productData, year==2011), aes(weighted_gapfilling)) + 
  geom_histogram(fill="gray", color="black") +
  labs(title="Estimated NP gap-filling for each region", y="regions") + 
  theme_bw()

write.csv(productData, "globalprep/FAO_commodities/v2014_test/data/NP_gapfill_attr.csv", row.names=FALSE)
