######################################################
## Sequence to run the scripts to calculate FIS data
######################################################

1. CleanRawData.R 
  cleans the data from saup_ohi. There are lots of repeats with the same EEZID/FAOAreaID/Year/TaxonKey.  According to Lydia, they are legit.  But they need to be summarized for our analyses.
  - sums the catch with same fao, saup, taxon, year id's
 Inputs: Neptune: git-annex/globalprep/SAUP_FIS_data/v2015/raw/Catch_v16072015.csv
Outputs: Neptune: git-annex/globalprep/SAUP_FIS_data/v2015/tmp/Catch_v16072015_summary.csv'
  prepares catch data for cmsy code to calculate b/bmsy values 
 - summarizes catch by taxon/fao/year (taxon/fao combination is how we define a stock for CMSY)
   excludes data prior to 1980, includes stock with >= 10 years non-zero catch data; includes only taxa identified to species (N=2684 stocks)
   Outputs: Github: globalprep/SAUP_FIS/v2015/tmp/b_bmsy_v16072015.csv
   
2. meanCatch.R
 Input: Neptune:git-annex/globalprep/SAUP_FIS_data/v2015/tmp/Catch_v16072015_summary.csv
 calculates mean catch: 
 - adds zero values to catch after first recorded catch >0
 - subset catch to years >= 1980 (data prior to this is unreliable)
 - For weighting RAM B/Bmsy values at the SAUP level: calculate mean catch for each fao_id/saup_id/Taxon so values across all years are the same
 Output: Github:globalprep/SAUP_FIS/v2015/tmp/mean_catch_saup_fao.csv

- converts saup regions to ohi regions
There are several saup regions that are represented by multiple OHI regions.  For example saup_id 251 
corresponds to ohi_ids 33, 34, 35.  In these cases, these regions were all given the same catch scores.
Ther are also many cases where an OHI region is comprised of multiple SAUP regions. In these cases the 
catch for the saup regions that comprise a single OHI region are summed.

- For EEZ regions: high seas regions are excluded.  For each fao_rgn/ohi_rgn/Taxon the catch across years is averaged such that each fao_rgn/ohi_rgn/Taxa has the same catch across all years.  Filtered to include only years relevant to analysis (2005-2010). 

Outputs: 'globalprep/SAUP_FIS/v2015/data/mean_catch.csv' (N=184,393)

- Same done for High seas regions
Outputs: Github:globalprep/SAUP_FIS/v2015/data/mean_high_seas_catch.csv (N=9473)

****
B/bmsy data:
We use both the RAM database and the cmsy method to calculate b/bmsy.

CMSY method:

3. bbmsy_cmsy.R
calculates b/bmsy scores for 2,684 stocks using uniform and constrained priors
The "datalimited" package and function cmsy is used to calculate b/bmsy.

We use two general methods to calculate mean b/bmsy from the catch data: uniform vs. constrained 
priors. The following document provides information on how to define these methods in the cmsy function 
(C:\Users\Melanie\Github\ohiprep\Global\FIS_Bbmsy\DataExample\ReadmeCMSY.pdf). The default argument in 
the cmsy function is to calculate a constrained prior (finalbio <- if(ct[nyr]/max(ct) > 0.5) {c
(0.3,0.7)} else {c(0.01,0.4)}). To calculate the uniform prior substitute: finalbio <- c(0.01,0.7).

The uniform model tends to result in much higher scores than the constrained model. Also, there is 
something that looks like an artifact when catch decreases or remains constant for a series of years 
where the b/bmsy scores increase exponentially.  To control for that, we use the 5 year running mean to 
get the final b/bmsy scores.

Images of catch/uniform/constrained data are found here:
https://github.com/OHI-Science/ohiprep/blob/master/globalprep/SAUP_FIS/tmp/bbmsy_figs/)


Inputs: Github:globalprep/SAUP_FIS/v2015/tmp/b_bmsy_v16072015.csv'
 Outputs: Github:globalprep/SAUP_FIS/v2015/tmp/b_bmsy_scores_uniform.csv
Github:globalprep/SAUP_FIS/v2015/tmp/b_bmsy_scores_constrained.csv 

4. Stock_resilience.R
To determine whether to use the uniform or the constrained prior for each stock we calculated a stock 
"resilience" score based on the governance scores for the FAO (rfmo protectiveness) and the countries 
with catch in that particular FAO (Mora scores).  

For each stock (FAO/taxa combination), the proportion of catch occurring within each country/high seas 
region is multiplied by the country/high seas governance scores and these values are then summed to get 
the stock's overall resilience score that will range between 0-1.  The stock resilience score is used 
to determine whether to use the B/bmsy calculated using the uniform or constrained prior.  Last year, 
we used the uniform b/bmsy score for stocks with a resilience score >=0.6.  

For catch that occurs within country eezs, the proportion of catch occuring in each country is 
multiplied by the country's Mora resilience score.  For the proportion of the stock's catch occurring 
within the high seas region, the following calculation is performed:
1) determine whether the species is managed by any of the RFMO's within the FAO
2) determine the proportion of area protected by each rfmo within an FAO and then multiply the rfmo's 
effectiveness score by this proportion. For example, if an rfmo has a score of 0.8 but only covers 50% 
of the FAO region the protectivess will by 0.4.
3) when a species is protected by multiple rfmos within an FAO select the most protective score (this 
seems like the most conservative option because the rfmos often overlap)
4) the proportion of catch that occurs in the high seas for a taxa that is not protected by an rfmo 
gets a zero score
  
  Outputs:globalprep/SAUP_FIS/v2015/tmp/stock_resil_06cutoff_2015.csv

5. RAMdataPrep.R
 Prepares the b/bmsy values from the RAM data (based on stock assessments)
- The stocks in the RAM data were matched to the TaxonIDs and the SAUP/FAO regions in the SAUP 
database
-  The range of years of b/bmsy data varies for each stock. We only included stocks with 6 years of 
b/bmsy values collected since 2002. We scaled the year of data collection such that the most recent 
b/bmsy value would correspond to the 2010 catch data.
 Number of stocks with RAM data: 195
 The years of ram data that correspond to the 2010 catch data ranges from 2007 to 2015
-  We supplemented the RAM data with ICCAT data for: Skipjack tuna Western Atlantic and Skipjack tuna 
Eastern Atlantic.
- When a saup/fao region had multiple stocks of the same species the b/bmsy values were averaged
- The SAUP regions were converted to OHI rgns.  In many cases, an ohi region is comprised of >1 SAUP 
regions.  For these, we took a weighted average based on the mean catch within each SAUP region.

Inputs:
  globalprep/SAUP_FIS/v2015/raw/TaxonEEZ_FAO.csv,
  globalprep/SAUP_FIS/v2015/raw/RLSADBv3_timeseries_values_views.csv
  globalprep/SAUP_FIS/v2015/tmp/Skipjack_Bmsy.csv
  globalprep/SAUP_FIS/v2015/tmp/mean_catch_saup_fao.csv

Outputs: 
globalprep/SAUP_FIS/v2015/tmp/RAM_fao_ohi.csv
  
  

6. b_bmsy_tb_prep.R
Final preparation of the b/bmsy data for toolbox
- selecting appropriate b/bmsy score (from cmsy calculations) based on stock resilience calculations
- adding in ohi-regions to cmsy b/bmsy data
- replace cmsy data with RAM data when possible
- formatting for toolbox
Inputs:
globalprep/SAUP_FIS/v2015/tmp/stock_resil_06cutoff_2015.csv
globalprep/SAUP_FIS/v2015/tmp/b_bmsy_scores_uniform_mean5yrs.csv
globalprep/SAUP_FIS/v2015/tmp/b_bmsy_scores_constrained_mean5yrs.csv
globalprep/SAUP_FIS/v2015/tmp/RAM_fao_ohi.csv

Outputs:
globalprep/SAUP_FIS/v2015/data/fnk_fis_b_bmsy_lyr.csv