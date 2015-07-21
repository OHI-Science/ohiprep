######################################################
## Sequence to run the scripts to calculate FIS data
######################################################

1. CleanRawData.R 
  cleans the data from saup_ohi
  - sums the repeated data from SAUP
 Inputs:
Outputs:
  prepares catch data for cmsy code to calculate b/bmsy values 
 - summarizes by taxon/fao (assumed stocks)
 Inputs:
   Outputs:
   
2. meanCatch.R
 calculates mean catch 
 - adds zero catch 
 - converts from saup to ohi regions
 - calculates mean catch
 Inputs: 
 Outputs: 
   
3. bbmsy_cmsy.R
 calculates b/bmsy scores using uniform and constrained priors
 Inputs:
 Outputs: 
 
4. RAMdataPrep.R
 Prepares the b/bmsy values from the RAM data (based on stock assessments)
 - links RAM data to SAUP data
 
Inputs:
  Outputs:
  
  
5. Stock_resilience.R
  Calculates stock resilience scores based on governance of countries catching these stocks
  
  Inputs:
  Outputs:

6. b_bmsy_tb_prep.R
Final preparation of the b/bmsy data for toolbox
- selecting appropriate b/bmsy score (from cmsy calculations) based on stock resilience calculations
- merging in RAM data
- formatting for toolbox
Inputs:
Outputs: