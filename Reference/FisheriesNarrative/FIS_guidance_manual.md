---
output: pdf_document
---
### Preparing the fisheries sub-goal 

Here is some background information about how to prepare fisheries data layers for the Toolbox.

**Data layers used by the Toolbox:**

* `fis_b_bmsy`
* `fis_meancatch`
* `fis_proparea_saup2rgn`
* `fp_wildcaught_weight`

#### Description of data layers

`fis_b_bmsy`

* *for species*: B/Bmsy estimate (either from formal stock assessment, or from a data-poor method such as CMSY)    
* *for genus/family/broader taxa*: the toolbox will use median B/Bmsy from species in that region + a penalty for not reporting at species level. In order for the code to assign the correct penalty, the taxa need to include a numerical code of 6 digits, where the first digit behaves like an ISSCAAP code (the standardized species codes used by FAO): 6 means species, 5 means genus, 4 to 1 are increasingly broad taxonomic groups    
* *data source (for CMSY)*: catch time-series (at least 10 years of catch >0), species resilience (if available)  

**Example data:**

|fao_id |taxon_name     |year |b_bmsy   |
|:------|:--------------|:----|:--------|
|51     |Ablennes hians |1985 |1.112412 |
|51     |Ablennes hians |1986 |1.222996 |
|51     |Ablennes hians |1987 |1.371058 |


NOTE: if a species that is caught in different sub-regions belongs to the same population, you don't want to split the catch among sub-regions, instead, you want to sum catch across all sub-regions, so you can calculate B/Bmsy for the whole population. For the global analysis we grouped all species catch by FAO major fishing area (www.fao.org/fishery/area/search/en), indicated in the column *fao_id*, assuming that all species caught within the same FAO area belonged to the same stock, while we assumed that the same species, if caught in a different fishing area, belonged to a separate stock.  
Use *fao_id* as an identifier that separates different fisheries 'stocks' belonging to the same species.   
If you don't have multiple stocks in your study area, set all *fao_id* = 1.  

`fis_meancatch`:

* average catch across all years, per species, per region  
* *data source*: catch time-series (at least 10 years of catch >0), with a unique identifier for each population that you want to assess separately   
 
**Example data:**

|fao_saup_id |taxon_name_key             |year |mean_catch  |
|:-----------|:--------------------------|:----|:-----------|
|37_8        |Aristeus antennatus_690051 |2014 |14.24398116 |
|37_8        |Atherinidae_400218         |2014 |27.30120156 |
|37_8        |Balistes capriscus_607327  |2014 |3.247883895 |

The *taxon_name_key* column indicates the name of the species (e.g. Aristeus antennatus) and its 'taxonkey'. The taxonkey is a 6 digit numeric code used by the Sea Around Us Project, modified from FAO codes. The important element of this code is the first digit, because it reflects the taxonomic level (6=species, 5=genus, 4=family, etc.) of the reported catch.The toolbox uses this first digit to assign a score to all catch that was not reported at species level, taking the median of the B/Bmsy of assessed species, and adding a penalty that is increasingly strong for coarser taxa.    

`fis_proparea_saup2rgn`:  

* a conversion file that, for each region for which catch is reported, tells us what proportion of that region falls within each of the final OHI reporting regions.   

**Example data:**

| saup_id| rgn_id| prop_area|
|-------:|------:|---------:|
|     166|      1|       1.0|
|     162|      2|       1.0|
|     574|      3|       0.7|
|      37|      4|       0.8|

**Specific instances:**

 *only if catch is reported for different regions than the ones used for the OHI assessment:* this should be calculated using spatial analyses of overlap of the spatial units at which catch is reported with the spatial units at which the OHI assessment will be reported. The global data was reported by subregions (*saup_id*) and in some cases multiple subregions were part of the same, larger EEZ. Since for OHI we wanted results by EEZ (*rgn_id*), in those cases we needed to combine results from the subregions to get the final score, based on their size relative to the total EEZ size (*prop_area*).   
 *If catch is reported for the same areas for which OHI is calculated:* then all the *prop_area* are = 1.   
 *If catch is reported for the whole area of the assessment, but you want to calculate a separate OHI score for different sub-regions:* for each OHI reporting region (*rgn_id*) you'll repeat the same region in the *saup_id* column, and *prop_area* will be =1. This effectively means all the reporting regions will get assigned 100% of the catch and will have the same final stastus and trend score for the fisheries goal (but may have different pressures and resilience scores, if those layers are different in each sub-region).  

`fp_wildcaught_weight`: 

*only needed if there is mariculture*: for each region, this represents the relative proportion of catch coming from wild caught fisheries versus mariculture. The layer is used to weight how much the fisheries score influences the final food provision score, the higher the fisheries catch, the more the food provision score will reflect the fisheries score, and vice-versa if mariculture has a higher catch.       
(NOTE that, before all mariculture harvest from all species gets summed, the mariculture harvest for each species is smoothed and then multiplied by the resilience score).     

#### Running CMSY model

**Sample data to run CMSY:**

|id |stock_id                    |res    |ct          |yr   |
|:--|:---------------------------|:------|:-----------|:----|
|6  |Acanthistius brasilianus_41 |Medium |100         |1950 |
|23 |Acanthurus dussumieri_61    |       |0.059250269 |1950 |
|24 |Acanthurus dussumieri_71    |       |0.190749971 |1950 |
|25 |Acanthurus lineatus_61      |Low    |12.74821966 |1950 |

The current CMSY script produces an output that looks something like this (split into 2 tables): 

|stock_id          |convergence |effective_sample_size |yr   |b_bmsy   |b_bmsyUpper |
|:-----------------|:-----------|:---------------------|:----|:--------|:-----------|
|Ablennes hians_51 |SC          |30974                 |1985 |1.112412 |1.8         |
|Ablennes hians_51 |SC          |30974                 |1986 |1.222996 |1.768895    |

|stock_id          |yr   |b_bmsyLower |b_bmsyiq25 |b_bmsyiq75 |b_bmsyGM |b_bmsyMed |
|:-----------------|:----|:-----------|:----------|:----------|:--------|:---------|
|Ablennes hians_51 |1985 |1           |1          |1          |1.093932 |1         |
|Ablennes hians_51 |1986 |1.014688    |1.075699   |1.298437   |1.209005 |1.160329  |

where *stock_id* is the unique identifier for each stock that was used in the input file, *convergence* indicates whether the model converged and how strongly ('SC' = strong convergence), *effective_sample_size* reports the number of iterations used, *yr* = year, b_bmsy = B/Bmsy for the corresponding year (based on the median of all the estimated values: reccomended),  b_bmsyUpper = B/Bmsy at the upper 95% bootstrapped confidence bound, b_bmsyLower = B/Bmsy at the lower 95% bootstrapped confidence bound, b_bmsyiq25 = B/Bmsy at the first quartile, b_bmsyiq75 = B/Bmsy at the third quartile, b_bmsyGM = B/Bmsy based on the geometric mean of estimates, b_bmsyMed = B/Bmsy based on the median of estimates.

**How to:** 

**1. Include resilience in the CMSY code:**

In the CMSY R script, in the PARAMETERS section, replace the following:

```
start_r     <- c(0.01,10)  ## disable this line if you use resilience  
with 

  if(res == "Very low"){
    start_r  <- c(0.015, 0.1)
  } else { 
    if(res == "Low"){
      start_r  <- c(0.05,0.5)
    } else { 
      if(res == "High"){
        start_r  <- c(0.6,1.5)   
      } else {
        start_r  <- c(0.1,1)
      }
    }
  }
```

**2. Make assumptions about fisheries regulations:**

If you assume that fisheries are depleted and there isn't very much fisheries regulation, and you are using the CMSY method to assess B/Bmsy, the original model may work well. If, however, the catch of a species declined because fisheries regulations have closed or limited the fishery, or if a fishery was abandoned for economic reasons (e.g., change in consumer prefereces, market price dynamics, etc.), the model may be too pessimistic and understimate B/Bmsy. In that case it may be best to use a version with a uniform prior on final biomass, instead of the constrained prior.  
The original constrained prior on final biomass is set by this line within the code:  
  
```
finalbio    <- if(ct[nyr]/max(ct) > 0.5) {c(0.3,0.7)} else {c(0.01,0.4)}    
```  

The model uses a uniform prior if that line is replaced with:

```
finalbio    <- c(0.01,0.7) 
```

**3. Use data at a different spatial resolution than the final assessment:**

See notes above for `fis_proparea_saup2rgn`

**4. Calculate B, or Bmsy:**

The CMSY model calculates B/Bmsy as a ratio, it does not estimate the two variables separately.      

**5. Use catch per unit of effort (CPUE):**

The CMSY model requires total biomass removed by fisheries, and uses catch as a proxy for that. It cannot use CPUE. Other more sophisticated stock assessment models use CPUE and may be employed. We do not provide documentation for the use of these other models.     

**6. Use other life-history characteristics, in addition to resilience:**

The CMSY model does not use more detailed information. Other more sophisticated stock assessment models use other life-history traits such as fecundity, larval dispersal, r, K, Lmax, etc., and may be employed. We do not provide documentation for the use of these other models.    

**7. Create a 'taxonkey' to assign to each species:**

When replacing the SAUP_FAO data with your own data, assign a key of 600000 to all species. For all catch that is reported at genus or coarser taxonomic level, you can use the [taxonlookup.csv](https://github.com/OHI-Science/ohiprep/blob/master/Global/NCEAS-Fisheries_2014a/tmp/EEZlookup.csv) to decide what should be the most appropriate taxonkey. Otherwise, you can create your own key, from 100000 to 500000, based on your own judgment of how many species may be reported under that same denomination, and how different they may be (all that matters for the toolbox code is whether the number starts with a 1,2,3,4,5 or 6 with 1 being the coarsest, such as 'miscellaneous marine animals', or 'crustaceans nei'). 

#### Resources

Martell, S & Froese, R (2013) "A simple method for estimating MSY from catch and resilience". *Fish and Fisheries*, DOI: 10.1111/j.1467-2979.2012.00485.x. [Downloadable here](http://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=2&ved=0CCkQFjAB&url=http%3A%2F%2Fwww.iotc.org%2Fsites%2Fdefault%2Ffiles%2Fdocuments%2F2013%2F06%2FIOTC-2013-WPNT03-INF01%2520-%2520Martell%2520%2526%2520Froese%25202012.pdf&ei=PXryU6TtGY3goATglYHoDA&usg=AFQjCNE-S0T1B7B_l7rUYaNNLxsUDguDaQ&bvm=bv.73231344,d.cGU)   
     
Rosenberg, A.A., Fogarty, M.J., Cooper, A.B., Dickey-Collas, M., Fulton, E.A., Gutiérrez, N.L., Hyde, K.J.W., Kleisner, K.M., Kristiansen, T., Longo, C., Minte-Vera, C., Minto, C., Mosqueira, I., Chato Osio, G., Ovando, D., Selig, E.R., Thorson, J.T. & Ye, Y. (2014) Developing new approaches to global stock status assessment and fishery production potential of the seas. *FAO Fisheries and Aquaculture Circular No. 1086*. Rome, FAO. 175 pp. [Downloadable here](http://www.fao.org/docrep/019/i3491e/i3491e.pdf)