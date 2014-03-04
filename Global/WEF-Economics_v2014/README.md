README.md
========================================================

Original data downloaded as a pdf from [World Economic Forum](http://www.weforum.org/issues/global-competitiveness) at this [link] (http://www3.weforum.org/docs/WEF_GlobalCompetitivenessReport_2013-14.pdf). This pdf is stored on Neptune on data_edit: git-annex/Global/WEF-Economics_v2014/WEF_GlobalCompetitivenessReport_2013-14.pdf

To create .csv files from these pdf tables, follow these steps 
TODO: make a python script for this:

* To copy data, use Adobe Acrobat Pro 9.0 
* Table was copied into WEF_GCI_2013-2014_Table3.txt and WEF_TTCI_2013-2014_Table1.txt 
* Table saved as WEF_GCI_2013-2014_Table3_reformatted.csv and WEF_TTCI_2013-2014_Table1_reformatted.csv with TextWrangler: 
  + add quotes around countries with commas (~5 of these) 
  + add commas to make it csv: 
    - search '(\w) (\d)' replace '\1,\2'; 
    - search '(\d) (\d)' replace '\1,\2'; 
    - search ' (n\/a)' replace ',NA' 
    - simplify/rename headers with commas

WEF_GCI_2013-2014_Table3_reformatted.csv is processed further by 


Example content
When you click the **Knit HTML** button a web page will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:


## easy automatic output of table
```{r}
summary(cars)
head(cars)
```

## explicit pretty table
```{r, echo=FALSE, results='asis'}
kable(head(cars))
```

```
##      speed           dist    
##  Min.   : 4.0   Min.   :  2  
##  1st Qu.:12.0   1st Qu.: 26  
##  Median :15.0   Median : 36  
##  Mean   :15.4   Mean   : 43  
##  3rd Qu.:19.0   3rd Qu.: 56  
##  Max.   :25.0   Max.   :120
```

```r
head(cars)
```

```
##   speed dist
## 1     4    2
## 2     4   10
## 3     7    4
## 4     7   22
## 5     8   16
## 6     9   10
```


## explicit pretty table
|  speed|  dist|
|------:|-----:|
|      4|     2|
|      4|    10|
|      7|     4|
|      7|    22|
|      8|    16|
|      9|    10|



You can also embed plots, for example:


```r
plot(cars)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 


