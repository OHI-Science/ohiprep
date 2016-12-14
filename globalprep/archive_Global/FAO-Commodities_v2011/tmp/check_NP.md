-   check\_NP
    -   load
    -   tonnes
        -   tonnes diffs summary
        -   tonnes diffs head
    -   usd
        -   usd diffs summary
        -   usd diffs head

check\_NP
=========

investigate NP calculated scores; compare raw values as well. June 2014

load
----

    # load libraries
    suppressPackageStartupMessages({
      library(gdata)
      library(ohicore) # devtools::install_github('ohi-science/ohicore') # may require uninstall and reinstall
    })

    # get paths. Rmd files get knitted from current directory.
    # setwd('~/github/ohiprep/Global/FAO-Commodities_v2011/tmp')
    source('../../../src/R/common.R')         # set dir_neptune_data
    source('../../../src/R/ohi_clean_fxns.R') # has functions: cbind_rgn(), sum_na()

    # read in original input layers from FAO
    tonnes_fao = read.csv('../data/FAO-Commodities_v2011_tonnes.csv')
    usd_fao    = read.csv('../data/FAO-Commodities_v2011_usd.csv')

    # read in input layers from layers_global
    tonnes_lyr = read.csv('../data/FAO-Commodities_v2011_tonnes_lyr.csv')
    usd_lyr    = read.csv('../data/FAO-Commodities_v2011_usd_lyr.csv')

    # read in np debug reports
    np_1 = read.csv('../../../../ohi-global/eez2013/reports/debug/eez2013_np_1-harvest_lm-gapfilled_data.csv')
    np_2 = read.csv('../../../../ohi-global/eez2013/reports/debug/eez2013_np_2-rgn-year-product_data.csv')

    # scores
    scores_np = read.csv('scores_eez2012-2013_2014-06-27_vs_2013-10-09.csv')

    ## 1. compare tonnes, usd from layers_global with np1 debug report::: there are differences:: 7834/12011 rows ----
    d_tonnes = 
      join_all(
        list(
          tonnes_fao %>%
            select(rgn_name, rgn_id, product, year, tonnes_fao=tonnes),
          tonnes_lyr %>%
            select(rgn_id, product, year, tonnes_lyr=tonnes),
          np_1 %>%
            select(rgn_id, product, year, tonnes_1=tonnes),
          np_2 %>%
            select(rgn_id, product, year, tonnes_2=tonnes)), 
        by=c('rgn_id','product','year'), type='full') %>%
      mutate(
        tonnes_dif_lyr_fao = tonnes_lyr - tonnes_fao,
        tonnes_dif_1_fao   = tonnes_1   - tonnes_fao,
        tonnes_dif_2_fao   = tonnes_2   - tonnes_fao) %>%
      filter(tonnes_dif_lyr_fao != 0 | tonnes_dif_1_fao != 0 | tonnes_dif_2_fao != 0) %>%
      arrange(desc(abs(tonnes_dif_2_fao)), desc(abs(tonnes_dif_1_fao)), desc(abs(tonnes_dif_lyr_fao)))

    d_usd = 
      join_all(
        list(
          usd_fao %>%
            select(rgn_name, rgn_id, product, year, usd_fao=usd),
          usd_lyr %>%
            select(rgn_id, product, year, usd_lyr=usd),
          np_1 %>%
            select(rgn_id, product, year, usd_1=usd),
          np_2 %>%
            select(rgn_id, product, year, usd_2=usd)), 
        by=c('rgn_id','product','year'), type='full') %>%
      mutate(
        usd_dif_lyr_fao = usd_lyr - usd_fao,
        usd_dif_1_fao   = usd_1   - usd_fao,
        usd_dif_2_fao   = usd_2   - usd_fao) %>%
      filter(usd_dif_lyr_fao != 0 | usd_dif_1_fao != 0 | usd_dif_2_fao != 0) %>%
      arrange(desc(abs(usd_dif_2_fao)), desc(abs(usd_dif_1_fao)), desc(abs(usd_dif_lyr_fao)))

tonnes
------

### tonnes diffs summary

<table>
<thead>
<tr class="header">
<th align="left"></th>
<th align="left">tonnes_dif_lyr_fao</th>
<th align="left">tonnes_dif_1_fao</th>
<th align="left">tonnes_dif_2_fao</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left"></td>
<td align="left">Min. :0</td>
<td align="left">Min. :-211430</td>
<td align="left">Min. :-211430</td>
</tr>
<tr class="even">
<td align="left"></td>
<td align="left">1st Qu.:0</td>
<td align="left">1st Qu.: -24</td>
<td align="left">1st Qu.: -28</td>
</tr>
<tr class="odd">
<td align="left"></td>
<td align="left">Median :0</td>
<td align="left">Median : 0</td>
<td align="left">Median : 0</td>
</tr>
<tr class="even">
<td align="left"></td>
<td align="left">Mean :0</td>
<td align="left">Mean : -118</td>
<td align="left">Mean : -127</td>
</tr>
<tr class="odd">
<td align="left"></td>
<td align="left">3rd Qu.:0</td>
<td align="left">3rd Qu.: 25</td>
<td align="left">3rd Qu.: 27</td>
</tr>
<tr class="even">
<td align="left"></td>
<td align="left">Max. :0</td>
<td align="left">Max. : 138436</td>
<td align="left">Max. : 138436</td>
</tr>
<tr class="odd">
<td align="left"></td>
<td align="left">NA</td>
<td align="left">NA</td>
<td align="left">NA's :574</td>
</tr>
</tbody>
</table>

### tonnes diffs head

<table>
<thead>
<tr class="header">
<th align="left">rgn_name</th>
<th align="right">rgn_id</th>
<th align="left">product</th>
<th align="right">year</th>
<th align="right">tonnes_fao</th>
<th align="right">tonnes_lyr</th>
<th align="right">tonnes_1</th>
<th align="right">tonnes_2</th>
<th align="right">tonnes_dif_lyr_fao</th>
<th align="right">tonnes_dif_1_fao</th>
<th align="right">tonnes_dif_2_fao</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">Peru</td>
<td align="right">138</td>
<td align="left">fish_oil</td>
<td align="right">2000</td>
<td align="right">456448</td>
<td align="right">456448</td>
<td align="right">245018</td>
<td align="right">245018</td>
<td align="right">0</td>
<td align="right">-211430</td>
<td align="right">-211430</td>
</tr>
<tr class="even">
<td align="left">Peru</td>
<td align="right">138</td>
<td align="left">fish_oil</td>
<td align="right">1994</td>
<td align="right">285578</td>
<td align="right">285578</td>
<td align="right">115932</td>
<td align="right">115932</td>
<td align="right">0</td>
<td align="right">-169646</td>
<td align="right">-169646</td>
</tr>
<tr class="odd">
<td align="left">Peru</td>
<td align="right">138</td>
<td align="left">fish_oil</td>
<td align="right">1998</td>
<td align="right">35439</td>
<td align="right">35439</td>
<td align="right">173875</td>
<td align="right">173875</td>
<td align="right">0</td>
<td align="right">138436</td>
<td align="right">138436</td>
</tr>
<tr class="even">
<td align="left">Peru</td>
<td align="right">138</td>
<td align="left">fish_oil</td>
<td align="right">2002</td>
<td align="right">161558</td>
<td align="right">161558</td>
<td align="right">298166</td>
<td align="right">298166</td>
<td align="right">0</td>
<td align="right">136608</td>
<td align="right">136608</td>
</tr>
<tr class="odd">
<td align="left">Japan</td>
<td align="right">210</td>
<td align="left">fish_oil</td>
<td align="right">1984</td>
<td align="right">331974</td>
<td align="right">331974</td>
<td align="right">228301</td>
<td align="right">228301</td>
<td align="right">0</td>
<td align="right">-103673</td>
<td align="right">-103673</td>
</tr>
<tr class="even">
<td align="left">Japan</td>
<td align="right">210</td>
<td align="left">fish_oil</td>
<td align="right">1988</td>
<td align="right">347443</td>
<td align="right">347443</td>
<td align="right">245005</td>
<td align="right">245005</td>
<td align="right">0</td>
<td align="right">-102438</td>
<td align="right">-102438</td>
</tr>
</tbody>
</table>

usd
---

### usd diffs summary

<table>
<thead>
<tr class="header">
<th align="left"></th>
<th align="left">usd_dif_lyr_fao</th>
<th align="left">usd_dif_1_fao</th>
<th align="left">usd_dif_2_fao</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left"></td>
<td align="left">Min. :0</td>
<td align="left">Min. :-309765</td>
<td align="left">Min. :-309765</td>
</tr>
<tr class="even">
<td align="left"></td>
<td align="left">1st Qu.:0</td>
<td align="left">1st Qu.: -99</td>
<td align="left">1st Qu.: -116</td>
</tr>
<tr class="odd">
<td align="left"></td>
<td align="left">Median :0</td>
<td align="left">Median : 0</td>
<td align="left">Median : 0</td>
</tr>
<tr class="even">
<td align="left"></td>
<td align="left">Mean :0</td>
<td align="left">Mean : -338</td>
<td align="left">Mean : -369</td>
</tr>
<tr class="odd">
<td align="left"></td>
<td align="left">3rd Qu.:0</td>
<td align="left">3rd Qu.: 40</td>
<td align="left">3rd Qu.: 43</td>
</tr>
<tr class="even">
<td align="left"></td>
<td align="left">Max. :0</td>
<td align="left">Max. : 105825</td>
<td align="left">Max. : 105825</td>
</tr>
<tr class="odd">
<td align="left"></td>
<td align="left">NA</td>
<td align="left">NA</td>
<td align="left">NA's :706</td>
</tr>
</tbody>
</table>

### usd diffs head

<table>
<thead>
<tr class="header">
<th align="left">rgn_name</th>
<th align="right">rgn_id</th>
<th align="left">product</th>
<th align="right">year</th>
<th align="right">usd_fao</th>
<th align="right">usd_lyr</th>
<th align="right">usd_1</th>
<th align="right">usd_2</th>
<th align="right">usd_dif_lyr_fao</th>
<th align="right">usd_dif_1_fao</th>
<th align="right">usd_dif_2_fao</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left">Taiwan</td>
<td align="right">14</td>
<td align="left">seaweeds</td>
<td align="right">1989</td>
<td align="right">416611</td>
<td align="right">416611</td>
<td align="right">106846</td>
<td align="right">106846</td>
<td align="right">0</td>
<td align="right">-309765</td>
<td align="right">-309765</td>
</tr>
<tr class="even">
<td align="left">Peru</td>
<td align="right">138</td>
<td align="left">fish_oil</td>
<td align="right">2008</td>
<td align="right">385036</td>
<td align="right">385036</td>
<td align="right">246667</td>
<td align="right">246667</td>
<td align="right">0</td>
<td align="right">-138369</td>
<td align="right">-138369</td>
</tr>
<tr class="odd">
<td align="left">Taiwan</td>
<td align="right">14</td>
<td align="left">seaweeds</td>
<td align="right">1990</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">105825</td>
<td align="right">105825</td>
<td align="right">0</td>
<td align="right">105825</td>
<td align="right">105825</td>
</tr>
<tr class="even">
<td align="left">Taiwan</td>
<td align="right">14</td>
<td align="left">seaweeds</td>
<td align="right">1991</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">104707</td>
<td align="right">104707</td>
<td align="right">0</td>
<td align="right">104707</td>
<td align="right">104707</td>
</tr>
<tr class="odd">
<td align="left">Taiwan</td>
<td align="right">14</td>
<td align="left">seaweeds</td>
<td align="right">1992</td>
<td align="right">0</td>
<td align="right">0</td>
<td align="right">104153</td>
<td align="right">104153</td>
<td align="right">0</td>
<td align="right">104153</td>
<td align="right">104153</td>
</tr>
<tr class="even">
<td align="left">Denmark</td>
<td align="right">175</td>
<td align="left">fish_oil</td>
<td align="right">2008</td>
<td align="right">203395</td>
<td align="right">203395</td>
<td align="right">135410</td>
<td align="right">135410</td>
<td align="right">0</td>
<td align="right">-67986</td>
<td align="right">-67986</td>
</tr>
</tbody>
</table>
