Tourism and Recreation
===========================
Outputs:
* tr_unemployment.csv
  * rgn_id, year, percent
* tr_sustainability.csv
  * rgn_id, score (no year value - most recent year?)
* tr_jobs_tourism.csv
  * rgn_id, year, count (individuals)
* tr_jobs_total.csv
  * rgn_id, year, count (individuals)

formula:
* E = Ed / (L - (L*U))
* Sr = (S-1)/5
* Xtr = E * Sr
 
* Ed = Direct employment in tourism (tr_jobs_tourism): 
  * Direct and indirect contributions
    * gn_wttc_empd_2013.csv : Direct Contribution To Employment: The number of direct jobs within travel and tourism
    * rgn_wttc_empt_2013.csv : Total Contribution To Employment: The number of jobs generated directly in the Travel and Tourism sector plus the indirect and induced contributions
    * rgn_wttc_gdpt_2013.csv : Total Contribution to GDP: GDP generated directly by the Travel and Tourism sector plus its indirect and induced impacts
  * this has not been gapfilled. We thought it would make more sense to do at the status level.
* L = Total labor force (tr_jobs_total)
  * `v2015/data/wb_rgn_tlf.csv`: Total labor force (count) from World Bank Statistics
* U = Unemployment (tr_unemployment) 
  * `v2015/data/wb_rgn_uem.csv`: Unemployment (percent of total labor force) from World Bank Statistics
* S = Sustainability index (tr_sustainability)
  * `v2015/data/wef_ttci_2015.csv`: TTCI scaled score from World Economic Forum.

* E is tourism / employed
