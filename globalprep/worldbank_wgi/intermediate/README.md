# README

`wgi_combined_scores_by_country.csv` is an intermediate file created for the Ocean Health Index Global assessments. These data are from the World Bank WGI (World Governance Indicators) data using World Bank Development Indicators API (an `R` package accessed by `install.packages('WDI')`). 

It has the following columns: 

- "country": reporting country
- "year": reporting year
- "score_wgi_scale": mean score of six indicators: 
  - Voice and Accountability
  - Political Stability and Absence of Violence/Terrorism
  - Government Effectiveness
  - Regulatory Quality
  - Rule of Law
  - Control of Corruption
- "score_ohi_scale": score_wgi_scale rescaled between 0 and 1 (score_wgi_scale range was -2.5 and 2.5)


`wgi_combined_scores_by_country.csv` is created by `data_prep_wgi.r` in `ohiprep/globalprep/worldbank_wgi`. See `data_prep_wgi.r` for further details.
