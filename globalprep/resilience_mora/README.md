Source: GL-Scripps-Mora2009, GL-NCEAS-ArtisanalFishing

We use the Mora data (Oaf) from the Artisanal Fishing goal, and 
we used georegional means for 18 regions.

    "Regions with nodata, filled with georegion average"
     id  |                 label                  
    -----+----------------------------------------
      32 | Djibouti
      38 | Bahrain
      43 | Gibraltar
      67 | French Southern Ocean Territories
      73 | Republic of the Congo
      79 | Clipperton Island
     108 | British Pacific Territories (Pitcairn)
     110 | Jarvis Island
     114 | USA Pacific Uninhabited Territories
     136 | Monaco
     137 | Serbia and Montenegro
     139 | Slovenia
     143 | Iraq
     157 | Vietnam
     158 | Singapore
     164 | Jordan
     171 | East Timor
     172 | Bosnia and Herzegovina
    (18 rows)


We computed the resilience as $1 - mean(x_1, ..., x_6)$ where $x_i$ is the
score for 6 Mora et al (2009) indicators ('fishing-effort', 'foreign-fishing',
'implementation-capability', 'policy-transparency', 'scientific-robustness',
and 'subsidies'). In 28 out of 193 countries, there were multiple sets of
values for these indicators, so we took the mean for each category. We
computed weighted averages, weighted by country area, to aggregate into our
reporting regions. We had actuals data for 162 regions, and georegional means
for 9 regions. Any region with no data is given resilience=0.

    "Regions with nodata, filled with georegion average"
     id  |                 label                  
    -----+----------------------------------------
      67 | French Southern Ocean Territories
      79 | Clipperton Island
     108 | British Pacific Territories (Pitcairn)
     110 | Jarvis Island
     114 | USA Pacific Uninhabited Territories
     136 | Monaco
     158 | Singapore
     171 | East Timor
     172 | Bosnia and Herzegovina
    (9 rows)
