### Pathogens

Data is from here: http://www.anta.canterbury.ac.nz/documents/2008-09%20projects%20GCAS/Tarasenko.pdf
and this paper:
Grondahl et al. 2009. Survey of waste water disposal practices at Antarctica research stations. Polar Research: 28: 298-306.

Treatment was scored from 0-3 (0=no treatment; 3 = best treatment), based on the following criteria:

If stations had < 30 people for the annually averaged population but didn't report, they got a 0 score, if they have > 30 but didn't report, they got a 1(stations with >30 people are required to do some treatment). 

Stations that had 0 people reported were assumed to be abandoned, or very transiently visited. 

This gives us 77 bases (which is about right for the correct # of active bases).  

1. The original csv file is: Antarctica_Facilities_OHIscorecalc.csv
2. The shoreline_distance.R file limits the bases to those within 100 km of the shoreline and with >0 population.
   The resulting file is saved as: Antarctica_Facilities_OHIscorecal_final_sites.csv
3. The lat/lon data is converted to ccamlr regions using coordinate_overlay.R
   The resulting file is: pathogen_data.csv
4. The final scores are calculated using scores_calc.R
   The trend calculation assumes that the treatment efficacy has not changed since the station was installed. 
   Which is probably not correct, but is the best information we have.
