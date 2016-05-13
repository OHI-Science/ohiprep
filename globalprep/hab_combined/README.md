##### FYI/Reference habitat data

Currently the habitat data are split into 6 general files:
hab_coral, hab_mangrove, hab_rockyreef, hab_saltmarsh, hab_seagrass, hab_soft_bottom, NSIDC_SeaIce.

If updates are made to any of the habitat layers, they should be saved in these files.  In 2015 we updated sea ice (which is updated every month) and mangrove data (based on an improved version of the data).

After updates are made to the individual habitat files, they must be combined into the "habitat_extent...", "habitat_health...", "habitat_trend..." using this script: Github:ohiprep/globalprep/hab_combined/v2015/CombiningHabitats.R

###### Future: It would be better to call the individual habitat files in the Toolbox and skip the "combining" step.

Once the habitat files are ready, there are some additional files that need to be created based on these data:

- pressures CP: cp_habitat_extent_rank
- pressures CS: cs_habitat_extent
- pressures HAB: hab_pressence

These are currently created using this script: Github: ohiprep/globalprep/hab_combined/SupplementaryHabitatFiles.R

###### Future: It would be better if these files were created in the respective functions within the toolbox.

SeparatingHabitatData.R is included for reference only (this should never need to be run again).  This separated the single habitat file from the <2015 analysis into the individual habitat layers so they can more easily be updated.

gap_filling.R: This generates the gap-filling data for the habitat: health, extent, trend data