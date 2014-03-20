NCEAS-Regions_v2014
===================

## Description
The OHI 2014 regions cover the entire earth with non-overlapping regions having the following fields:
* rgn_type, having possible values:
  - eez: exclusive economic zone (EEZ)
  - land: terrestrial land
  - fao: offshore Food & Agriculture Organization (FAO) Major Fishing Areas, with EEZs erased
  - land-noeez: land without any EEZ
  - disputed-eez: disputed EEZ
  - disputed-land: disputed land
* rgn_id: unique identifier (within same rgn_type)
* rgn_name: name for region

## Outputs

The two main outputs are in geographic coordinate system (gcs):
* **sp_gcs***: spatial areas  based on original EEZs v8 and Antarctica CCAMLR subregions. Version 8 of the EEZs introduced [changes](http://www.marineregions.org/files/eez_v8_changes.php) including splitting of EEZs (Guadeloupe and Martinique; Puerto Rico and Virgin Islands of the United States), which got merged back to form the same regions as OHI 2013.

* **rgn_gcs***: OHI regions dissolved on these fields in sp_gcs: rgn_type, rgn_id, rgn_name, rgn_key. Most significantly, the Antarctica CCAMLR regions become a single Antarctica EEZ, and several of the EEZs get merged into a single OHI region. The correspondence between sp and rgn is maintained by [manual_output/sp_rgn_manual.csv](https://github.com/OHI-Science/ohiprep/blob/master/Global/NCEAS-Regions_v2014/manual_output/sp_rgn_manual.csv).

The two formats (and locations) for the outputs are in:
* ***.shp** in `\\neptune\git_annex\Global\NCEAS-Regions_v2014\data\`
* ***_data.csv** in [`data/`](https://github.com/OHI-Science/ohiprep/tree/master/Global/NCEAS-Regions_v2014/data).

![map of outputs](https://raw.githubusercontent.com/OHI-Science/ohiprep/master/Global/NCEAS-Regions_v2014/fig/NCEAS-Regions_v2014_sp_rgn_map.png)

## Inputs
* EEZ, EEZ_land (http://marineregions.org)
* FAO: Food & Agriculture Organization (FAO) Major Fishing Areas, including CCAMLR Antarctica regions (http://www.fao.org/fishery/area/search/en)
* Z: master lookup table to go from EEZ to OHI regions from 2013 regions

## Process
* remove Antarctica from EEZ
* erase EEZ_land from FAO
* dissolve CCAMLR regions in FAO to create Antarctica EEZ
* add 1000 to FAO ids to create FAO rgn_id
* erase EEZ from EEZ_land to get land
* replace some iso3 in land to match EEZ ('MNP++' to 'MNP', 'ABW' to 'AW', 'BES' to 'BQ')
* select out land parts either misidentified ('SHN' for eezs 'ASC', 'TAA') or iso3 is duplicated having several eez_ids
iso3 IN ('SHN','ATF','AUS','BRA','CHL','ECU','ESP','IND','KIR','PRT','TLS','UMI','USA','ZAF')
* associate these land parts with the neighboring EEZs
* create Antarctica land by erasing rest from earth box and dissolving every polygon with a centroid less than 60 degrees latitude
* go through slivers of FAO and holes from earth box erased by the rest and manually associate with legit region
* convert EEZ of Caspian and Black Seas to land
* merge all products and peform checks for overlap and geometry repair


## Antarctica

Note that the EEZ's have been clipped from the original CCAMLR regions as described in the FAO Major Fishing Areas. Here's the original non-overlapping CCAMLR regions for Antarctica and the OHI subset of regions with the EEZs clipped, as well as proportion of original.

![Antarctica CCAMLR regions: original(top), clipped (middle) and percent original (bottom)](fig/NCEAS-Regions_v2014_Antarctica.png)

Some of the regions slightly exceed 100% of the original region. This is presumably related to some strange ArcGIS geodesic area calculation differences, since slivers were not otherwise added to the OHI version.

Made an Antarctic specific set of shapefiles here:

    \\neptune\data_edit\git-annex\Global\NCEAS-Regions_v2014\data

* antarctica_ccamlr_alleez_gcs.shp: CCAMLR regions before being clipped by EEZs
  - area_orig_ -> area_orig_km2
* antarctica_ccamlr_ohi2014_gcs.shp: CCAMLR regions after being clipped by EEZs with the following fields explained:
  - area_orig_ -> area_orig_km2
  - area_km2
  - area_pct_o -> area_pct_orig: area_km2 / area_orig_km2 * 100
  
This limitation of 10 characters for shapefiles is uber lame. I'm looking into using the R and Python packages for this simple data package format.

NCEAS-Regions_v2012
===================

Source: GL-FAO-FisheryAreas, GL-NCEAS-EEZ, GL-NCEAS-Landsea

We created 186 oceanic regions for every cell in our ocean raster, where
regions defined as either an EEZ boundary (n=174) or an FAO Subocean
fishing area (i.e., open ocean) (n=11), or an unclaimed area (n=1), in
that order. Each raster cell has an 8-bit integer code that is a key into
the **ocean_regions.csv** attribute table. 

If the raster cell value is 0, then that is an unclaimed region (ID=186). The
**ocean_region_details.csv** attribute table is provided as a merged version
of the EEZ and Subocean attribute tables with columns for ID, TYPE, LABEL,
ORIG_ID, and COMMENTS. We have 224 countries represented in the EEZ
boundaries, and we generated a data table (**ocean_regions_country.csv**) that
maps an ISO 3166 country code to its requisite EEZ region (31 do not have ISO
country codes so their names are listed as a code).

Due to resolution and data source differences, the EEZ boundary data and
the ocean mask data do not align at the shoreline. So, we implement the
following algorithm for all ocean pixels to close these data gaps:

    1  Use EEZ pixel if available
    2. Else if within 50mi offshore, use a grown EEZ (+50 pixels) pixel if available
    3. Else if outside 50mi offshore, use a subocean pixel if available
    4. Else "unclaimed"

We also made manual corrections to this algorithm where the data gaps
were greater than 50km (~50 pixels), such as in ocean inlets that extend
more than 50 km inland in northeastern Canada.

The merge rules are expressed in GRASS *r.mapcalc* syntax as follows:

    offshore_50mi_eez_p50=\
        if(offshore_50mi && gl_eez_p50,\
            gl_eez_p50,\
            null())

    merge_s50mi_d50mi_p50=\
        if(earth && ocean,\
            if(!isnull(gl_eez_v6),\
                gl_eez_v6,\
                if(!isnull(offshore_50mi_eez_p50),\
                    offshore_50mi_eez_p50,\
                    if(!isnull(suboceans_50mi),\
                        int(1000+"suboceans_50mi"),\
                        0))),\
            null())



We also create a lookup table that lists each region that is a singleton
(contains 1 and only 1 country) with its associated country.  Finally,
we calculate an adjacency matrix where 2 regions are adjacent if they
are within a distance of 10km.

Update v2013a
=============

Conflating previously seperate products for generation of ocean regions.
