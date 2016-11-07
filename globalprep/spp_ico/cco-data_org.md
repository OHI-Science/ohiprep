organizing the data and such.

General
Git-annex or Github?
- File size info: https://help.github.com/articles/what-is-my-disk-quota/
- Git-annex:
  - advantages: remote, so accessed from any computer.  Can store huge files.  Probably backed up?
  - disadvantages: remote, so slower access than from local drive.  No version control?
- Github:
  - advantages: remote, so accessed from any computer, but pulled to local drive for speed.  Revision control.
  - disadvantages: can only handle small files.
- Decision tree: 
  - files over (say 1 MB?) and collections of files over (say 10 MB?) go into git-annex.
  - generally, raw data sets in git-annex?
  - all scripts in github, and all final data prep outputs go in github (generally small .csvs)
  - intermediate files (.csvs output from scripts during processing, temp files, etc) decide based on likely file size.

cache vs tmp: 
- cache is stuff that is likely to be useful in future years - e.g. scraped data that can be saved and built upon for future years. 
- tmp is stuff that is temporary - you should feel no qualms about dragging everything in here straight to the trash when you’re done.

year-specific folders/directories:
- save these for year-specific data, outputs (intermediate or final), and script archiving. No temp files!

For files shared across other goals etc, consider symbolic or hard links for disk space?
- e.g. region shapefiles and rasters
- links mean the original file can be modified, so nothing that is being modified or saved



base directory: git-annex/globalprep/SpeciesDiversity

raw: raw data files from the original source, not to be modified
- Shape files from IUCN 
  - no year-specific folders, just keep adding new/updated maps into this folder
  - as new shape files are added, use a .csv or .txt to track which files were used/accessed for which assessments.
- Aquamaps data
  - do these get added to, updated annually, etc?
  - if updated, just store latest version? or store deprecated versions too?

cache: use this instead of tmp for long-term storage of generated data, that can be added to each year, rather than recreated from scratch.  Cache implies something you have stored intentionally so you can access it again later.
- iucn_details
- iucn_intersections

tmp: temporary working files, which no one cares about after the process is done.
- work in progress type of stuff; not an important end product for a particular year.
- for intermediate files for data checking and debugging?
- for intermediate files that are slow to regenerate, but not used year-to-year (e.g. not region rasters)
- maybe geodb.gdb, temporary working geodatabase for python analysis - probably don’t need to keep it around, just the outputs (in v201x/intermediate)

regions: shape files, tifs, etc that are likely to get reused year-to-year.

v201x: year-specific information.
- v201x/data (or outputs?): the completely processed info that gets pulled into function.R or whatever.  This might end up on github, because these are probably not immense files.
- v201x/intermediate: all the .csvs and such that are created during the analysis (e.g. spp_iucn_all.csv etc)
— v201x/tmp: shouldn’t exist - if it’s really temporary, don’t keep it! just use the root tmp while you’re working.

Base directory: github/ohiprep/globalprep/SpeciesDiversity
- All current scripts at this level, or in an R folder here.
- Try to make a common master script e.g. data_prep.R, that runs all the processing, or at least outlines the sequence and calls functions or sources scripts from auxiliary R files.

similar structure as git-annex.
raw: anything small that is appropriate for github? 
- for other goals, maybe small datasets can go directly on github rather than network drive. Not for SPP probs.
- maybe here a readme.md pointing towards files in git-annex. For goals with some data on git-annex and some on github, a listing of all files and locations would be helpful..
- maybe here some information on which raw files were used for which assessments
  - e.g. all the IUCN shapefiles are in the raw directory, but some were added more recently than others
  - so create a .txt or .csv for each year’s assessment, indicating which species groups were included that year.

cache: small files appropriate for github - 
- similar to github raw.  Logs of which iucn_detail/xxxxxxx.htm files were used for each year.

tmp: temporary working files from scripts.

R: other R files likely to be used year to year, incl functions, auxiliary scripts

v201x: as for git-annex.
- v201x/data: final outputs of data prepping - ready for functions.R or whatevs.  These are not likely to be huge files, so keep ‘em on github rather than git-annex.
- v201x/intermediate: any intermediate processing outputs that are not huge?
- v201x/tmp: shouldn’t exist!
- v201x/script_archive: while the assessment is going, all scripts in github base directory, or in R folder there.  After the year is finished, archive the scripts into this folder for future reference.

