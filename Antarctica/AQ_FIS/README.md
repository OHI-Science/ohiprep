Generating B_bmsy data and FIS status/trend for Antarctica
=====================

This goal has been completely overhauled for the 2015 analysis.
Previously, we used the catch-MSY model to estimate B/Bmsy for all
species, except for 3 that had stock assessments from CCAMLR.  

We have since decided that these catch data do not meet the assumptions
needed to apply the catch-MSY model.  Consequently, we are now only
calculating fisheries status using species with stock assessment data.

I deleted all the relevant files (although they are preserved in a Git commit if necessary).  

Here are the species with Stock Assessment data:

Code  |  Name      | Scientific Name
------ | --------- | ---------------
KRI   | Krill      | Euphasia superba
TOA   | Antarctic Toothfish  | Dissostichus mawsoni
ANI   | Mackerel Icefish     | Champsocephalus gunnari
TOP   | Patagonian Toothfish | Dissostichus eleginoides

The catch data is all from: CCAMLR_2015_Statistical_Bulletin_Volume_27
(can be downloaded as Access file from here: 
https://www.ccamlr.org/en/publications/statistical-bulletin-%E2%80%93-archive
here is more information: https://www.ccamlr.org/en/data/statistical-bulletin)

The data were exported as four files by Katie Longo.  Here is her description:

> The latest data is split in 4 text files: 'CDSlandings' has only toothfish catch data ('TOT' = toothfish spp, 'TOP'=Patagonian toothfish, 'TOA'=Antarctic toothfish), 'CDexports' is its meta-data telling you area and year, 'SBcatch' is catch for all other species (only use 'KRI'=krill and 'ANI'=mackerel icefish) and 'SBeffort' is its metadata. This data release is missing some helpful stuff (e.g. year codes, species codes), so I'm also giving you a spreadsheet from the previous year, Bulletin 26, just in case (i.e. up to November 2014). Remember: 'season year', starts on December, and ends on November of the following year (e.g. 2014 fishing season goes from Dicember2014 to November2015). The real calendar date is in 'SplitYear' and 'CalendarMonth', but 'SeasonYear' matches the reference points from stock assessments.

One thing to note is that catch is in kg. (not tonnes, like one might expect!)





