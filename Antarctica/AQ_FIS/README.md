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

# Notes for Toothfish data 

 area 48.4 is open to research and 58.4.3b and 58.4.4a/58.4.4b are closed. This means that the (legal) catch from these areas is not used for food provision. Should they get a 0 or an NA? The low score would highlight a problem, but would ignore that reduced harvest is at least an attempt to remedy the situation.

Note that the year corresponds to the final year of the fishing season (i.e. 2013/2014 is indicated as 2014). 

Values only for areas that don’t correspond to separate OHI reporting regions, e.g. excludes Heard Is, S Georgia Is, Prince Edward Is, Sandwhich, 258510, 258520, 258600, 258700). Although 483 has a large component in the South Georgia island area, it also had a large portion outside this area.  I decided to keep these data even though it includes some of the Island data. 
 
# Notes for Krill
Regarding the reference point, the same target (620,000 tonnes) is assigned to all four regions 48.1, 48.2, 48.3, 48.4. This is an overall target, meaning that the sum of catch across four regions cannot exceed that.However, there are a couple of areas that got a low score that shouldn’t be included because one (48.6, or sp_id 248600) has only experimental fisheries allowed, the other (sp_id 288100) had a small amount of catch as bycatch, in a single year, in an area where krill fishing is not permitted. This too should not be penalized for food provision.

# Notes for Mackerel icefish
Mackerel icefish: from the assessment reports it’s clear the fishery is only fished inshore inside EEZs that we exclude from the Antarctica OHI (i.e. South Georgia and Heard Island).


# Notes from Katie:
[It’s worth flagging however, that for the small island EEZs that we exclude, the current fishery assessment is rubbish because it uses bad SAUP data and a bad CMSY model. Using the CCAMLR data and the method we developed for Antarctica, it would be easy to replace these values. An Antarctic expert will notice, and it would be consistent with using stock assessment data elsewhere, but it won’t change much in the grand scheme of things. Something to consider for future iterations of Global OHI.]


