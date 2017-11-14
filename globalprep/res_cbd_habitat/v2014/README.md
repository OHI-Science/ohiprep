## Ocean Health Index: Convention on Biological Diversity

This folder has data derived from country responses to the Convention on Biological Diversity (CBD) Third National Report. These data are used for several resilience layers related to habitat protection.

The folders in this file include the metadata, R scripts, and data for each assessement year (i.e., the year the assessment was conducted).  The most current year represents the best available data and methods, and previous years are maintained for archival purposes.

Our [data managment SOP](https://rawgit.com/OHI-Science/ohiprep/master/src/dataOrganization_SOP.html) describes how we manage OHI global data, including a description of the file structure.

Please see our [citation policy](http://ohi-science.org/citation-policy/) if you use OHI data or methods.

Thank you!


Notes from MRF (Feb 2 2016)

Moved from ohiprep/Global/GL-Resilience_Fish_Hab_2014a (now deleted)
Resilience_2014a.R: describes how the fishing_v1 and habitat_combo resilience layers
are created using a combination of data (described in HAB_resilience_guide.pdf) (This is no longer relevant because we no longer use composite resilience data.)

Other potentially relevant files include:
neptune: model/GL-CBD-Survey
neptune: model/GL-NCEAS-Resilience_Mora

****

Source: GL-CBD-Survey 

For 147 regions, we use actuals, and geographical means weighted by
country area for the remaining regions.

We categorize the CBD questions into 6 different categories as follows:

         category      | question 
    -------------------+----------
     cbd-alien-species | 160b
     cbd-alien-species | 160c
     cbd-alien-species | 160d
     cbd-alien-species | 160e
     cbd-habitat       | 153a
     cbd-habitat       | 153b
     cbd-habitat       | 153c
     cbd-habitat       | 153e
     cbd-habitat       | 153g
     cbd-habitat       | 158a
     cbd-habitat       | 158b
     cbd-habitat       | 158c
     cbd-habitat       | 158f
     cbd-habitat       | 158g
     cbd-habitat       | 158h
     cbd-mariculture   | 158d
     cbd-mariculture   | 159a
     cbd-mariculture   | 159b
     cbd-mariculture   | 159c
     cbd-mariculture   | 159e
     cbd-mariculture   | 159f
     cbd-mariculture   | 159g
     cbd-mariculture   | 159h
     cbd-mariculture   | 159i
     cbd-mariculture   | 159j
     cbd-mariculture   | 159k
     cbd-mariculture   | 159l
     cbd-tourism       | 79x
     cbd-tourism       | 80x
     cbd-tourism       | 82x
     cbd-water         | 153d
     cbd-water         | 153f
    (32 rows)


For the CBD signatories we designated 51 territories with the signatory
status of their soverign nation.

     iso3166_from | iso3166_to 
    --------------+------------
     ABW          | NLD
     AIA          | GBR
     ALA          | FIN
     ANT          | NLD
     ASM          | USA
     ATF          | FRA
     BES          | NLD
     BLM          | FRA
     BMU          | GBR
     BVT          | NOR
     CCK          | AUS
     CUW          | NLD
     CXR          | AUS
     CYM          | GBR
     FLK          | GBR
     FRO          | DNK
     GGY          | GBR
     GIB          | GBR
     GLP          | FRA
     GRL          | DNK
     GUF          | FRA
     GUM          | USA
     HKG          | CHN
     HMD          | AUS
     IMN          | GBR
     IOT          | GBR
     JEY          | GBR
     MAC          | CHN
     MAF          | FRA
     MNP          | USA
     MSR          | GBR
     MTQ          | FRA
     MYT          | FRA
     NCL          | FRA
     NFK          | AUS
     PCN          | GBR
     PRI          | USA
     PYF          | FRA
     REU          | FRA
     SGS          | GBR
     SHN          | GBR
     SJM          | NOR
     SPM          | FRA
     SXM          | NLD
     TCA          | GBR
     TKL          | NZL
     TWN          | CHN
     UMI          | USA
     VGB          | GBR
     VIR          | USA
     WLF          | FRA
    (51 rows)


