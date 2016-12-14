wd = 'N:/model/GL-NCEAS-SpeciesDiversity_v2013a'
setwd(wd)

# order of operations:
#   setup.sh
#   setup_tmp.py
#   ingest_aquamaps.R
#   ingest_iucn.R
#   ingest.py
#   ingest_intserctions.R
#   model.R
#   finish_tmp.py

source('ingest_intersections.R')
source('model.R')

# TODO: move 'D:/best/tmp/GL-NCEAS-SpeciesDiversity_v2013a/spp.db'
#           r'D:\best\tmp\GL-NCEAS-SpeciesDiversity_v2013a\tmp\geodb.gdb'

# TODO: calculate 2012 SPP with 2013 regions
#       D:\best\docs\data\model\GL-NCEAS-SpeciesDiversity\ohi_spp