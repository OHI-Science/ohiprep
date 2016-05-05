### Source this file from data_prep_SPP.Rmd 
### - no need to load packages or common.R

### this should be defined in the top-level script:
### dir_am_data <- file.path(dir_M, 'git-annex/globalprep/_raw_data/aquamaps/d2015')

##############################################=
### Process hcaf .sql file -----
##############################################=
### challenge: not much, really - pretty straightforward.
###   Beyond the first ten columns or so, some rows seem to fall out of
###   alignment - as if some rows have gained an extra value.
### Currently our process does not require these columns, so dropping
###   them solves that problem.  And, saves memory and load time!

am_hcaf_sql <- file.path(dir_am_data, 'aquamaps_2015_full_dataset_ohi/hcaf_ohi.sql')
am_zip <- file.path(dir_am_data, 'aquamaps_2015_full_dataset_ohi.zip')

# am_file_hcaf_csq <- file.path(am_dir_2014, 'hcaf.sql')
am_hcaf_csv <- file.path(dir_am_data, 'csv/hcaf_truncated.csv')

if(!file.exists(am_hcaf_csv) | reload) {
  
  if(!file.exists(am_hcaf_sql) & file.exists(am_zip)) {
    stop('File ', basename(am_zip), ' needs to be extracted to create ', basename(am_hcaf_sql))
  }
  system.time({
    sql_df <- data.frame('raw' = scan(file = am_hcaf_sql,      
                                      what = 'character', 
                                      nmax = -1, sep = '\n'))
    git_prov(am_hcaf_sql, filetype = 'input')
  }) ### on Neptune: just under 30 seconds
  
  system.time({
    sql_df <- sql_df %>% mutate(raw = as.character(raw))
    cols_to_keep <- 10
    
    hdr_index <- c(which(str_detect(sql_df$raw, 'CREATE TABLE')) + 1, which(str_detect(sql_df$raw, 'ENGINE')) - 1)
    hdr_lines <- data.frame('raw' = sql_df[hdr_index[1]:hdr_index[2], ]) %>%
      mutate(raw = tolower(str_trim(raw)),
             raw = str_replace_all(raw, '`', '')) %>%
      filter(!str_detect(raw, '^key')) %>%
      separate(raw, c('var_name', 'var_type'), sep = ' ', extra = 'drop', remove = TRUE)
    tbl_index <- c(first(which(str_detect(sql_df$raw, 'INSERT INTO'))), last(which(str_detect(sql_df$raw, 'INSERT INTO'))))
    hcaf_df <- data.frame('raw' = sql_df[tbl_index[1]:tbl_index[2], ]) %>%
      mutate(raw = tolower(str_trim(raw)),
             raw = str_replace(raw, 'insert into `.*` values ', ''),
             raw = str_replace(raw, '\\(', ''),
             raw = str_replace(raw, '\\).', ''),
             raw = str_replace_all(raw, "'", '')) %>%
      separate(raw, into = hdr_lines$var_name[1:cols_to_keep], sep = ', ', convert = TRUE, remove = TRUE, extra = 'drop')
  }) ### On Neptune: 65.7 seconds with 2014 data; 60.8 for 2015 data
  
  write_csv(hcaf_df, am_hcaf_csv)
  ### hcaf_df <- read_csv(am_hcaf_csv, stringsAsFactors = FALSE)
  
  ### Compare outputs from this script using 2014 MySQL method and R method:
  # hcaf_2014 <- data.table::fread(file.path(am_dir_2014, '../tables/hcaf.csv'), 
  #                                stringsAsFactors = FALSE) %>%
  #   select(CsquareCode:OceanArea)
  # names(hcaf_2014) <- tolower(names(hcaf_2014))
  # setdiff(hcaf_2014, hcaf_df)
  ### Empty data.table (0 rows) of 10 cols: csquarecode,loiczid,nlimit,slimit,wlimit,elimit...
  ### NICE! no difference detected, using this new method against 2014 or 2015 hcaf .sql
  
  rm(hdr_lines, sql_df, tbl_lines, x)
  ### clean up memory before next one!
} else {
  ### no need to load, but get the git_prov info manually for the .sql input (or .zip) and .csv output
  if(file.exists(am_hcaf_sql))
    git_prov(am_hcaf_sql, filetype = 'input')
  else
    git_prov(am_zip, filetype = 'input')
  
  git_prov(am_hcaf_csv, filetype = 'output')
}


##############################################=
### Process speciesoccursum .sql file -----
##############################################=
### Challenge: variations in species names and species IDs.  New species
###   added from previous year; some species IDs seem to have changed for
###   quite a few species (genus/species has new species ID).
### New columns added; 'occurcells' might be a useful filter?

# am_sp_occur_sql <- file.path(am_dir_2014,  'ohi_speciesoccursum.sql')
am_sp_occur_sql <- file.path(dir_am_data,  'aquamaps_2015_full_dataset_ohi/speciesoccursum_ohi.sql')
am_sp_occur_csv <- file.path(dir_am_data, 'csv/speciesoccursum.csv')

if(!file.exists(am_sp_occur_csv) | reload) {

  if(!file.exists(am_sp_occur_sql) & file.exists(am_zip)) {
    stop('File ', basename(am_zip), ' needs to be extracted to create ', basename(am_sp_occur_sql))
  }
  system.time({
    sql_df <- data.frame('raw' = scan(file = am_sp_occur_sql,  
                                      what = 'character', 
                                      nmax = -1, sep = '\n'))
    git_prov(am_sp_occur_sql, filetype = 'input')
  }) ### Neptune: 1.2 seconds for 2014 data (17348 species); 1.5 s for 2015 (22934 species)
  
  
  system.time({
    sql_df <- sql_df %>% mutate(raw = as.character(raw))
    cols_to_keep <- 10
    
    hdr_index <- c(which(str_detect(sql_df$raw, 'CREATE TABLE')) + 1, which(str_detect(sql_df$raw, 'ENGINE')) - 1)
    hdr_lines <- data.frame('raw' = sql_df[hdr_index[1]:hdr_index[2], ]) %>%
      mutate(raw = tolower(str_trim(raw)),
             raw = str_replace_all(raw, '`', '')) %>%
      filter(!str_detect(raw, '^key') & !str_detect(raw, '^primary key')) %>%
      separate(raw, c('var_name', 'var_type'), sep = ' ', extra = 'drop', remove = TRUE)
    tbl_index <- c(first(which(str_detect(sql_df$raw, 'INSERT INTO'))), last(which(str_detect(sql_df$raw, 'INSERT INTO'))))
    sp_occur_df <- data.frame('raw' = sql_df[tbl_index[1]:tbl_index[2], ]) %>%
      mutate(raw = str_replace(raw, 'INSERT INTO `.*` VALUES ', ''),
             raw = str_replace(raw, '\\(', ''),
             raw = str_replace(raw, '\\).', ''),
             raw = str_replace_all(raw, "'", '')) %>%
      separate(raw, into = hdr_lines$var_name, sep = ', ', convert = TRUE, remove = TRUE, extra = 'drop') %>%
      mutate(fbname = str_replace(fbname, '\\\\', "'")) 
    ### This line replaces left-over backslashes with a single quote (apostrophe).
    ### The str_replace_all(raw, "'", '') mutation ditches the "'" in a "\'" combo
    ### Note that this column isn't really used, and apostrophes just muck things up,
    ### but for the purposes of comparing differences, this became an issue...
  }) ### On Neptune: 1.4 seconds with 2014 data; 2.2 s for 2015 data
  
  write_csv(sp_occur_df, am_sp_occur_csv)
  rm(hdr_lines, sql_df, sp_occur_df)
  ### clean up memory before next one!
} else {
  ### no need to load, but get the git_prov info manually for the .sql input (or .zip) and .csv output
  if(file.exists(am_sp_occur_sql))
    git_prov(am_sp_occur_sql, filetype = 'input')
  else
    git_prov(am_zip, filetype = 'input')

    git_prov(am_sp_occur_csv, filetype = 'output')
}

  ### Check method against 2014 data -----
  ### Compare outputs from this script using 2014 MySQL method and R method:
  # sp_occur_2014 <- data.table::fread(file.path(am_dir_2014, '../tables/ohi_speciesoccursum.csv'), 
  #                                stringsAsFactors = FALSE)
  # names(sp_occur_2014) <- tolower(names(sp_occur_2014))
  # sp_occur_2014 <- sp_occur_2014 %>%
  #   select(-v1) %>%
  #   mutate(fbname = ifelse(fbname == '\\N', 'null', fbname),
  #          iucn_code = ifelse(iucn_code == '\\N', 'null', iucn_code),
  #          expert_id = ifelse(expert_id == '\\N', 'null', expert_id)) %>%
  #   as.data.frame()
  # x <- setdiff(sp_occur_2014, sp_occur_df)
  # asdf <- sp_occur_2014[sp_occur_2014$speciesid %in% x$speciesid, ]
  # qwer <- sp_occur_df[sp_occur_df$speciesid %in% x$speciesid, ]
  # setdiff(asdf[, c(1:4, 6:12)], qwer[, c(1:4, 6:12)])
  ### Using 2014 data, comparing the two data sets: some differences:
  ### * mostly due to "\N" characters - thus the fix to fbname in the old code 
  ###   (new method uses 'null', which should be just as good?)
  ### * many problems due to dropped apostrophes - fixed in the code above, though
  ###   prob irrelevant for analysis
  ### * 29 problems due to umlauts and circonflex and such - new method reads fine,
  ###   old method returned odd character strings. Prob irrelevant for analysis.
  ### * 1 problem due to dropped paren at end of family name; prob irrelevant for
  ###   analysis.
  ###        speciesid speccode      genus species  kingdom   phylum    class        order                        family iucn_code expert_id
  ###     1 Hyd-284425    42894 Egmundella superba Animalia Cnidaria Hydrozoa Leptothecata Incertae sedis (Leptothecata)      N.E.      null
  ###
  ### Check spp occur method, compare 2015 to 2014 
  ### In 2015 data, new columns: "reviewed", "iucn_version", "occurcells", "iucn_id" (was "expert_id")
  ### v2014: speciesid | speccode | genus | species | fbname | kingdom | phylum | class | order | family | iucn_code | expert_id
  ### v2015: speciesid | reviewed | speccode | genus | species | fbname | occurcells | kingdom |      
  ###        phylum | class | order | family | iucn_id | iucn_code | iucn_version
  # spp_new <- sp_occur_df %>%
  #   filter(!speciesid %in% sp_occur_2014$speciesid) %>%
  #   mutate(sciname = paste(genus, species, sep = ' '))
  # spp_lost <- sp_occur_2014 %>%
  #   filter(!speciesid %in% sp_occur_df$speciesid) %>%
  #   mutate(sciname = paste(genus, species, sep = ' '))
  # spp_weird <- spp_new %>%
  #   rename(sid2015 = speciesid) %>%
  #   inner_join(spp_lost %>%
  #                select(sid2014 = speciesid, sciname), 
  #              by = 'sciname')
  ### WTF: 2015 has 22889 species; 2014 has 17348, for a difference of 5541 species.
  ### * 10697 speciesid in 2015 that do not occur in 2014 (new)
  ### * 5156 speciesid in 2014 that do not occur in 2015 (dropped)
  ### * This would explain the difference (new - dropped = 5541)
  ### * of these, 3725 match in scientific name (unique).  So really, more like 1816 new species?
  
  # sp_occur_df1 <- sp_occur_df %>% select(-reviewed, -iucn_version, -occurcells, expert_id = iucn_id)
  # for (i in 1:ncol(sp_occur_df1)) { # i = i
  #   x[[i]] <- setdiff(sp_occur_2014[ ,i], sp_occur_df1[ ,i])
  #   cat(sprintf('Column %s differences: %s\n', names(sp_occur_2014)[i], length(x[[i]])))
  # }
  # Column speciesid differences: 5156
  # Column speccode differences: 1113
  # Column genus differences: 324
  # Column species differences: 627
  # Column fbname differences: 419
  # Column kingdom differences: 0
  # Column phylum differences: 1
  # Column class differences: 1
  # Column order differences: 7
  # Column family differences: 58
  # Column iucn_code differences: 9
  # Column expert_id differences: 15
  


##############################################=
### Process hcaf species native .sql file -----
##############################################=
### Challenges: this is a huge file, and chokes the system.
###   Consider breaking into smaller chunks, processing each separately,
###   and appending to a file in sequence.
### Opportunity: ditch CsquareCode entirely! replace it with LOICZID in
###   this step.  According to 2015 hcaf file, each is a unique identifier.
###   But LOICZID is integers rather than characters - should be
###   (a) smaller in memory (each character = 1 byte; so 10 char = 10 bytes;
###       integer = 4 bytes total)
###   (b) easier to work with
###   Is there a reason to work with CsquareCode at all?

# am_sp_native_sql <- file.path(am_dir_2014, 'ohi_hcaf_species_native.sql')
am_sp_native_sql <- file.path(dir_am_data, 'aquamaps_2015_full_dataset_ohi/hcaf_species_native_ohi.sql')
am_sp_native_csv <- file.path(dir_am_data, 'csv/hcaf_sp_native_trunc.csv')

if(!file.exists(am_sp_native_csv) | reload) {
  
  if(!file.exists(am_sp_native_sql) & file.exists(am_zip)) {
    stop('File ', basename(am_zip), ' needs to be extracted to create ', basename(am_sp_native_sql))
  }
  
  system.time({
    nlines_query <- system2(command = 'wc', args = sprintf('-l %s', am_sp_native_sql), stdout = TRUE)
    nlines <- as.integer(unlist(str_split(nlines_query, ' ')))[1]
  }) 
  ### 2014 data: n = 78,168,507 lines, 2.5 seconds
  ### 2015 data: n = 95,260,482 lines, 12.2 seconds? why so slow?
  
  system.time({
    
    csq_to_loiczid <- fread(am_hcaf_csv) %>% 
      select(csquarecode, loiczid) %>%
      data.table(key = 'csquarecode')
    n_records <- 0
    n_window  <- 15e6 ### try 15M line windows for better speed?
    
    for (i in 1:ceiling(nlines/n_window)) { # i = 1
      ### establish window from (i - 1)*n_window + 1 to min(i*n_window, nlines)
      system.time({
        read_skip <- (i - 1) * n_window
        read_end   <- min(i * n_window, nlines)
        message(sprintf('Reading rows %s to %s', read_skip + 1, read_end))
        sql_df <- data.table('raw' = scan(file = am_sp_native_sql,
                                          what = 'character',
                                          skip = read_skip,
                                          nmax = read_end - read_skip, 
                                          sep = '\n'),
                             stringsAsFactors = FALSE)
        git_prov(am_sp_native_sql, filetype = 'input')
      
      })  ### geez, that data.frame call takes forever: 90 sec total for 1M lines, and 
          ###   still comes out factor.
          ### Try data.table? 6.5 sec and it comes out character.  Boo-yah!
          ### Timing gets slow later in the file:
          ###   when i = 1 (start of file), 1M lines = 6.5 seconds
          ###   when i = 30 (mid-file), 1M lines = 158 seconds
          ### Larger windows and fewer skips will reduce this. Balance between memory and reading lines.
    
      if (i == 1) {
        ### determine headers
        hdr_index <- c(which(str_detect(sql_df$raw, 'CREATE TABLE')) + 1, which(str_detect(sql_df$raw, 'ENGINE')) - 1)
        hdr_lines <- data.frame('raw' = sql_df[hdr_index[1]:hdr_index[2], ]) %>%
          mutate(raw = tolower(str_trim(raw)),
                 raw = str_replace_all(raw, '`', '')) %>%
          filter(!str_detect(raw, '^key') & !str_detect(raw, '^primary key')) %>%
          separate(raw, c('var_name', 'var_type'), sep = ' ', extra = 'drop', remove = TRUE)
      }
      ### process all INSERT INTO lines in this window
      tbl_index <- c(first(which(str_detect(sql_df$raw, 'INSERT INTO'))), 
                     last(which(str_detect(sql_df$raw, 'INSERT INTO'))))
      
      message(sprintf('Data in rows %s to %s', (i-1) * n_window + tbl_index[1], (i-1) * n_window + tbl_index[2]))
      
      ### start timing from here
      system.time({
        sp_cells_df <- data.frame('raw' = sql_df[tbl_index[1]:tbl_index[2], ]) %>%
          mutate(raw = str_replace(raw, 'INSERT INTO `.*` VALUES ', ''),
                 raw = str_replace(raw, '\\(', ''),
                 raw = str_replace(raw, '\\).', ''),
                 raw = str_replace_all(raw, "'", '')) %>% ### 13 seconds to here
          separate(raw, into = hdr_lines$var_name[1:3], sep = ', ', convert = TRUE, remove = TRUE, extra = 'drop') %>% ### 15 secs to here
          data.table(key = 'csquarecode') %>%
          left_join(csq_to_loiczid, by = 'csquarecode') %>% 
          select(-csquarecode) ### 22 seconds to here (15 sec with keyed data.table)
      }) ### using keyed data.table nearly eliminates time from join.
    
      ### Save outputs for this window
      if (i == 1) message(sprintf('Headers identified: %s', paste(names(sp_cells_df), collapse = ', ')))
      
      n_records <- n_records + nrow(sp_cells_df)
      message(sprintf('Writing %s records (total = %s records) to:\n  %s', 
              nrow(sp_cells_df), n_records, am_sp_native_csv))
      write.table(sp_cells_df, am_sp_native_csv, sep = ',', qmethod = 'double',
                  row.names = FALSE, col.names = (i == 1), 
                  append = !(i == 1))
      git_prov(am_sp_native_csv, filetype = 'output')
      ### if i = 1, write lines and col names, otherwise append lines and not col names
      ### report out number of lines, and total number
      rm(sp_cells_df, sql_df)
      
    } ### end of for loop
  
  }) 

} else {
  ### no need to load, but get the git_prov info manually for the .sql input (or .zip) and .csv output
  if(file.exists(am_sp_native_sql))
    git_prov(am_sp_native_sql, filetype = 'input')
  else
    git_prov(am_zip, filetype = 'input')
  
  git_prov(am_sp_native_csv, filetype = 'output')
}


### at 1M lines per window, 2014 data, 1 hour for 30M lines.  Smaller window = more line skipping = longer read time
### at 5M lines per window, 2014 data, 6250 seconds on Neptune for full file.  Result: 1.7 GB file.
### at 15M lines per window, 2015 data:

# ### Now to do comparisons?
# x <- fread(am_sp_native_csv) ### reads 1.686 GB in 26 seconds
# x <- x %>% data.table(key = 'loiczid') %>%
#   left_join(data.table(csq_to_loiczid, key = 'loiczid'), by = 'loiczid') %>%
#   select(-loiczid)
# y <- fread(file.path(am_dir_2014, '../tables/ohi_hcaf_species_native.csv')) ### reads 3.3 GB in 3:11
# y <- y %>% select(speciesid = SpeciesID, csquarecode = CsquareCode, probability)
# 
# ### Using anti_join on the two data sets results in a zero-length data.frame 
# ###   so all rows present in both data frames.
# ### Some duplicated rows, but otherwise no differences.
# w <- y[duplicated(y, fromLast = FALSE) | duplicated(y, fromLast = TRUE), ]
# z <- x[duplicated(x, fromLast = FALSE) | duplicated(x, fromLast = TRUE), ]
