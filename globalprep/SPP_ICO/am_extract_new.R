
library(dplyr)
source('src/R/common.R')

my_db <- src_sqlite("my_db.sqlite3", create = T)

library(nycflights13)
flights_sqlite <- copy_to(my_db, flights, temporary = FALSE, indexes = list(
  c("year", "month", "day"), "carrier", "tailnum"))

am_dir <- file.path(dir_neptune_data, 'git-annex/globalprep/_raw_data/aquamaps/v2015/aquamaps_2015_full_dataset_ohi')
am_file_hcaf <- file.path(am_dir, 'hcaf_ohi.sql')
am_file_sp_occur <- file.path(am_dir, 'speciesoccursum_ohi.sql')
am_file_sp_native <- file.path(am_dir, 'hcaf_species_native_ohi.sql')

x <- data.frame('raw' = scan(file = am_file_hcaf,      
                             what = 'character', 
                             nmax = 2000, sep = '\n'))
y <- data.frame('raw' = scan(file = am_file_sp_occur,  
                             what = 'character', 
                             nmax = 2000, sep = '\n'))
z <- data.frame('raw' = scan(file = am_file_sp_native, 
                             what = 'character', 
                             nmax = 2000, sep = '\n'))

### scan file in; maybe a few thousand lines at a time, to keep within memory? 
### use 'CREATE TABLE' to identify column names
###   find the index of 'create table'; add a line; find index of 'records of'; subtract a couple of lines
###   For each of the lines in between, extract column name between '`' marks
###   for each column, extract column type from 'varchar' (character), 'int' (integer, or numeric?), 'tinyint' (logical), 'float' (numeric)
### use 'Records of' to identify start of records
###   from this index on, extract each record - 
###     each line starts with "INSERT INTO (file name) VALUES (" 
###     each line ends with ");" (except maybe last line?)
###     each record is surrounded by ' marks (except null values)
### For each record, eliminate ' marks, str_split by ', ' then rbind with other records
###   coerce column types now, or later? Later seems more efficient.
###   maybe use an lapply for this step?
### Alternative idea: just read into lines, omit ' marks, do tidyr::separate() by ', ' into the specified column names
###   

sql_df <- x %>% mutate(raw = as.character(raw))
cols_to_keep <- 10

hdr_index <- c(which(str_detect(sql_df$raw, 'CREATE TABLE')) + 1, which(str_detect(sql_df$raw, 'ENGINE')) - 1)
hdr_lines <- data.frame('raw' = sql_df[hdr_index[1]:hdr_index[2], ]) %>%
  mutate(raw = tolower(str_trim(raw))) %>%
  filter(!str_detect(raw, '^key')) %>%
  separate(raw, c('var_name', 'var_type'), sep = ' ', extra = 'drop', remove = FALSE) %>%
  mutate(var_name = str_replace_all(var_name, '`', ''),
         var_type = ifelse(str_detect(var_type, 'char'),    'character', var_type),
         var_type = ifelse(str_detect(var_type, 'double'),  'numeric',   var_type),
         var_type = ifelse(str_detect(var_type, 'float'),   'numeric',   var_type),
         var_type = ifelse(str_detect(var_type, 'tinyint'), 'logical',   var_type),
         var_type = ifelse(str_detect(var_type, 'int'),     'integer',   var_type))

tbl_index <- c(first(which(str_detect(sql_df$raw, 'INSERT INTO'))), last(which(str_detect(sql_df$raw, 'INSERT INTO'))))
tbl_lines <- data.frame('raw' = sql_df[tbl_index[1]:tbl_index[2], ]) %>%
  mutate(raw1 = tolower(str_trim(raw)),
         raw1 = str_replace(raw1, 'insert into `.*` values ', ''),
         raw1 = str_replace(raw1, '\\(', ''),
         raw1 = str_replace(raw1, '\\).', ''),
         raw1 = str_replace_all(raw1, "'", '')) %>%
  separate(raw1, into = hdr_lines$var_name[1:cols_to_keep], sep = ', ', convert = TRUE, remove = FALSE, extra = 'drop')
