#' Gapfill using UN georegions
#' 
#' Gapfills data using average of UN georegions
#' 
#' @param df_in dataset with 
#' @param weighting ("none", "area")
#' @param flds_unique unique identifying fields for the dataset, e.g., c("rgn_name", "year")
#' @param keep_fld_name keep original name
#' 
#' @details This function translates name to region id with a lookup.
#'  
#' @keywords ohi
#' @export
name_2_rgn <- function(df_in,   #df_in=empd
                       fld_name      = 'country',
                       flds_unique   = NULL,
                       keep_fld_name = TRUE) {
  ### DETAIL. Return a data.frame (vs add_rgn_id which writes to a csv) 
  ### and perform extra checks, including collapsing on duplicates.
  
  ### Read file of region names/IDs and select rgn_id, rgn_name, and rgn_type
  rgns <- rgn_master %>% 
    dplyr::select(rgn_id = rgn_id_2013, rgn_name = rgn_nam_2013, rgn_type = rgn_typ) %>% 
    dplyr::arrange(rgn_type, rgn_id, rgn_name) %>% 
    dplyr::group_by(rgn_id) %>% 
    dplyr::summarize(rgn_name = first(rgn_name), rgn_type = first(rgn_type)) %>% 
    dplyr::ungroup()
  
  ### attach rgn_synonyms; summarize eliminates duplicate rows (same tmp_name 
  ### and rgn_id) - rgn type not critical?
  syns <- rgn_synonyms %>% 
    dplyr::select(rgn_id = rgn_id_2013, tmp_name = rgn_nam_2013, 
                  tmp_type = rgn_typ)
  
  rgn_syn <- rgns %>% 
    dplyr::select(rgn_id, tmp_name = rgn_name, tmp_type = rgn_type) %>%
    dplyr::bind_rows(syns) %>% 
    dplyr::group_by(tmp_name) %>% 
    dplyr::summarize(rgn_id = first(rgn_id), tmp_type = first(tmp_type)) 
  
  ### create a temp field in the target data frame, for the field that is being combined.
  df_in['tmp_name'] <- df_in[fld_name]
  
  ### replace problematic symbols (accents and such) within target data frame.
  df_in <- df_in %>% 
    dplyr::mutate(tmp_name = str_trim(tmp_name),
                  tmp_name = stringr::str_replace(tmp_name, "^'", ""))
  
  ### combine target data frame with region name data frame;
  ### filter to ones with 'eez' or 'ohi_region' in the type.  
  ### * 'eez' is original OHI rgn_name/rgn_id list;
  ### * 'ohi_region' is from the synonyms.  
  df_in <- df_in %>%
    dplyr::left_join(rgn_syn, by = "tmp_name") 
  
  df_matched <- df_in %>% 
    dplyr::filter(tmp_type %in% c("eez", "ohi_region"))
  
  ### This is the list of countries removed.  Why change tmp_name into a factor? (for table display?)
  df_removed <- df_in %>%
    dplyr::filter(!tmp_type %in% c("eez", "ohi_region") | is.na(tmp_type))
  
  ### countries in df_in_removed with no match - so tmp_type is NA (this is 
  ### why left_join() is used).
  ### Print a table of the non-matches.
  if (sum(is.na(df_removed$tmp_type)) > 0) {
    toprint <- df_removed %>% 
      dplyr::filter(is.na(tmp_type))
    cat("\nThese data were removed for not having any match in the lookup tables:\n")
    print(table(as.character(unique(toprint$tmp_name))))
  }
  
  ### print out the full table of removed names.
  if (sum(!is.na(df_removed$tmp_name)) > 0) {
    toprint <- df_removed %>% 
      dplyr::filter(!is.na(tmp_type))
    cat("\nThese data were removed for not being of the proper rgn_type (eez,ohi_region) or mismatching region names in the lookup tables:\n")
    print(table(select(toprint, tmp_name, tmp_type), useNA = "ifany"))
  }
  
  ### Sanity check of matched df to make sure none have NA rgn_id after the process.
  ### This would be a failure of the region lookups table.  
  df_matched_na <- filter(df_matched, is.na(rgn_id))
  if (nrow(df_matched_na) > 0) {
    cat("\nThese data have a valid tmp_type but no rgn_id:\n")
    print(table(df_matched_na[, c(fld_name, "tmp_type")], useNA = "ifany"))
    stop("FIX region lookups; one or more rows missing rgn_id values.")
  }
  
  ### Drop fld_name column ('country' e.g.) if desired. If kept, 
  ### and == 'rgn_name', rename so it doesn't conflict with new 'rgn_name'
  if(!keep_fld_name) {
    df_matched <- df_matched[ , -which(names(df_in_matched) == fld_name)]
  } else {
    if(fld_name == 'rgn_name') {
      df_matched <- df_matched %>%
        dplyr::rename(rgn_name_orig = rgn_name)
    }
  }
  
  ### Add rgn_name column, ditch the tmp_name and tmp_type column.
  df_out <- df_matched %>%
    dplyr::left_join(rgns %>% 
                       dplyr::select(rgn_id, rgn_name), by='rgn_id') %>%
    dplyr::select(-tmp_name, -tmp_type)
  
  ### Identify duplicated rgn_ids and report out.
  dups_data <- df_out[ , c(flds_unique, 'rgn_id')] 
  i_dupes <- duplicated(dups_data, fromLast = FALSE) | 
    duplicated(dups_data, fromLast = TRUE)
  
  if(sum(i_dupes) > 0) {
    message(sprintf("\nDUPLICATES found. Consider using collapse2rgn to collapse duplicates (function in progress).\n"))
    df_out_dupes <- unique(df_out[i_dupes, fld_name]) 
    print(df_out_dupes)
  } 
  return(df_out)
}
