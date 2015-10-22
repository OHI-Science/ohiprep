### used in:
### * whounicef_sanitation/data_prep.R
###   Georegions: 
      # > georegions <- read.csv('../ohi-global/eez2013/layers/rgn_georegions.csv', na.strings='')
      # > head(georegions)
      # rgn_id level georgn_id
      #      1    r2        53
      #      2    r2        53
      #      3    r2        53
      #      4    r2        53
      #      5    r2        54
      #      6    r2        54
      ### after spreading:
      # rgn_id r0 r1 r2
      #      1  1  9 53
      #      2  1  9 53
      #      3  1  9 53
      #      4  1  9 53
      #      5  1  9 54
      #      6  1  9 54
      # georegion_labels <- read.csv('../ohi-global/eez2013/layers/rgn_georegion_labels.csv')
      # > head(georegion_labels)
      # rgn_id level                     label
      #      1    r2 Australia and New Zealand
      #      2    r2 Australia and New Zealand
      #      3    r2 Australia and New Zealand
      #      4    r2 Australia and New Zealand
      #      5    r2                 Melanesia
      #      6    r2                 Melanesia      
      ### after processing:
      # rgn_id r0_label r1_label       r2_label             v_label
      #     28    World   Africa Eastern Africa      Comoro Islands
      #     29    World   Africa Eastern Africa             Mayotte
      #     30    World   Africa Eastern Africa    Glorioso Islands
      #     31    World   Africa Eastern Africa          Seychelles
      #     32    World   Africa Eastern Africa             R_union
      #     33    World   Africa Eastern Africa Juan de Nova Island
### * wef-economics/v2014/data_prep.R (same as above)
### * wef-economics/data_prep_GCI.R (same as above)
### * TourismRecreation/R/process_WorldBank.R (not used)
### * worldbank_wgi/data_prep.R
      # rgn_id  r2  r1  r0 fld_wt
      #      1  16  16  16      0
      #      2  16  16  16      0
      #      3  16  16  16      0
      #      4  16  16  16      0
      #      5 179 179 179      0
      #      6   6   6   6      1
###   which came from: ohiprep/src/LookupTables/eez_rgn_2013master.csv
      # rgn_typ rgn_key_2013 rgn_id_2013               rgn_nam_2013 rgn_iso2 eez_key eez_id                    eez_nam
      #     eez           CC           1              Cocos Islands       CC      CC      1              Cocos Islands
      #     eez           CX           2           Christmas Island       CX      CX      2           Christmas Island
      #     eez           NF           3             Norfolk Island       NF      NF      3             Norfolk Island
      #     eez           HM          94 Heard and McDonald Islands       HM      HM     94 Heard and McDonald Islands
      #     eez           MQ           4           Macquarie Island       AU  AU-XMQ      4           Macquarie Island
      #     eez           NC           5              New Caledonia       NC      NC      5              New Caledonia
      # eez_iso3 sov_id   sov_nam  country_id_2012 region_id_2012                      region_name_2012
      #      CCK     16 Australia              CCK              1       Australian Tropical Territories
      #      CXR     16 Australia              CXR              1       Australian Tropical Territories
      #      NFK     16 Australia              NFK              1       Australian Tropical Territories
      #      HMD     16 Australia              HMD              2 Australian Southern Ocean Territories
      #      AUS     16 Australia Macquarie Island              2 Australian Southern Ocean Territories
      #      NCL    179    France              NCL              3                         New Caledonia
### * functions.R - Livelihoods and Economies - but basically the same as the 
###   top one above, except uses the rgn_georegions layer... why is that a layer?

#' Gapfill using georegional means
#' 
#' Gapfill using georegional means, providing the finest possible resolution from 3 hierarchies (r2 > r1 > r0) derived from \href{http://en.wikipedia.org/wiki/United_Nations_geoscheme}{United Nations geoscheme}.
#' 
#' @param data data.frame to gapfill having at least fields: \code{fld_id} and \code{fld_value}, and optionally \code{fld_weight}
#' @param georegions data.frame having at least fields: \code{fld_id} and \code{r0}, \code{r1}, and \code{r2} with georegion id values
#' @param fld_id common spatial id field (eg region_id or country_key) between \code{data} and \code{georegions}
#' @param fld_weight optional weighting field in \code{data}
#' @param rgn_weights data frame of weights, expecting rgn_id in first column and weight in second
#' @param ratio_weights if TRUE, multiply the gapfilled value by the ratio of the region's weight to the regional average weight. Defaults to FALSE. IMPORTANT to set to TRUE if dealing with values that SUM!
#' @param fld_year optional year field in \code{data}
#' @param fld_value value to gapfill in \code{data}
#' @param georegion_labels with same dimensions as georegions having fields: \code{r0_label}, \code{r1_label}, \code{r2_label} and \code{v_label}
#' @param gapfill_scoring_weights used to determine gapfilling scoreset. should range 0 to 1. defaults to \code{c('r0'=1, 'r1'=0.8, 'r2'=0.5, 'v'=0)}
#' @param r0_to_NA assign value of NA if only georegional average availabe at the global level (r0). defaults to True.
#' @param attributes_csv optional path and filename to save attribute table. defaults to NULL
#' 
#' @return Returns a data.frame of having all the \code{fld_id} from georegions filled in the following columns:
#' \itemize{
#'   \item \code{fld_id} - spatial id (eg region_id or country_key).
#'   \item \code{fld_value} - the gapfilled value (eg score).
#' }
#' The returned data.frame also has an attribute "gapfill_georegions" which shows the calculated georegional means and which levels were chosen:
#' \itemize{
#'   \item \code{r0} - georegional id for level 0, ie global.
#'   \item \code{r1} - georegional id for level 1.
#'   \item \code{r2} - georegional id for level 2, the finest resolution of georegions.
#'   \item \code{id} - spatial id (eg region_id or country_key).
#'   \item \code{wts} - weight used to apply \code{\link{weighted.mean}}. Defaults to 1 if not supplied as \code{fld_weight} parameter.
#'   \item \code{val} - original \code{fld_value} in \code{data}
#'   \item \code{r2_val} - weighted.mean for level 2
#'   \item \code{r1_val} - weighted.mean for level 1
#'   \item \code{r0_val} - weighted.mean for level 0 (global)
#'   \item \code{r2_n} - count of regions available for level 2
#'   \item \code{r1_n} - count of regions available for level 1
#'   \item \code{r0_n} - count of regions available for level 0
#'   \item \code{r2_n_notna} - count of region values that are not NA for level 2
#'   \item \code{r1_n_notna} - count of region values that are not NA for level 1
#'   \item \code{r0_n_notna} - count of region values that are not NA for level 0
#'   \item \code{z_level} - finest level available
#'   \item \code{z_ids} - ids for regions that are not NA which contributed to the score
#'   \item \code{z_n} - count of input values for finest level available
#'   \item \code{z_n_pct} - percent of region values that are not NA over all possible [0 to 1]
#'   \item \code{z_g_score} - gapfilling score (see details)
#'   \item \code{z} - weighted.mean for finest level available
#' }
#' 
#' @details
#' Gapfill using georegional means, providing the finest possible resolution from 3 hierarchies (r2 > r1 > r0).
#' 
#' The gapfill score (z_g_score) in the attribute table is formulated such that the higher the score, the 
#' more gapfilling performed. The maximal gapfill score is based on gapfilling at the global level (r0=1) and least
#' if no gapfilling performed (ie z = val). But then some regional averages are applied with only a few regional values 
#' while others might have all but the gapfilled region available. To account for this aspect, the difference between the next
#' finer level's weight is multiplied by the percent regions and subtracted from the level's weight, like so:
#'
#' \code{gapfill_scoring_weights[z_level] - z_n_pct * diff(gapfill_scoring_weights[z_level, z_level_finer])}
#' 
#' @keywords ohi
#' @examples
#' 
#' \dontrun{
#' ## setup
#' require(ohicore)
#' 
#' # gapfill
#' g = gapfill_georegions(data, georegions, fld_weight='w_sum')
#' 
#' # show result and table
#' head(g)
#' head(attr(g, 'gapfill_georegions'))
#' }
#' @import dplyr
#' 
#' @export

library(dplyr); library(tidyr); library(stringr)
### Sample code/data from whounicef sanitation data
data <- read.csv('src/gapfill_georegion_sampledata.csv', stringsAsFactors = FALSE)

georegions <- read.csv('../ohi-global/eez2013/layers/rgn_georegions.csv', na.strings='') %>%
  spread(level, georgn_id) 

georegion_labels <- read.csv('../ohi-global/eez2013/layers/rgn_georegion_labels.csv') %>%    
  mutate(level_label = sprintf('%s_label', level)) %>%
  select(-level) %>%
  spread(level_label, label) %>%
  left_join(
    read.csv('../ohi-global/eez2013/layers/rgn_labels.csv') %>%
      select(rgn_id, v_label=label),
    by='rgn_id') %>%
  arrange(r0_label, r1_label, r2_label, rgn_id); head(georegion_labels)

# attrsave  <- file.path(dir_int, 'rgn_jmp_san_2015a_attr.csv')

# r_g_a <- gapfill_georegions(
#   data = rgn_sani %>%
#     filter(!rgn_id %in% c(213,255)) %>%
#     select(rgn_id, year, access_pct),
#   fld_id = 'rgn_id',
#   georegions = georegions,
#   georegion_labels = georegion_labels,
#   r0_to_NA = TRUE, 
#   attributes_csv = attrsave) # don't chain 

# investigate attribute tables
# head(attr(r_g_a, 'gapfill_georegions'))  # or to open in excel: system(sprintf('open %s', attrsave))


gapfill_georegions <- function(
  data, 
  georegions        = NULL, ### !!! added default; if NULL, load the layers/rgn_georegions.csv and process accordingly
  fld_id            = 'rgn_id', ### was intersect(names(data), names(georegions)), 
                        ### !!! error check and return useful message; fld_id must be in georegions as well, so rgn_id is a good default
  fld_year          = ifelse('year' %in% names(data), 'year', NA),
  fld_value         = setdiff(names(data), c(fld_id, fld_weight, 'year')), 
                        ### !!! error check and return useful message
  georegion_labels  = NULL, 
                        ### !!! keep default; if georegions is NULL and this is TRUE, load layers/rgn_georegion_labels.csv
                        ###     possible values: TRUE (only if georegions default), FALSE or NULL (no labels), pathname (for override)
  fld_weight        = NULL, ### ??? difference between this and rgn_weights and ratio_weights?
  rgn_weights       = NULL, ### ???
  ratio_weights     = FALSE, ### ???
  gapfill_scoring_weights = c('r0' = 1, 'r1' = 0.8, 'r2' = 0.5, 'val' = 0), ### ???
  r0_to_NA          = TRUE, ### ???
  attributes_csv    = NULL ### ??? what is being saved here?
){
  
  ### !!! no default for georegions input?
  ### !!! fld_id a little bit of crapshoot - intersecting names(data) with 
  ###     names(georegions) could produce unintended results?  Can only use
  ###     one fld_id? what about rgn_name and rgn_id for instance
  
  # TODO: provide gapfilling with category data
  
  # check arguments
  stopifnot(length(fld_id) == 1, fld_id %in% names(data), fld_id %in% names(georegions), !fld_id %in% c('r0','r1','r2'))
    ### make sure fld_id is valid, and present in both dataframes, and not one of the georegion groupings
  stopifnot( is.null(fld_weight) || (!is.null(fld_weight) && fld_weight %in% names(data)) )
    ### if weighted, make sure field weighting column is present in data.  What is field weight???
  if (!is.null(rgn_weights)) stopifnot(ncol(rgn_weights) == 2 & names(rgn_weights)[1] == 'rgn_id')
    ### rgn_weights is data frame, 2 columns, first = rgn_id.  Seems restrictive or untransparent?
  stopifnot(length(fld_value) == 1, fld_value %in% names(data))
    ### fld_value can only be one column; must be in data
  stopifnot(all(c('r0', 'r1', 'r2') %in% names(georegions)))
    ### make sure all groupings in georegions
  stopifnot(all(data[[fld_id]] %in% georegions[[fld_id]]))
    ### if some of the fld_id instances don't show up in georegions, stop
  stopifnot( is.na(fld_year) || (!is.na(fld_year) && fld_year %in% names(data)) )
    ### if fld_year used, make sure it's in the data frame
  stopifnot(!(!is.null(fld_weight) & !is.null(rgn_weights))) 
    ### can't weight both ways by georegion and data.frame
  stopifnot( ratio_weights==F | (ratio_weights==T & !is.null(rgn_weights)) )  
    ### need rgn_weights if applying ratio_weights
  
  if(is.null(georegions)) {
    ### no georegions passed to function; use default
    georegions <- read.csv('../ohi-global/eez2013/layers/rgn_georegions.csv', na.strings='') %>%
      spread(level, georgn_id) 
  }
  if(is.null(georegions) & georegion_labels == TRUE) {
    ### no georegions object passed to function, but labels requested; use default
    georegion_labels <- read.csv('../ohi-global/eez2013/layers/rgn_georegion_labels.csv') %>%    
      mutate(level_label = sprintf('%s_label', level)) %>%
      select(-level) %>%
      spread(level_label, label) %>%
      left_join(
        read.csv('../ohi-global/eez2013/layers/rgn_labels.csv') %>%
          select(rgn_id, v_label=label),
        by='rgn_id') %>%
      arrange(r0_label, r1_label, r2_label, rgn_id); head(georegion_labels)
  }
  
  ### rename fields (using dplyr alternative evaluation, rename_())
  georgn <- georegions %>% 
    dplyr::rename_(.dots = setNames(fld_id, 'id'))
  df_in <- data %>%
    dplyr::rename_(.dots = setNames(c(fld_id, fld_value), c('id', 'val')))
  
  ### check for duplicate georegion id entries
  stopifnot(anyDuplicated(georgn$id) == 0)
  
  ### georegion_labels - error checking -----
  if (!is.null(georegion_labels)){
    stopifnot(fld_id %in% names(georegion_labels)) ### ??? need rgn_id (or id?) field
    stopifnot(all(c('r0_label', 'r1_label', 'r2_label') %in% names(georegion_labels)))
      ### !!! this forces a specific format for georegion_labels - dislike?
    stopifnot(nrow(georegion_labels) == nrow(georegions))
    gr_labs <- georegion_labels %>% 
      dplyr::rename_(.dots = setNames(fld_id, 'id'))
    stopifnot(anyDuplicated(gr_labs$id) == 0)
  }
  
  ### get n regions per georegion for later calculating gapfill score
  georgn <- georgn %>%
    group_by(r0) %>%
    mutate(r0_n = n()) %>%  
    group_by(r1) %>%
    mutate(r1_n = n()) %>%
    group_by(r2) %>%
    mutate(r2_n = n())
  
  ### add weights to data ----
  ### options:
  ### * no fld_weight and no rgn_weights, so use default (wts = 1)
  ### * fld_weight is set; use weights included in dataframe
  ### * rgn_weights is set; use weights from rgn_weights object.
  ### !!! use a single argument to function: 
  ### * if character string, use as field name (or if ends in .csv, maybe read.csv?)
  ### * if dataframe, use as dataframe
  
  if (is.null(fld_weight) & is.null(rgn_weights)){
    ### default weights
    df_in$wts <- 1
    
  } else if (!is.null(fld_weight)){
    ### use weights in data frame fld_weight column
    df_in <- df_in %>% 
      dplyr::rename_(.dots = setNames(fld_weight, 'wts'))

    if (sum(is.na(df_in$wts)) > 0){
      ### weights variable contains NAs; remove NA occurrences
      message(sprintf('\n  data[[fld_weights]] are NA (where values reported) so removed: %d of %d rows\n    %s', 
                      sum(is.na(df_in$wts)), nrow(df_in), 
                      paste(unique(df_in$id[is.na(df_in$wts)]), collapse=',') ))
      df_in <- df_in %>%
        filter(!is.na(wts))      
    }    
  } else if(!is.null(rgn_weights)){
    ### use rgn_weights object
    georgn <- georgn %>%
      left_join(
        rgn_weights %>%
          dplyr::rename_(.dots = setNames(names(rgn_weights)[2], 'wts')) %>%
          select(id=rgn_id, wts),
        by='id')
    
    if (sum(is.na(georgn$wts)) > 0){
      message(sprintf('\n  georegions[[weights]] are NA (where georegions with rgn_id exist) so removed: %d of %d rows\n    %s', 
                      sum(is.na(georgn$wts)), nrow(georgn), 
                      paste(unique(georgn$id[is.na(georgn$wts)]), collapse=',') ))
      georgn <- subset(georgn, !is.na(georgn))
    }
  } else {
    stop('weights setting options exhausted: logical impossibility!')
  }
  
  ### remove NAs from value field
  if (sum(is.na(df_in$val)) > 0){
    message(sprintf('\n  data values are NA so removed: %d of %d rows', 
                    sum(is.na(df_in$val)), nrow(df_in) ))
    df_in <- df_in %>%
      filter(!is.na(val))
  }
  
  ### Create dataframe of georegion summaries, with no year -----
  if (is.na(fld_year)){
    ### No year field, so should not be duplicate rgn IDs: check for duplicates
    stopifnot(anyDuplicated(df_in$id) == 0)
    
    ### merge georegions with data
    ### !!! How are x, y, and z related and used? argh!
    ###   x is the basic df_in with georegions attached.
    ###   y is x, NAs filtered out, for grouping and summarizing at each georegion level
    ###   z is x, with summarized ys for each georegion.  z is the final product.
    x <- georgn %>%
      left_join(df_in, by = 'id') %>%
      arrange(id)
    
    # georegion means
    y <- x %>%
      filter(!is.na(val), !is.na(wts))
    z <- x %>%
      left_join(
        y %>% 
          group_by(r2) %>%
          summarise(
            r2_val       = weighted.mean(val, wts),
            r2_wts_avg   = mean(wts),
            r2_n_notna = n(),
            r2_ids     = paste(id, collapse=',')),
        by='r2') %>%
      left_join(
        y %>% 
          group_by(r1) %>%
          summarise(
            r1_val       = weighted.mean(val, wts),
            r1_wts_avg   = mean(wts),
            r1_n_notna = n(),
            r1_ids     = paste(id, collapse=',')),
        by='r1') %>%
      left_join(
        y %>% 
          group_by(r0) %>%
          summarise(
            r0_val       = weighted.mean(val, wts),
            r0_wts_avg   = mean(wts),
            r0_n_notna = n(),
            r0_ids     = paste(id, collapse=',')),
        by='r0') %>%
      arrange(r0, r1, r2, id) %>%
      select(r0, r1, r2, id, wts, val, r2_val, r1_val, r0_val, 
             r2_wts_avg,   r1_wts_avg,   r0_wts_avg, 
             r2_n,       r1_n,       r0_n, 
             r2_n_notna, r1_n_notna, r0_n_notna, 
             r2_ids,     r1_ids,     r0_ids)    
  } else {    
    ### Create dataframe of georegion summaries, *with* year -----
    df_in <- df_in %>%
      dplyr::rename_(.dots = setNames(fld_yr, 'yr'))
    
    # check for duplicates
    stopifnot(anyDuplicated(df_in[,c('id','yr')]) == 0)
    
    # TODO: expand gapfill_georegions to use rgn_weights with year to match data
    
    # expand georegions to every possible year in data
    georgn_yr <- expand.grid(list(
      yr = sort(unique(d$yr)),
      id = georgn$id)) %>%
      merge(
        georgn, 
        by='id') %>%
      #select(yr, id, r0, r1, r2, r2_n, r1_n, r0_n, wts) %>%
      arrange(yr, id)
    
    # merge with data
    x <- georgn_yr %>%
      merge(
        df_in, 
        by=c('yr','id'), all.x=T) %>%      
      arrange(yr, id) %>%
      select(yr, id, r0, r1, r2, r2_n, r1_n, r0_n, val, wts)
    
    # get rows with val and wts
    y <- x %>%
      filter(!is.na(val), !is.na(wts))
    
    # calculate georegion means
    z <- x %>%
      left_join(
        y %>% 
          group_by(yr, r2) %>%
          summarise(
            r2_val       = weighted.mean(val, wts),
            r2_wts_avg   = mean(wts),
            r2_n_notna = n(),
            r2_ids     = paste(id, collapse=',')),
        by=c('yr','r2')) %>%
      left_join(
        y %>% 
          group_by(yr, r1) %>%
          summarise(
            r1_val       = weighted.mean(val, wts),
            r1_wts_avg   = mean(wts),
            r1_n_notna = n(),
            r1_ids     = paste(id, collapse=',')),
        by=c('yr','r1')) %>%
      left_join(
        y %>% 
          group_by(yr, r0) %>%
          summarise(
            r0_val       = weighted.mean(val, wts),
            r0_wts_avg   = mean(wts),
            r0_n_notna = n(),
            r0_ids     = paste(id, collapse=',')),
        by=c('yr','r0')) %>%
      arrange(yr, r0, r1, r2, id) %>%
      select(yr, r0, r1, r2, id, wts, val, 
             r2_val,       r1_val,       r0_val, 
             r2_wts_avg,   r1_wts_avg,   r0_wts_avg, 
             r2_n,       r1_n,       r0_n, 
             r2_n_notna, r1_n_notna, r0_n_notna, 
             r2_ids,     r1_ids,     r0_ids)    
  }
  
  # select best available value and calculate gapfilling score  
  z <- z %>%
    mutate(
      z_level = ifelse(!is.na(val), 'val',
                       ifelse(!is.na(r2_val), 'r2',
                              ifelse(!is.na(r1_val), 'r1',
                                     ifelse(!is.na(r0_val), 'r0', NA)))))
  
  # assign attributes by georegion level (r#)
  z  <- 
  rbind_list(
    # rgn
    z %>%
      filter(z_level=='val') %>%
      mutate(
        z_ids     = as.character(id),
        z_wts_avg   = wts,
        z_n       = 1,
        z_n_pct   = 1,
        z_g_score = 0,
        z         = val),
    # r2
    z %>%
      filter(z_level=='r2') %>%
      mutate(
        z_ids     = r2_ids,
        z_wts_avg   = r2_wts_avg,
        z_n       = r2_n_notna,
        z_n_pct   = r2_n_notna/r2_n,
        z_g_score = gapfill_scoring_weights['r2'] - z_n_pct * diff(gapfill_scoring_weights[c('val','r2')]),
        z         = r2_val),
    # r1
    z %>%
      filter(z_level=='r1') %>%
      mutate(
        z_ids     = r1_ids,
        z_wts_avg   = r1_wts_avg,
        z_n       = r1_n_notna,
        z_n_pct   = r1_n_notna/r1_n,
        z_g_score = gapfill_scoring_weights['r1'] - z_n_pct * diff(gapfill_scoring_weights[c('val','r1')]),
        z         = r1_val),
    # r0
    z %>%
      filter(z_level=='r0') %>%
      mutate(
        z_ids     = r0_ids,
        z_wts_avg   = r0_wts_avg,
        z_n       = r0_n_notna,
        z_n_pct   = r0_n_notna/r0_n,
        z_g_score = gapfill_scoring_weights['r0'] - z_n_pct * diff(gapfill_scoring_weights[c('val','r0')]),
        z         = r0_val)
  ) %>%
  select(-r2_ids, -r1_ids, -r0_ids)
  
  # multiply by ratio if argument
  if (ratio_weights){  
    z <- z %>% mutate(
      z_orig = z,
      z = z * wts / z_wts_avg)    
  }


  # if r0_to_NA, assign value of NA if only georegional average available at the global level (r0)
  if (r0_to_NA) z$z <- ifelse(z$z_level=='r0', NA, z$z)
  
  # add labels if provided
  if (!is.null(georegion_labels)){
    z <- z %>%
      left_join(
        gr_labs %>%
          select(id=id, r0_label, r1_label, r2_label, v_label),
        by='id') %>%
      arrange(r0_label, r1_label, r2_label, v_label) 
    if (is.na(fld_year)){
      z <- z %>%
        select(r0_label, r1_label, r2_label, v_label, 
               r0, r1, r2, id, wts, val, r2_val, r1_val, r0_val, r2_n, r1_n, r0_n, 
               r2_n_notna, r1_n_notna, r0_n_notna, z_level, z_ids, z_n, z_n_pct, z_g_score, z)
    } else {
      z <- z %>%
        select(r0_label, r1_label, r2_label, v_label, yr, 
               r0, r1, r2, id, wts, val, r2_val, r1_val, r0_val, r2_n, r1_n, r0_n, 
               r2_n_notna, r1_n_notna, r0_n_notna, z_level, z_ids, z_n, z_n_pct, z_g_score, z)
    }
  }
  
  # return result
  if (is.na(fld_year)){
    r <- z %>%
      select(id, z) %>%
      arrange(id) %>%
      dplyr::rename_(.dots = setNames(c('id','z'), c(fld_id, fld_value)))
      
  } else {
    r <- z %>%
      select(yr, id, z) %>%
      arrange(yr, id) %>%
      dplyr::rename_(.dots = setNames(c('yr', 'id', 'z'), c(fld_year, fld_id, fld_value)))
  }
  
  # store attributes, with option to save as .csv
  attr(r, 'gapfill_georegions') <- z
  
  if (!is.null(attributes_csv)){
    write.csv(z, attributes_csv, na = '', row.names=FALSE)   
  }
  
  return(r)
}