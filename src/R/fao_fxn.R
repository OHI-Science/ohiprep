### ohiprep/src/R/fao_fxn.R
### Function(s) to help clean and manipulate FAO data.
###
### Provenance:
###   Apr2015: created by Casey O'Hara (oharac)


fao_clean_data <- function(m, sub_0_0 = 0.1) {
### Swaps out FAO-specific codes for analysis:
### * FAO_commodities (Natural Products goal)
###
### Note separate calls to mutate() may not be necessary, but ensures proper sequence of flag-replacing, just in case...
###
  
  m1 <- m %>%
    mutate(  
      value = str_replace(value, fixed('F '), ''),
      value = str_replace(value, fixed(' F'), ''),
        ### FAO denotes with F when they have estimated the value using best available data,
        ###   sometimes comes at start (commodities), sometimes at end (mariculture)...?
      value = ifelse(value == '...', NA, value),
        ### FAO's code for NA
      value = str_replace(value, fixed('0 0'), sub_0_0),  
        ### FAO denotes something as '0 0' when it is > 0 but < 1/2 of a unit. 
        ### Replace with lowdata_value.
      value = str_replace(value, fixed(  '-'), '0'),  
        ### FAO's code for true 0
      value = ifelse(value =='', NA, value)) %>%
    mutate(
      value = as.numeric(as.character(value)),
      year  = as.integer(as.character(year)))       # search in R_inferno.pdf for "shame on you"
  
  return(m1)
}