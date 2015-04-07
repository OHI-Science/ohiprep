fao_clean_data <- function(m, lowdata_value = 0.1) {
### Swaps out FAO-specific codes for analysis.
  
  m1 <- m %>%
    mutate(  
      value = str_replace(value, fixed( ' F'),    ''),  
        # FAO denotes with F when they have estimated the value using best available data
      value = str_replace(value, fixed(  '-'),   '0'),  
      # FAO's code for true 0
      value = str_replace(value, fixed('...'),    NA),  
      # FAO's code for NA
      value = str_replace(value, fixed(  '.'),    NA),  
        # is this a code that shows up? This line MUST occur before replacing '0 0' with '0.1'
      value = str_replace(value, fixed('0 0'), lowdata_value),  
        # FAO denotes something as '0 0' when it is > 0 but < 1/2 of a unit. 
        # Replace with lowdata_value.
      value = ifelse(value =='', NA, value),
      value = as.numeric(as.character(value)),
      year  = as.integer(as.character(year)))       # search in R_inferno.pdf for "shame on you"

  return(m1)
}