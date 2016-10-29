mar_split <- function(m) {
  ### Deal with special cases of countries, specific to MAR: Netherlands Antilles reported multiple ways, including 'Bonaire/S.Eustatius/Saba' 
  ### - FAO reports 'Antilles' as one region, but OHI considers as four 
  ###   reported regions; break up and distribute values 
  
  m_ant <- m %>%
    filter(country == 'Netherlands Antilles') %>%  # Conch was cultivated for restoration purposes in a joint programme across these 3 countries
    mutate(value            = value/3,  
      'Aruba'        = value,
      'Bonaire'           = value,
      'Curacao'   = value) %>%
    select(-value, -country) %>%
    gather('country', 'value', -species, -fao, -environment, -year, -Taxon_code) %>%
    mutate(country = as.character(country))

  m <- m %>%
    filter(country != 'Netherlands Antilles') %>%
    bind_rows(m_ant) %>%  
    arrange(country, fao, environment, species, year, value) 
    
m_ant2 <- m %>%
  filter(country == 'Bonaire/S.Eustatius/Saba') %>%  # Cobia was probably mostly in Curacao, but can't find evidence for it
  mutate(
    value            = value/3,
    'Bonaire'        = value,
    'Saba'           = value,
    'Sint Eustatius'   = value) %>%
  select(-value, -country) %>%
  gather(country, value, -species, -fao, -environment, -year, -Taxon_code) %>%
  mutate(country = as.character(country)) 
m <- m %>%
  filter(country != 'Bonaire/S.Eustatius/Saba') %>%
    bind_rows(m_ant) %>% 
  arrange(country, fao, environment, species, year, value) 

m_ant3 <- m %>%
  filter(country == 'Channel Islands') %>%
  mutate(
    value            = value/2,
    'Guernsey'        = value,
    'Jersey'           = value) %>%
  select(-value, -country) %>%
  gather(country, value, -species, -fao, -environment, -year, -Taxon_code) %>%
  mutate(country = as.character(country))  
m <- m %>%
  filter(country != 'Channel Islands') %>%
      bind_rows(m_ant) %>%  
  arrange(country, fao, environment, species, year, value) 


  return(m)
}

