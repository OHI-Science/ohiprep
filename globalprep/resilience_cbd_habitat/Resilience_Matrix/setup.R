sink('_per_habitat_matrix.txt') # used to copy-n-paste into the static matrix
d <- read.csv('manual_output/habitat_combos.csv')
for (n in d$name) {
  if (substr(n, 1, 2) == 'sh') {
    goal <- 'safe-coastlines'
  } else if (substr(n, 1, 2) == 'ca') {
    goal <- 'carbon-storage'
  } else if (substr(n, 1, 2) == 'bd') {
    goal <- 'biodiversity'
  } else {
    stopifnot(TRUE)
  }
  
  for (i in 2:ncol(d)) {
    l <- as.character(d[d$name==n,i])
    if (!is.na(l) && l != '') {
      cat(sprintf('%s,habitats-%s,%s\n', goal, n, l))
    }
  }
}
