yr.to.formula = function(yrs, yr){
  # function to produce formula for weighting year inputs, eg for population predictions
  if (yr %in% yrs){
    cat(sprintf('%d:"r_%d",\n', yr, yr))
  } else {
    n = length(yrs)
    a = approx(yrs, 1:n, yr)$y
    i.1 = floor(a) # integer
    i.2 = ceiling(a)
    m = a %% 1   # modulus
    yr.1 = yrs[i.1]
    yr.2 = yrs[i.2] # ifelse(i+1>n, yrs[n], yrs[i])
    #browser()
    if (m)
    cat(sprintf('%d:"%g * r_%d + %g * r_%d",\n', yr, (1-m), yr.1, m, yr.2))
  }
}

# get formulas for all years
yrs = c(2005, 2010, 2015)
for (yr in 2005:2015){ # yr = 2013
  yr.to.formula(yrs, yr)
}