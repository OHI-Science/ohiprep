# README.md

Starting with eez2015, we will access WGI data (from World Bank) using their API and the R package `WGI`, available at:
https://github.com/vincentarelbundock/WDI

Data in the `raw` folder was archived after access in April 2015: write.csv(d, file.path(dir_wgi, 'raw', 'worldbank_wgi_from_wdi_api.csv'), row.names=F)

Note @Melsteroni in Issue #410

OK, data are done and in `ohiprep/globalprep/worldbank_wgi/data` (commit a97716e). There is a separate file for each scenario (suffixed with `_2015a`, etc), and also for the inverse of wgi (`_2015a_inverse`, etc). The `_attr` tables are there also for gapfilling.

A few details within [data_prep.r](https://github.com/OHI-Science/ohiprep/blob/master/globalprep/worldbank_wgi/data_prep.R):

*  [gapfilling](https://github.com/OHI-Science/ohiprep/blob/master/globalprep/worldbank_wgi/data_prep.R#L159-L195) happens by sovereign country if applicable, and then by georegion 
* for error checking I added the [readline](https://github.com/OHI-Science/ohiprep/blob/master/globalprep/worldbank_wgi/data_prep.R#L123) function ([also here](https://github.com/OHI-Science/ohiprep/blob/master/globalprep/worldbank_wgi/data_prep.R#L217)), which pauses so that you can look at at the output in the console and think about it before the code moves on. **This only operates when you source the script**. I was trying to make sure that the highs and lows made sense once the six WGI indicators were combined into a score; this was something I had to look at and couldn't do any kind of fancy differencing (come on dat!)
* I spent some time (too long really) trying to play with ggvis to look into the gapfilling behavior, but I didn't end up with anything substantial. I'd like to be able to hover over a histogram and see what is making up each bin. I can get close with [this code](http://rpackages.ianhowson.com/cran/ggvis/man/add_tooltip.html) but not quite there: 
```r
# The data sent from client to the server contains only the data columns that
# are used in the plot. If you want to get other columns of data, you should
# to use a key to line up the item from the plot with a row in the data.
mtc <- mtcars
mtc$id <- 1:nrow(mtc)  # Add an id column to use ask the key

all_values <- function(x) {
  if(is.null(x)) return(NULL)
  row <- mtc[mtc$id == x$id, ]
  paste0(names(row), ": ", format(row), collapse = "<br />")
}

mtc %>% ggvis(x = ~wt, y = ~mpg, key := ~id) %>%
  layer_points() %>%
  add_tooltip(all_values, "hover")

}
```


I'll also add this to the `ohiprep/globalprep/worldbank_wgi/readme.md`