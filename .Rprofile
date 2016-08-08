
## Set Default cran repository
r <- getOption("repos");
r["CRAN"] <- "https://cran.rstudio.com/"
options(tz="US/Eastern", stringsAsFactors=TRUE, repos = r)
rm(r)


## if OS X, set package library folder
if(Sys.info()[['sysname']] == "Darwin") {.libPaths('/Users/umit/Rlibs')}






# Packages to load
require(pacman, quietly = TRUE)
pacman::p_load(utils, stats, grid, gridExtra, lubridate, readxl, readr,
  tibble, RColorBrewer, magrittr, purrr)

pacman::p_load_gh("hadley/dplyr", "hadley/tidyr", "hadley/ggplot2",
  "hadley/lubridate", update =TRUE)


#Additional packages to load
p_load(roxygen2)
