pkgs <- c("dplyr", "assertthat", "countrycode", "stringr", "lubridate", "ggplot2", "party",
          "foreach", "doParallel", "tidyr", "stringr", "devtools", "rio", "plyr")
install.packages(pkgs, repo = "http://cran.rstudio.com")

library(devtools)

install_github("zmjones/edarf")
install_github("hadley/readxl")
install_github("leeper/rio")
