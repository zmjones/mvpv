## have these packages installed
## - party
## - mmpf
## - stringr
## - batchtools
## - ggplot2
## - data.table
## - assertthat
## - lubridate
## - countrycode
## - foreign
## - dplyr
## - dtplyr
## - snow
## - polycor

## i assume you have extracted `data.tar.gz` to its own directory `data` and
## that this is being executed in the parent directory
## i also assume you have extracted `R.tar.gz` to its own directory `R` as well.
library(assertthat)
assert_that(all(c("data", "R") %in% dir()))

## also create two directories named 'figures' and 'results' in the parent directory.
dir.create("figures")
dir.create("results")
assert_that(all(c("figures", "results") %in% dir()))

## ingests and joins all the data
source("R/data.R")

## memory cleanup: removes any objects in your environment (hope you didn't have anything you wanted in there...)
rm(list = ls())
gc()

## fits models, partial dependence, creates figures
## this is written assuming you have a multiple processor system with *enough* memory
## it is setup by default to use a socket cluster. you can easily change this to execute
## serially in the interpreter by changing the cluster type to "interactive" (read the batchtools documentation)
## a multicore cluster, a torque/pbs type cluster (what i ran it on), etc.
## if you don't have enough memory i guess it might melt your laptop (more likely crash R)
source("R/analysis.R")
