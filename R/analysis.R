args <- commandArgs(TRUE)
cores <- as.integer(args[1])

seed <- 1987
set.seed(seed)

if (is.na(cores)) cores <- parallel::detectCores()

## load and preprocess data
pkgs <- c("party", "edarf", "reshape2", "stringr", "doParallel", "parallel")
invisible(sapply(pkgs, library, character.only = TRUE))

path <- unlist(str_split(getwd(), "/"))
dir_prefix <- ifelse(path[length(path)] == "R", "../", "./")
r_dir_prefix <- ifelse(path[length(path)] == "mvpv", "R/", "./")

source(paste0(r_dir_prefix, "functions.R"))
source(paste0(r_dir_prefix, "global.R"))

data <- list(
  df_1990_2008 = read.csv(paste0(dir_prefix, "data/1990_2008_rep.csv"), stringsAsFactors = TRUE),
  df_1970_2008 = read.csv(paste0(dir_prefix, "data/1970_2008_rep.csv"), stringsAsFactors = TRUE)
)

data <- lapply(data, preprocess, regime_variables = regime$name)

cl <- makePSOCKcluster(cores)
registerDoParallel(cl)

## fit random forest on each combination of regime type variable and data (1990 and 1970 start)
## with some outcome variables excluded for 1970 start
## compute the univariate partial dependence of regime type on the trained random forest
## compute the bivariate partial dependence of each explanatory variable and each measure of regime type
## on the the random forest. save all of this for visualization
for (d in data) {
  for (x in regime$name) {
    form <- paste0(paste0(d$outcomes, collapse = " + "), " ~ ",
      paste0(c(explanatory$name, x), collapse = " + "))
    fit <- cforest(as.formula(form), data = d$df, weights = d$weights, controls = d$control)
    save(fit, file = paste0(dir_prefix, "results/fit_", x, "_", min(d$df$year), ".RData"))
    pd <- partial_dependence(fit, var = x, cutoff = 15, parallel = TRUE)
    save(pd, file = paste0(dir_prefix, "results/pd_", x, "_", min(d$df$year), ".RData"))
    load(paste0(dir_prefix, "results/fit_", x, "_", min(d$df$year), ".RData"))
    pd_int <- vector("list", length(explanatory_variables))
    names(pd_int) <- apply(expand.grid(x, explanatory_variables), 1,
      function(x) paste0(x, collapse = ":"))
    for (z in explanatory_variables) {
      pd_int[[paste(x, z, sep = ":")]] <- partial_dependence(fit, var = c(x, z),
        cutoff = 30, interaction = TRUE, parallel = TRUE)
    }
    save(pd_int, file = paste0(dir_prefix, "results/pd_int_", x, "_",
      min(d$df$year), ".RData"))
  }
}
