args <- commandArgs(TRUE)
cores <- as.integer(args[1])

seed <- 1987
set.seed(seed)

if (is.na(cores)) cores <- parallel::detectCores()

## load and preprocess data
pkgs <- c("party", "edarf", "reshape2", "stringr", "foreach", "doParallel")
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

data <- lapply(data, preprocess, regime_variables = regime_variables)

cl <- makePSOCKcluster(cores)
registerDoParallel(cl)

fits <- foreach(d = data, .packages = "party") %:% foreach(x = regime_variables) %dopar% {
  form <- paste0(paste0(d$outcomes, collapse = "+"), "~",
                 paste0(c(explanatory_variables, x), collapse = "+"))
  cforest(as.formula(form), d = d$df, weights = d$weights, controls = d$control)
}
fits <- unlist(fits, FALSE)
save(fits, file = "fits.RData")

pd <- foreach(fit = fits[1:2], .packages = c("party", "edarf")) %do% {
  xvars <- names(fit@data@get("input"))
  partial_dependence(fit, var = xvars[length(xvars)], cutoff = 15, parallel = TRUE)
}
save(pd, file = "pd.RData")

pd_int <- foreach(fit = fits[1], .packages = c("party", "edarf")) %:%
  foreach(x = regime_variables[1], z = explanatory_variables[1]) %do% {
    partial_dependence(fit, var = c(x, z), cutoff = 3, interaction = TRUE, parallel = TRUE)
  }
save(pd_int, file = "pd_int.RData")
