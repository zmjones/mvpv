seed <- 1987
set.seed(seed)

## load and preprocess data
pkgs <- c("party", "mmpf", "reshape2", "stringr", "batchtools", "rio")
invisible(sapply(pkgs, library, character.only = TRUE))

path <- unlist(str_split(getwd(), "/"))
dir_prefix <- ifelse(path[length(path)] == "R", "../", "./")
r_dir_prefix <- ifelse(path[length(path)] == "mvpv", "R/", "./")
data_prefix <- paste0(dir_prefix, "data/")

source(paste0(r_dir_prefix, "functions.R"))
source(paste0(r_dir_prefix, "global.R"))

resources <- list(walltime = 24 * 60 * 60, memory = "24gb", nodes = 1L,
  measure.memory = TRUE)
options(batchtools.progress = FALSE)
pars <- CJ(x = regime$name, year = c(1970, 1990))

## fit bivariate models
fit_bv_reg <- makeRegistry("fit_bv_registry", packages = pkgs, seed = seed)
fit_bv_reg$cluster.functions <- makeClusterFunctionsTorque("template.tmpl")
batchMap(estimate, x = pars$x, year = pars$year, more.args = list(covariates = FALSE))
submitJobs(reg = fit_bv_reg, resources = resources)
waitForJobs(reg = fit_bv_reg)
fits_bv <- reduceResultsList(reg = fit_bv_reg)
## need to adapt viz code
write_results(fits_bv, pars, "fit_bv")

## fit models
fit_reg <- makeRegistry("fit_registry", packages = pkgs, seed = seed)
fit_reg$cluster.functions <- makeClusterFunctionsTorque("template.tmpl")
batchMap(estimate, x = pars$x, year = pars$year)
batchExport(list(preprocess = preprocess, dir_prefix = dir_prefix), reg = fit_reg)
submitJobs(reg = fit_reg, resources = resources)
waitForJobs(reg = fit_reg)
fits <- reduceResultsList(reg = fit_reg)
write_results(fits, pars, "fit")

## fit/predict multi target models for comparison
## for each start year and regime variable combination
## iteratively hold out each year from start year + 1 to 2008
## and compute predictions for that year 
pred_pars <- rbind(
  CJ(x = regime$name, year = 1970, hold_out = 1971:2008),
  CJ(x = regime$name, year = 1990, hold_out = 1991:2008)
)

fit_multi_reg <- makeRegistry("fit_multi_registry", packages = pkgs, seed = seed)
fit_multi_reg$cluster.functions <- makeClusterFunctionsTorque("template.tmpl")
batchMap(estimate, x = pred_pars$x, year = pred_pars$year, hold_out = pred_pars$hold_out)
submitJobs(reg = fit_multi_reg, resources = resources)
waitForJobs(reg = fit_multi_reg)
fits_multi <- reduceResultsList(reg = fit_multi_reg)
## stack using start year and regime
write_results(fits_multi, pred_pars, "fits_multi")

## fit/predict single target models for comparison
fit_single_reg <- makeRegistry("fit_single_registry", packages = pkgs, seed = seed)
fit_single_reg$cluster.functions <- makeClusterFunctionsTorque("template.tmpl")
batchMap(estimate, x = pred_pars$x, year = pred_pars$year, hold_out = pred_pars$hold_out)
submitJobs(reg = fit_single_reg, resources = resources)
waitForJobs(reg = fit_single_reg)
fits_single <- reduceResultsList(reg = fit_single_reg)
write_results(fits_single, pred_pars, "fits_single")

## compute errors and create a comparison plot
invisible(lapply(as.list(t(pred_pars)),
  function(x) contrast_error(x[1], as.integer(x[2]), as.integer(x[3]))))

## univariate partial dependence
pd_reg <- makeRegistry("pd_registry", packages = pkgs, seed = seed)
pd_reg$cluster.functions <- makeClusterFunctionsTorque("template.tmpl")
batchExport(list(dir_prefix = dir_prefix), reg = pd_reg)
batchMap(univariate_pd, x = pars$x, year = pars$year,
  more.args = list(n = c(10, NA)), reg = pd_reg)
submitJobs(resources = resources, reg = pd_reg)
waitForJobs(reg = pd_reg)
pd <- reduceResultsList(findDone(reg = pd_reg), reg = pd_reg)
write_results(pd, pars[unlist(findDone(reg = pd_reg), )], "pd")

## bivariate partial dependence
pd_int_reg <- makeRegistry("pd_int_registry", packages = pkgs, seed = seed)
pd_int_reg$cluster.functions <- makeClusterFunctionsTorque("template.tmpl")
batchExport(list(dir_prefix = dir_prefix), reg = pd_int_reg)
pars <- CJ(x = regime$name, year = c(1970, 1990), z = explanatory$name)
batchMap(bivariate_pd, x = pars$x, year = pars$year, z = pars$z,
  more.args = list(n = c(10, NA)), reg = pd_int_reg)
submitJobs(resources = resources, reg = pd_int_reg)
waitForJobs(reg = pd_int_reg)
pd_int <- reduceResultsList(reg = pd_int_reg)
write_results(pd_int, pars[unlist(findDone(reg = pd_int_reg), )], "pd_int")
