seed <- 1987
set.seed(seed)

## load and preprocess data
pkgs <- c("party", "mmpf", "reshape2", "stringr", "ddalpha", "batchtools")
invisible(sapply(pkgs, library, character.only = TRUE))

path <- unlist(str_split(getwd(), "/"))
dir_prefix <- ifelse(path[length(path)] == "R", "../", "./")
r_dir_prefix <- ifelse(path[length(path)] == "mvpv", "R/", "./")

source(paste0(r_dir_prefix, "functions.R"))
source(paste0(r_dir_prefix, "global.R"))

resources <- list(walltime = 24 * 60 * 60, memory = "24gb", nodes = 1L,
  measure.memory = TRUE)
options(batchtools.progress = FALSE)
pars <- CJ(x = regime$name, year = c(1970, 1990))

fit_reg <- makeRegistry("fit_registry", packages = pkgs, seed = seed)
fit_reg$cluster.functions <- makeClusterFunctionsTorque("template.tmpl")

batchMap(estimate, x = pars$x, year = pars$year,
  more.args = list(explanatory = explanatory, regime = regime))
batchExport(list(preprocess = preprocess, dir_prefix = dir_prefix), reg = fit_reg)
submitJobs(reg = fit_reg, resources = resources)
waitForJobs(reg = fit_reg)
fits <- reduceResultsList(reg = fit_reg)
write_results(fits, pars, "fit")

pd_reg <- makeRegistry("pd_registry", packages = pkgs, seed = seed)
pd_reg$cluster.functions <- makeClusterFunctionsTorque("template.tmpl")
batchExport(list(dir_prefix = dir_prefix), reg = pd_reg)
batchMap(univariate_pd, x = pars$x, year = pars$year,
  more.args = list(n = c(10, NA)), reg = pd_reg)
submitJobs(resources = resources, reg = pd_reg)
waitForJobs(reg = pd_reg)
pd <- reduceResultsList(findDone(reg = pd_reg), reg = pd_reg)
write_results(pd, pars[unlist(findDone(reg = pd_reg), )], "pd")

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
