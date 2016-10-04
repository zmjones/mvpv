seed <- 1987
set.seed(seed)

## load and preprocess data
pkgs <- c("party", "edarf", "reshape2", "stringr", "ddalpha", "batchtools",
  "doParallel")
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

inner <- 20L
resources$ppn <- inner
pd_reg <- makeRegistry("pd_registry", packages = pkgs, seed = seed)
pd_reg$cluster.functions <- makeClusterFunctionsTorque("template.tmpl")
batchExport(list(dir_prefix = dir_prefix), reg = pd_reg)
batchMap(univariate_pd, x = pars$x, year = pars$year,
  more.args = list(cutoff = 15, inner = inner, fun = mean),
  reg = pd_reg)
submitJobs(resources = resources, reg = pd_reg)

pd_int_reg <- makeRegistry("pd_int_registry", packages = pkgs, seed = seed)
pd_int_reg$cluster.functions <- makeClusterFunctionsTorque("template.tmpl")
batchExport(list(dir_prefix = dir_prefix, explanatory = explanatory$name),
  reg = pd_int_reg)
batchMap(bivariate_pd, x = regime$name, year = c(1970, 1990),
  more.args = list(cutoff = 30, inner = inner, fun = mean),
  reg = pd_int_reg)
submitJobs(resources = resources, reg = pd_int_reg)

waitForJobs(reg = pd_reg)
pd <- reduceResultsList(reg = pd_reg)
write_results(pd, pars, "pd")

waitForJobs(reg = pd_int_reg)
pd_int <- reduceResultsList(reg = pd_int_reg)
write_results(pd_int, pars, "pd_int")
