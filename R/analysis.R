seed = 1987
set.seed(seed)

## load and preprocess data
pkgs = c("party", "mmpf", "stringr", "batchtools", "ggplot2", "data.table")
invisible(sapply(pkgs, library, character.only = TRUE))

path = unlist(str_split(getwd(), "/"))
dir_prefix = ifelse(path[length(path)] == "R", "../", "./")
r_dir_prefix = ifelse(path[length(path)] == "mvpv", "R/", "./")
data_prefix = paste0(dir_prefix, "data/")

source(paste0(r_dir_prefix, "functions.R"))
source(paste0(r_dir_prefix, "global.R"))

resources = list(walltime = 24 * 60 * 60, memory = "24gb", nodes = 1L,
  measure.memory = TRUE)
options(batchtools.progress = FALSE)
pars = CJ(x = regime$name, year = c(1970, 1990))

## fit bivariate models
fit_bv_reg = makeRegistry("fit_bv_registry", packages = "party", seed = seed)
fit_bv_reg$cluster.functions = makeClusterFunctionsTORQUE("template.tmpl")
batchMap(estimate, x = pars$x, year = pars$year, more.args = list(covariates = FALSE))
batchExport(list(preprocess = preprocess, dir_prefix = dir_prefix,
  data_prefix = data_prefix, outcomes = outcomes,
  outcomes_1970 = outcomes_1970, explanatory = explanatory,
  regime = regime), reg = fit_bv_reg)
submitJobs(reg = fit_bv_reg, resources = resources)
waitForJobs(reg = fit_bv_reg)
fits_bv = reduceResultsList(reg = fit_bv_reg)
fits_bv = lapply(fits_bv, function(x) {
  design = uniformGrid(x@data@env$input, 25)
  data.table(do.call("rbind", predict(x, newdata = design)), design)
})
bv_plots = lapply(fits_bv, plot_bivariate, label = "Bivariate Prediction", separate = FALSE)
write_figures(unlist(bv_plots, FALSE), pars, label = "bv")
write_results(fits_bv, pars, "fit_bv")

## fit models
fit_reg = makeRegistry("fit_registry", packages = "party", seed = seed)
fit_reg$cluster.functions = makeClusterFunctionsTORQUE("template.tmpl")
batchMap(estimate, x = pars$x, year = pars$year)
batchExport(list(preprocess = preprocess, dir_prefix = dir_prefix,
  data_prefix = data_prefix, outcomes = outcomes,
  outcomes_1970 = outcomes_1970, explanatory = explanatory,
  regime = regime), reg = fit_reg)
submitJobs(reg = fit_reg, resources = resources)
waitForJobs(reg = fit_reg)
fits = reduceResultsList(findDone(reg = fit_reg), reg = fit_reg)
write_results(fits, pars, "fit")

## fit/predict multi target models for comparison
## for each start year and regime variable combination
## iteratively hold out each year from start year + 1 to 2008
## and compute predictions for that year 
pred_pars = rbind(
  CJ(x = regime$name, year = 1970, hold_out = 1971:2008),
  CJ(x = regime$name, year = 1990, hold_out = 1991:2008)
)

fit_multi_reg = makeRegistry("fit_multi_registry", packages = pkgs, seed = seed)
fit_multi_reg$cluster.functions = makeClusterFunctionsTorque("template.tmpl")
batchMap(estimate, x = pred_pars$x, year = pred_pars$year, hold_out = pred_pars$hold_out)
submitJobs(reg = fit_multi_reg, resources = resources)
waitForJobs(reg = fit_multi_reg)
fits_multi = reduceResultsList(reg = fit_multi_reg)
## stack using start year and regime
write_results(fits_multi, pred_pars, "fits_multi")

## fit/predict single target models for comparison
fit_single_reg = makeRegistry("fit_single_registry", packages = pkgs, seed = seed)
fit_single_reg$cluster.functions = makeClusterFunctionsTorque("template.tmpl")
batchMap(estimate, x = pred_pars$x, year = pred_pars$year, hold_out = pred_pars$hold_out,
  more.args = list(multivariate = FALSE))
submitJobs(reg = fit_single_reg, resources = resources)
waitForJobs(reg = fit_single_reg)
fits_single = reduceResultsList(reg = fit_single_reg)
write_results(fits_single, pred_pars, "fits_single")

## compute errors and create a comparison plot
data = fread(paste0(dir_prefix, "data/1970_2008_rep.csv")) %>%
  mutate(max_hostlevel.use.of.force = ifelse(max_hostlevel == "use of force", 1, 0)) %>%
  select(one_of(c(outcomes$name, "year", "ccode")))

comp = lapply(1:nrow(pred_pars), function(i) {
  single = data.table(fits_single[[i]],
    data[year == pred_pars$hold_out[i], "ccode", with = FALSE])
  multi = data.table(fits_multi[[i]],
    data[year == pred_pars$hold_out[i], "ccode", with = FALSE])
  preds = merge(melt(single, id.vars = "ccode", value.name = "single.pred"),
    melt(multi, id.vars = "ccode", value.name = "multi.pred"), by = c("ccode", "variable"))
  obs = melt(data[year == pred_pars$hold_out[i],
    colnames(data) %in% c(outcomes$name, "ccode"), with = FALSE],
    id.vars = "ccode", value.name = "obs")
  merge(obs, preds, by = c("ccode", "variable")) %>%
    group_by(variable) %>%
    summarize("single.error" = mean(abs(single.pred - obs), na.rm = TRUE),
      "multi.error" = mean(abs(multi.pred - obs), na.rm = TRUE)) %>%
    data.table("year" = pred_pars$hold_out[i])
})
comp = rbindlist(comp)

ggplot(melt(comp, id.vars = c("variable", "year"),
  variable.name = "method"), aes(year, value, color = method)) + geom_smooth(se = FALSE) +
  facet_wrap(~ variable, scales = "free")



## univariate partial dependence
pd_reg = makeRegistry("pd_registry", packages = "mmpf", seed = seed)
pd_reg$cluster.functions = makeClusterFunctionsTORQUE("template.tmpl")
batchExport(list(dir_prefix = dir_prefix), reg = pd_reg)
batchMap(univariate_pd, x = pars$x, year = pars$year,
  more.args = list(n = c(10, NA), p = .1), reg = pd_reg)
submitJobs(resources = resources, reg = pd_reg)
waitForJobs(reg = pd_reg)
pd = reduceResultsList(findDone(reg = pd_reg), reg = pd_reg)
write_results(pd, pars[unlist(findDone(reg = pd_reg), )], "pd")
for (i in 1:length(pd)) ## single file per combination
  plot_bivariate(pd[[i]], single = TRUE, pars$year[i])

pd_plots = unlist(lapply(pd, plot_bivariate, separate = FALSE, single = FALSE), FALSE)
write_figures(pd_plots, pars, "pd")

pd_plots = lapply(pd, plot_bivariate, separate = TRUE)
pd_plots = unlist(pd_plots, FALSE)
pars = pars[rep(1:nrow(pars), each = 2), ]
pars$type = rep(c("main", "secondary"), nrow(pars) / 2)
write_figures(pd_plots, pars, "pd")

## bivariate partial dependence
pd_int_reg = makeRegistry("pd_int_registry", packages = "mmpf", seed = seed)
pd_int_reg$cluster.functions = makeClusterFunctionsTORQUE("template.tmpl")
batchExport(list(dir_prefix = dir_prefix), reg = pd_int_reg)
## not computing all of them right now
## pars = CJ(x = regime$name, year = c(1970, 1990), z = explanatory$name)
int_pars = CJ(x = regime$name, year = 1970, z = "year")
batchMap(bivariate_pd, x = int_pars$x, year = int_pars$year, z = int_pars$z,
  more.args = list(n = c(10, NA), p = .05), reg = pd_int_reg)
submitJobs(resources = resources, reg = pd_int_reg)
waitForJobs(reg = pd_int_reg)
pd_int = reduceResultsList(reg = pd_int_reg)
pd_int_plots = unlist(lapply(pd_int, plot_trivariate), FALSE)

## create some summary plots of the data
data = list(
  df_1990_2008 = fread(paste0(dir_prefix, "data/1990_2008_rep.csv")),
  df_1970_2008 = fread(paste0(dir_prefix, "data/1970_2008_rep.csv"))
)

## regime measure density plot
invisible(lapply(data, regime_density))

## outcome variable correlation heatmap
invisible(lapply(data, function(x)
  outcome_cor(x[, which(colnames(x) %in% c(outcomes$name, "year"))])))
