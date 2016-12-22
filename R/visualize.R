pkgs <- c("ggplot2", "stringr", "data.table", "gridExtra", "party")
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

invisible(lapply(data, function(x) regime_density(x[, c(regime$name, "year")])))
invisible(lapply(data, function(x)
  outcome_cor(x[, which(colnames(x) %in% c(outcomes$name, "year"))])))

for (i in 1:length(data)) {
  for (j in 1:length(regime$name)) {
    test <- data[[i]]
    test <- preprocess(data[[i]], regime$name[j])
    form <- paste0(paste0(test$outcomes, collapse = " + "), " ~ ", regime$name[j])
    fit = cforest(as.formula(form),
      data = test$df, controls = test$control,
      weights = test$weights)
    n <- c(10, NA)
    if (is.factor(test$df[[regime$name[j]]])) {
      points <- data.frame(na.omit(unique(test$df[[regime$name[j]]])))
    } else {
      if (all(round(test$df[[regime$name[j]]], 0) == test$df[[regime$name[j]]])) {
        points <- data.frame(na.omit(unique(test$df[[regime$name[j]]])))
      } else {
        points <- data.frame(seq(min(test$df[[regime$name[j]]], na.rm = TRUE),
          max(test$df[[regime$name[j]]], na.rm = TRUE),
          length.out = n[1]))
      }
    }
    names(points) <- regime$name[j]
    preds <- do.call("rbind", predict(fit, newdata = points))
    preds <- data.table(preds, points)
    preds <- preds[, colnames(preds) %in% all$name, with = FALSE]
    setnames(preds, colnames(preds), all$label[match(colnames(preds), all$name)])
    xvar <- colnames(preds)[colnames(preds) %in% regime$label]
    plt <- melt(preds, id.vars = xvar, variable.name = "Outcome",
      value.name = "Bivariate Model Prediction")
    xvar <- paste0("`", xvar, "`")
    p <- ggplot(plt, aes_string(xvar, "`Bivariate Model Prediction`", group = 1)) +
      geom_point(stat = 'summary', fun.y = sum)
    if (regime$name[j] != "xpolity")
      p <- p + stat_summary(fun.y = sum, geom = "line")
    else
      p <- p + geom_vline(aes(xintercept = 4), linetype = "dashed")

    fname <- paste("bv", regime$name[j],
      str_extract(names(data)[i], "[0-9]{4}"), sep = "_")
    fpath <- paste0(dir_prefix, "figures/", fname, ".png")
    if (grepl("1990", names(data)[i])) {
      p <- p + facet_wrap(~ Outcome, nrow = 3, ncol = 4, scales = "free_y")
      ggsave(fpath, p, width = 12, height = 6)
    } else {
      p <- p + facet_wrap(~ Outcome, nrow = 2, ncol = 4, scales = "free_y")
      ggsave(fpath, p, width = 12, height = 4)
    }
  }
}

files <- dir(paste0(dir_prefix, "results/"))
files <- files[str_detect(files, "^pd*")]

pd_interaction_files <- files[str_detect(files, "int")]
pd_univariate_files <- files[!files %in% pd_interaction_files]

invisible(lapply(pd_univariate_files, function(x) {
  load(paste0(dir_prefix, "results/", x))
  pd <- data.table(tmp)
  pd <- pd[, colnames(pd) %in% all$name, with = FALSE]
  setnames(pd, colnames(pd), all$label[match(colnames(pd), all$name)])
  xvar <- colnames(pd)[colnames(pd) %in% regime$label]
  plt <- melt(pd, id.vars = xvar,
    variable.name = "Outcome", value.name = "Partial Dependence")
  xvar <- paste0("`", xvar, "`")
  p <- ggplot(plt, aes_string(xvar, "`Partial Dependence`", group = 1)) +
    geom_point(stat = 'summary', fun.y = sum)

  if (xvar != "`X-Polity (w/ missing categories)`")
    p <- p + stat_summary(fun.y = sum, geom = "line")
  else
    p <- p + geom_vline(aes(xintercept = 4), linetype = "dashed")
    
  fname <- str_replace(x, "\\.RData", "\\.png")
  fpath <- paste0(dir_prefix, "figures/", fname)
  if ("1990" %in% x) {
    p <- p + facet_wrap(~ Outcome, nrow = 2, ncol = 4, scales = "free_y")
    ggsave(fpath, p, width = 12, height = 4)
  } else {
    p <- p + facet_wrap(~ Outcome, nrow = 3, ncol = 4, scales = "free_y")
    ggsave(fpath, p, width = 12, height = 6)
  }
}))

invisible(lapply(pd_interaction_files, function(x) {
  load(paste0(dir_prefix, "results/", x))
  pd <- data.table(tmp)
  pd <- pd[, colnames(tmp) %in% all$name, with = FALSE]
  setnames(pd, colnames(pd),
    all$label[match(colnames(pd), all$name)])
  xvar <- colnames(pd)[colnames(pd) %in% regime$label]
  yvar <- colnames(pd)[colnames(pd) %in% explanatory$label]
  p <- lapply(colnames(pd)[colnames(pd) %in% outcomes$label],
    function(z) {
      plt <- pd[, c(z, xvar, yvar), with = FALSE]
      ggplot(plt, aes_string(paste0("`", xvar, "`"),
        paste0("`", yvar, "`"), fill = paste0("`", z, "`"))) +
        geom_raster() + theme(legend.position = "bottom")
    })
  fname <- paste("pd_int", str_extract(x, "\\d{4}"),
    paste0(names(tmp)[(ncol(tmp) - 1):ncol(tmp)], collapse = "_"), sep = "_")
  fname <- paste0(fname, ".png")
  fpath <- paste0(dir_prefix, "figures/", fname)
  
  png(fpath, width = 14, height = 12, units = "in", res = 400)
  do.call(grid.arrange, p)
  dev.off()
}))
