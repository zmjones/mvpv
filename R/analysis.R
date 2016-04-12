seed <- 1987
set.seed(seed)

## load and preprocess data
pkgs <- c("party", "edarf", "plyr", "dplyr", "reshape2", "stringr", "ggplot2", "foreach", "doParallel", "parallel")
invisible(sapply(pkgs, library, character.only = TRUE))

## use cli arg for this
cl <- makePSOCKcluster(5)
registerDoParallel(cl)

path <- unlist(str_split(getwd(), "/"))
dir_prefix <- ifelse(path[length(path)] == "R", "../", "./")
r_dir_prefix <- ifelse(path[length(path)] == "mvpv", "R/", "./")

source(paste0(r_dir_prefix, "functions.R"))

df <- read.csv(paste0(dir_prefix, "data/rep.csv"), stringsAsFactors = TRUE)
df$latent_mean <- df$latent_mean * -1 ## rescale so that higher = more abuse
ciri_levels <- c("none", "occasional", "frequent")
ciri_vars <- c("disap", "tort", "kill", "polpris")
for (x in ciri_vars)
  df[, x] <- factor(df[, x], levels = ciri_levels, ordered = TRUE)
df$xpolity <- factor(df$xpolity, ordered = TRUE)
df$xpolity_nas <- factor(df$xpolity_nas, ordered = TRUE)

id <- c("ccode", "year")
outcomes <- c("cwar", "max_hostlevel", "ns_fat", "osv_fat", "latent_mean",
              "terror_killed", "terror_events", "nonviolent_protest", "violent_protest")
regime_variables <- c("part", "xpolity", "xpolity_nas", "uds_xpolity")
explanatory_variables <- c("year", "rgdppc", "pop", "exclpop", "oilpc", "ethfrac", "newstate")

dropped <- df[apply(df[, outcomes], 1, function(x) any(is.na(x))), ]
write.csv(dropped, paste0(dir_prefix, "data/", "dropped_obs.csv"), row.names = FALSE)
df <- df[apply(df[, outcomes], 1, function(x) !any(is.na(x))), ]

## set up arguments for estimation
## no tuning for now
## stratified bootstrap by country
control <- cforest_unbiased(mtry = 3, ntree = 500, trace = FALSE)
ccodes <- unique(df$ccode)
weights <- sapply(1:control@ntree, function(x) {
  tab <- table(sample(ccodes, length(ccodes), TRUE))
  out <- tab[match(df$ccode, names(tab))]
})
weights[is.na(weights)] <- 0

out <- foreach(x = regime_variables, .packages = c("party", "edarf", "reshape2")) %dopar% {
  form <- paste0(paste0(outcomes, collapse = "+"), "~", paste0(c(explanatory_variables, x), collapse = "+"))
  fit <- cforest(as.formula(form), data = df, weights = weights, controls = control)
  pd <- partial_dependence(fit, df, var = x, cutoff = 20)
  pd_int <- partial_dependence(fit, df, var = c(x, "year"), cutoff = 20, interaction = TRUE)
  list(
    "fit" = fit,
    "pd" = pd, "pd_int" = pd_int,
    "plt" = melt(pd, id.vars = x, variable.name = "outcome"),
    "plt_int" = melt(pd_int, id.vars = c(x, "year"), variable.name = "outcome")
  )
}
names(out) <- regime_variables
save(out, file = "../output/results.RData")
stopCluster(cl)

invisible(lapply(out, function(x) {
  plt <- x$plt
  var <- colnames(plt)[1]
  var_label <- switch(var,
                      "xpolity_nas" = "X-Polity IV (Vreeland)",
                      "xpolity" = "X-Polity IV (Vreeland, NA)",
                      "part" = "Polyarchy",
                      "geddes" = "Authoritarian Regimes",
                      "uds_xpolity" = "Unified Democracy Scores (X-Polity Version)")
  plt <- plt[plt$outcome %in% c("civil.war", "ns_fat", "osv_fat", "latent_mean", "terror_killed",
                                "terror_events", "nonviolent_protest", "violent_protest",
                                "use.of.force"), ]
  plt$outcome <- as.character(plt$outcome)
  plt <- relabel_outcomes(plt, "outcome")
  plt[[var]] <- as.numeric(as.character(plt[[var]]))
  if (var == "xpolity") {
    aggr <- plt %>% group_by(outcome) %>% filter(xpolity < -6) %>%
      summarise(value = mean(value), xpolity = "observed")
    aggr <- aggr[, c("xpolity", "outcome", "value")]
    plt <- rbind(aggr, filter(plt, xpolity < -6))
  }
  p <- ggplot(plt, aes_string(var, "value", group = "outcome"))
  p <- p + geom_point() + geom_line()
    p <- p + geom_line()
  p <- p + facet_wrap(~ outcome, scales = "free_y")
  p <- p + xlab(var_label) + ylab("Partial Prediction")
  ggsave(paste0("../figures/", var, ".png"), width = 11, height = 8)

  plt_int <- x$plt_int
  plt_int <- plt_int[plt_int$outcome %in% c("civil.war", "ns_fat", "osv_fat", "latent_mean", "terror_killed",
                                            "terror_events", "nonviolent_protest", "violent_protest",
                                            "use.of.force"), ]
  plt_int$outcome <- as.character(plt_int$outcome)
  plt_int <- relabel_outcomes(plt_int, "outcome")
  plt_int[[var]] <- as.numeric(as.character(plt_int[[var]]))

  if (var == "xpolity") {
    aggr <- plt_int %>% group_by(outcome, year) %>% filter(xpolity < -6) %>%
      summarise(value = mean(value), xpolity = "observed")
    aggr <- aggr[, c("xpolity", "year", "outcome", "value")]
    plt_int <- rbind(aggr, filter(plt_int, xpolity < -6))
  } 

  for (y in unique(plt_int$outcome)) {
    p <- ggplot(plt_int[plt_int$outcome == y, ], aes_string(var, "value"))
    p <- p + geom_point() + geom_line()
    p <- p + facet_wrap(~ year)
    p <- p + xlab(var_label) + ylab(y)
    ggsave(paste0("../figures/", relabel_outcomes(y, "", TRUE), "_", var, "_int_year.png"), width = 11, height = 8)

    p <- ggplot(plt_int[plt_int$outcome == y, ], aes_string(var, "year", z = "value"))
    p <- p + geom_tile(aes_string(fill = "value"))
    p <- p + scale_fill_gradient2(space = "Lab", name = y)
    p <- p + guides(fill = guide_colorbar(barwidth = .75, barheight = 10, ticks = FALSE))
    p <- p + xlab(var_label) + ylab("Year")
    ggsave(paste0("../figures/", relabel_outcomes(y, "", TRUE), "_", var, "_int_year_tile.png"),
           width = 11, height = 8)
  }
}))
