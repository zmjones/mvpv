regime_density <- function(data) {
  ## estimates univariate densities for regime variables and plots by data (1990 or 1970)
  plt <- melt(data[, -which(colnames(data) == "year")], id.vars = NULL, na.rm = TRUE)
  plt$value <- as.numeric(plt$value)
  p <- ggplot(plt, aes(value)) + stat_density() + facet_wrap(~ variable, scales = "free")
  ggsave(paste0(dir_prefix, "figures/", min(data$year), "_", max(data$year), "_",
    "regime_variables_density.png"), width = 10, height = 8)
}

outcome_cor <- function(x) {
  ## estimates hetcor matrix for predictors
  library("polycor")
  x <- data.table(x)
  min_year <- min(x$year)
  if (min_year < 1990)
    outcomes <- outcomes[!grepl("protest|osv_deaths|nsv_deaths", outcomes$name), ]
  x <- x[, colnames(x) %in% outcomes$name, with = FALSE]
  setnames(x, colnames(x), outcomes$label[match(colnames(x), outcomes$name)])
  plt <- melt(hetcor(as.data.frame(x), use = "pairwise.complete.obs")$correlations)
  p <- ggplot(plt, aes(Var1, Var2, fill = value)) +
    geom_tile() +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white",
      name = "Heterogeneous\nCorrelation") +
    labs(x = NULL, y = NULL) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  ggsave(paste0(dir_prefix, "figures/cor_", min_year, ".png"), width = 10, height = 8)
}

preprocess <- function(df, regime_variables) {
  ## preprocesses data before models are fit
  df$latent_mean <- df$latent_mean * -1 ## rescale so that higher = more abuse
  df$xpolity <- factor(df$xpolity, ordered = FALSE)
  df$xpolity_nas <- factor(df$xpolity_nas, ordered = TRUE)

  id <- c("ccode", "year")
  ## this is separate from the labels in global.R because we only display
  ## one level of the mid max_hostlevel variable, otherwise should be the same

  ## exclude some outcome variables if the data start in 1970
  if (min(df$year) < 1990)
    outcomes <- outcomes_1970$name
  else
    outcomes <- outcomes$name
  
  outcomes <- str_replace(outcomes, ".use.of.force", "")

  ## write any obs. that have missings on the outcomes to file
  ## missingness in predictors is *not* dropped
  dropped_idx <- apply(df[, outcomes], 1, function(x) any(is.na(x)))
  write.csv(df[dropped_idx, ],
    paste0(dir_prefix, "data/", "dropped_obs_", min(df$year), ".csv"),
    row.names = FALSE)
  df <- df[!dropped_idx, ]

  ## set up hyperparameters and tree-subsampling weights
  control <- cforest_unbiased(mtry = 3, ntree = 1000, trace = FALSE)
  ## randomly draw a subsample of the countries, include their time-series
  ccodes <- unique(df$ccode)
  weights <- sapply(1:control@ntree, function(x) {
    tab <- table(sample(ccodes, length(ccodes), TRUE))
    out <- tab[match(df$ccode, names(tab))]
  })
  weights[is.na(weights)] <- 0

  list(df = df,
    control = control,
    weights = weights,
    outcomes = outcomes)
}

expand_years <- function(df, idx = c(2, 3)) {
  ## expand system membership data to country-year panel
  year <- apply(df, 1, function(x) seq(x[idx[1]], x[idx[2]]))
  unit <- vector("list", nrow(df))
  for (i in 1:nrow(df))
    unit[[i]] <- cbind(df[rep(i, length(year[[i]])), ], year[[i]])
  df <- do.call("rbind", unit)
  colnames(df)[grepl("year", colnames(df))] <- "year"
  row.names(df) <- NULL
  df[, -idx]
}

estimate <- function(year, x, covariates = TRUE, hold_out = NULL,
                     multivariate = TRUE) {
  ## load and preprocess the appropriate data
  data <- read.csv(paste0(data_prefix, year, "_2008_rep.csv")) %>%
    preprocess(regime$name)

  ## find indices for training/holdout data if applicable
  idx <- rep(TRUE, nrow(data$df))
  if (!is.null(hold_out)) {
    idx <- data$df$year < hold_out
  }

  if (multivariate) {
    ## create the formula
    form <- paste0(paste0(data$outcomes, collapse = "+"), "~",
      paste0(c(x, if (covariates) explanatory$name), collapse = "+"))

    ## fit multivariate outcome model and return
    fit <- cforest(as.formula(form),
      data = data$df[idx, ],
      weights = data$weights[idx, ],
      controls = data$control)

    if (!is.null(hold_out)) {
      do.call("rbind", predict(fit, newdata = data$df[data$df$year == hold_out, ]))
    }
  } else {
    fit <- lapply(data$outcomes,
      function(y) {
        ## create the formula
        form <- paste0(y, "~",
          paste0(c(x, if (covariates) explanatory$name), collapse = "+"))

        ## fit univariate outcome model and return
        cforest(as.formula(form), data = data$df[idx, ],
          weights = data$weights[idx, ],
          controls = data$control)
      }
    )
    if (!is.null(hold_out)) {
      do.call("cbind",
        lapply(fit, function(m) {
          if (m@responses@is_ordinal) {
            do.call("rbind",
              predict(m, newdata = data$df[data$df$year == hold_out, ], type = "prob"))
          } else {
            predict(m, newdata = data$df[data$df$year == hold_out, ])
          }
        })
      )
    }
  }
}

contrast_error <- function(x, year, hold_out) {
  load(paste0(dir_prefix, "results/fits_single_", x, "_", year, "_", hold_out, ".RData"))
  single <- tmp
  load(paste0(dir_prefix, "results/fits_multi_", x, "_", year, "_", hold_out, ".RData"))
  multi <- tmp

  data <- read.csv(paste0("../data/", year, "_2008_rep.csv"),
    stringsAsFactors = TRUE)
  data <- preprocess(data, regime$name)$df
  data <- data[data$year == hold_out, ]

  single <- compute_error(data, single)
  single$method <- "individual models"
  multi <- compute_error(data, multi)
  multi$method <- "multivariate model"
  plt <- rbind(sing, multi)

  p <- ggplot(plt, aes(year, med, color = method)) +
    geom_point() +
    geom_errorbar(aes(ymin = lwr, ymax = upr)) +
    facet_wrap(~ variable, scales = "free_y")
  ## save plot
}

compute_error <- function(data, preds, contrast = function(x, y) abs(x - y)) {
  ## compute mean error using contrast function for each outcome variable
  preds <- preds %>%
    select(one_of(outcomes$name)) %>%
    rename(use.of.force = max_hostlevel.use.of.force)

  data <- data %>%
    mutate(use.of.force = ifelse(max_hostlevel == "use of force", 1, 0)) %>%
    select(one_of(c(colnames(preds), "year")))

  errors <- as.data.frame(matrix(NA, nrow(preds), ncol(preds)))
  colnames(errors) <- colnames(preds)

  for (x in colnames(preds)) {
    errors[, x] <- contrast(preds[, x], data[, x])
  }
  errors$year <- data$year

  ## compute .25, .5, and .75 quantiles for abs errors
  errors %>%
    gather(variable, error, -year) %>%
    group_by(year, variable) %>%
    summarise(
      lwr = quantile(error, .25),
      med = quantile(error, .5),
      upr = quantile(error, .75)) %>%
    ungroup()
}

univariate_pd <- function(x, year, n) {
  ## estimate univariate partial dependence
  load(paste0(dir_prefix, "results/fit_", x, "_", year, ".RData"))
  data <- tmp@data@env$input
  n[2] <- nrow(data)

  if (is.factor(data[[x]]) || all(round(data[[x]], 0) == data[[x]]))  {
    points <- list(na.omit(unique(data[[x]])))
    n[1] <- NA
  } else {
    points <- list(seq(min(data[[x]], na.rm = TRUE), max(data[[x]], na.rm = TRUE),
      length.out = n[1]))
  }
  names(points) <- x
  marginalPrediction(data, x, n, tmp, points = points, aggregate.fun = mean,
    predict.fun = function(object, newdata)
      do.call("rbind", object@predict_response(newdata)))
}

bivariate_pd <- function(x, z, year, n) {
  ## estimate bivariate partial dependence
  load(paste0(dir_prefix, "results/fit_", x, "_", year, ".RData"))
  data <- tmp@data@env$input
  n[2] <- nrow(data)
  points <- vector("list", 2L)
  names(points) <- c(x, z)

  for (i in 1:length(points)) {
    if (is.factor(data[[names(points)[i]]])) {
      points[[i]] <- na.omit(unique(data[[names(points)[i]]]))
    } else {
      idx = !is.na(data[[names(points)[i]]])
      if (all(round(data[idx, names(points)[i]]) == data[idx, names(points)[i]])) {
        points[[i]] <- unique(data[idx, names(points)[i]])
      } else {
      points[[i]] <- seq(min(data[idx, names(points)[i]]),
        max(data[idx, names(points)[i]]),
        length.out = n[1])
      }
    }
  }
  n[1] <- NA

  marginalPrediction(data, c(x, z), n, tmp, points = points, aggregate.fun = mean,
    predict.fun = function(object, newdata)
      do.call("rbind", object@predict_response(newdata)))
}

write_results <- function(res, pars, prefix) {
  ## write partial dependence results lists to file
  for (i in 1:length(res)) {
    tmp <- res[[i]]
    save(tmp, file = paste0(dir_prefix,
      "results/", prefix, "_", paste0(pars[i, ], collapse = "_"), ".RData"))
  }
  NULL
}
