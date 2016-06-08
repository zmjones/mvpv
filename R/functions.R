regime_density <- function(data) {
  plt <- melt(data[, -which(colnames(data) == "year")], id.vars = NULL, na.rm = TRUE)
  plt$value <- as.numeric(plt$value)
  p <- ggplot(plt, aes(value)) + stat_density() + facet_wrap(~ variable, scales = "free")
  ggsave(paste0(dir_prefix, "figures/", min(data$year), "_", max(data$year), "_",
                "regime_variables_density.png"), width = 10, height = 8)
}

plot_univariate <- function(plt) {
  var <- colnames(plt)[1]
  var_label <- switch(var,
                      "xpolity_nas" = "X-Polity",
                      "xpolity" = "X-Polity",
                      "part" = "Polyarchy",
                      "geddes" = "Authoritarian Regimes",
                      "uds_xpolity" = "Unified Democracy Scores (X-Polity)")
  plt <- plt[plt$outcome %in% c("cwar", "cconflict", "ns_fat", "osv_fat", "latent_mean", "terror_killed",
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
  p <- p + geom_point()
  if (var != "xpolity")
    p <- p + geom_line()
  p <- p + facet_wrap(~ outcome, scales = "free_y")
  p <- p + xlab(var_label) + ylab("Partial Prediction")
  p <- p + theme_bw()

  ggsave(paste0(dir_prefix, "figures/", var, ".png"), width = 11, height = 8)

  for (y in unique(plt$outcome)) {
    p <- ggplot(plt[plt$outcome == y, ], aes_string(var, "value"))
    p <- p + geom_point()
    if (var != "xpolity")
      p <- p + geom_line()
    p <- p + xlab(var_label) + ylab(y)
    p <- p + theme_bw()
    ggsave(paste0(dir_prefix, "figures/", relabel_outcomes(y, "", TRUE), "_", var, ".png"), width = 11, height = 8)
  }
}

plot_bivariate <- function(plt) {
  plt_int$outcome <- as.character(plt_int$outcome)
  plt_int <- relabel_outcomes(plt_int, "outcome")
  plt_int[[var]] <- as.numeric(as.character(plt_int[[var]]))

  if (var == "xpolity") {
    aggr <- plt_int %>% group_by(outcome, year) %>% filter(xpolity < -6) %>%
      summarise(value = mean(value), xpolity = "observed")
    aggr <- aggr[, c("xpolity", "year", "outcome", "value")]
    plt_int <- rbind(aggr, filter(plt_int, xpolity < -6))
    plt_int[[var]] <- as.character(plt_int[[var]])
  }

  for (y in unique(plt_int$outcome)) {
    p <- ggplot(plt_int[plt_int$outcome == y, ], aes_string(var, "year", z = "value"))
    p <- p + geom_raster(aes_string(fill = "value"), interpolate = TRUE)
    p <- p + scale_fill_gradient(low = "white", high = "red", name = y)
    p <- p + guides(fill = guide_colorbar(barwidth = .75, barheight = 10, ticks = FALSE, raster = TRUE))
    p <- p + xlab(var_label) + ylab("Year")
    p <- p + theme_bw()
    ggsave(paste0(dir_prefix, "figures/", relabel_outcomes(y, "", TRUE), "_", var, "_int_year.png"),
           width = 11, height = 8)
  }
}


preprocess <- function(df, regime_variables) {
  df$latent_mean <- df$latent_mean * -1 ## rescale so that higher = more abuse
  ciri_levels <- c("none", "occasional", "frequent")
  ciri_vars <- c("disap", "tort", "kill", "polpris")
  for (x in ciri_vars)
    df[, x] <- factor(df[, x], levels = ciri_levels, ordered = TRUE)
  df$xpolity <- factor(df$xpolity, ordered = TRUE)
  df$xpolity_nas <- factor(df$xpolity_nas, ordered = TRUE)

  id <- c("ccode", "year")
  outcomes <- c("cwar_count", "cconflict_count", "cwar_onset", "cconflict_onset",
                "max_hostlevel", "latent_mean", "terror_killed", "terror_events",
                "nonviolent_protest", "violent_protest")
  if (min(df$year) < 1990) outcomes <- outcomes[!grepl("protest", outcomes)]

  dropped <- df[apply(df[, outcomes], 1, function(x) any(is.na(x))), ]
  write.csv(dropped, paste0(dir_prefix, "data/", "dropped_obs_", min(df$year), ".csv"), row.names = FALSE)
  df <- df[apply(df[, outcomes], 1, function(x) !any(is.na(x))), ]
  
  control <- cforest_unbiased(mtry = 3, ntree = 1000, trace = FALSE)
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

ccode_fix <- function(df) {
  df$ccode[df$year >= 1991 & df$year <= 2008 & df$ccode == 255] <- 260
  df$ccode[df$year >= 2006 & df$year <= 2008 & df$ccode == 340] <- 345
  df$ccode[df$year >= 1991 & df$year <= 2008 & df$ccode == 678] <- 670
  df
}

relabel_dataframe <- function(data, var, reverse = FALSE) {
  labs <- rbind(c("cwar", "Civil War (sum)"),
                c("cconflict", "Civil Conflict (sum)"),
                c("ns_fat", "Non-State Conflict Fatalities"),
                c("osv_fat", "One Sided Violence Fatalities"),
                c("latent_mean", "Respect for Physical Integrity\n (Fariss, posterior mean)"),
                c("terror_events", "Terrorist Attacks"),
                c("terror_killed", "Fatalities from Terrorist Attacks"),
                c("polpris", "Political Imprisonment"),
                c("disap", "Disappearances"),
                c("tort", "Torture"),
                c("kill", "Extrajudicial Killings"),
                c("max_hostlevel", "Maximum Hostility Level"),
                c("nonviolent_protest", "Non-Violent Protest"),
                c("violent_protest", "Violent Protest"),
                c("max_hostlevel.use.of.force", "Use of Force"),
                c("use.of.force", "Use of Force"),
                c("part", "Polyarchy"),
                c("xpolity", "X-Polity"),
                c("xpolity_nas", "X-Polity (NA)"),
                c("uds_xpolity", "Unified Democracy Scores (X-Polity)"),
                c("year", "Year"),
                c("gdppc", "GDP per Capita"),
                c("pop", "Population"),
                c("exclpop", "Excluded Population (%)"),
                c("oilpc", "Oil Exports (% of GDP)"),
                c("ethfrac", "Ethnic Fractionalization"),
                c("newstate", "Newly Independent"),
                c("durable", "Regime Durability"))
  if (reverse)
    labs <- labs[, c(2, 1)]
  labs <- as.data.frame(labs, stringsAsFactors = FALSE)
  colnames(labs) <- c("old", "new")
  if (is.data.frame(data)) {
    idx <- match(data[[var]], labs$old)
    new <- labs$new[idx]
    old <- labs$old[idx]
    data[data[[var]] == old, var] <- new
  } else {
    idx <- match(data, labs$old)
    new <- labs$new[idx]
    old <- labs$old[idx]
    data[data == old] <- new
  }
  data
}

relabel <- function(df, var, old, new) {
  df[df[[var]] == old, var] <- new
  df
}

ciri_grouper <- function(plt) {
  ciri_components <- c("polpris", "disap", "tort", "kill")
  ciri_matcher <- paste(paste0("^", ciri_components), collapse = "|")
  ciri_plt <- filter(plt, grepl(ciri_matcher, outcome))
  ciri_plt$outcome <- as.character(ciri_plt$outcome)
  ciri_plt$component <- str_extract(ciri_plt$outcome, ciri_matcher)
  ciri_plt$outcome <- str_replace_all(ciri_plt$outcome, ciri_matcher, "")
  ciri_plt$outcome <- str_replace_all(ciri_plt$outcome, "\\.| ", "")
  ciri_plt$outcome <- factor(ciri_plt$outcome, levels = c("none", "occasional", "frequent"),
                             labels = c("None", "Occasional", "Frequent"), ordered = TRUE)
  ciri_plt$value <- as.numeric(ciri_plt$value)
  relabel_outcomes(ciri_plt, "component")
}

mid_grouper <- function(plt) {
  mid_levels <- c("no dispute", "no militarized action", "threat to use force",
                  "display of force", "use of force", "war")
  mid_labels <- c("No Dispute", "No Militarized Action", "Threat to Use Force",
                  "Display of Force", "Use of Force", "War")
  mid_plt <- filter(plt, grepl("max_hostlevel", outcome))
  mid_plt$outcome <- str_replace_all(mid_plt$outcome, "max_hostlevel\\.", "")
  mid_plt$outcome <- str_replace_all(mid_plt$outcome, "\\.", " ")
  mid_plt <- mid_plt[mid_plt$outcome != "no dispute", ]
  mid_plt$outcome <- factor(mid_plt$outcome, levels = mid_levels, labels = mid_labels, ordered = TRUE)
  mid_plt
}

mse <- function(a, b) mean((a - b)^2)
mae <- function(a, b) mean(abs(a - b))
brier <- function(a, b) {
  if (is.matrix(a) & is.matrix(b))
    apply((a - b)^2, 2, mean)
  else
    mse(a, b)
}

read_csv <- function(dir_prefix, file) read.csv(paste0(dir_prefix, "data/", file), stringsAsFactors = TRUE)

expand_years <- function(df, idx = c(2, 3)) {
  year <- apply(df, 1, function(x) seq(x[idx[1]], x[idx[2]]))
  unit <- vector("list", nrow(df))
  for (i in 1:nrow(df))
    unit[[i]] <- cbind(df[rep(i, length(year[[i]])), ], year[[i]])
  df <- do.call("rbind", unit)
  colnames(df)[grepl("year", colnames(df))] <- "year"
  row.names(df) <- NULL
  df[, -idx]
}

dupes <- function(df) df[which(duplicated(df[, c("ccode", "year")]) |
                                 duplicated(df[, c("ccode", "year")], fromLast = TRUE)), ]

expand_ccodes <- function(df) {
  ccodes <- str_split(df$ccode, ", ")
  for (i in 1:length(ccodes)) {
    if (length(ccodes[[i]]) > 1) {
      out <- lapply(ccodes[[i]], function(x) {
        temp <- df[i, ]
        temp["ccode"] <- x
        temp
      })
      to_add <- do.call(rbind, out)
      df <- rbind(df, to_add)
    }
  }
  df <- df[-which(sapply(str_split(df$ccode, ", "), function(x) length(x) > 1)), ]
  df$ccode <- as.integer(df$ccode)
  df <- df[!is.na(df$ccode), ]
  df
}

miss_mapper <- function(df, var, label, outfile) {
  miss_map <- cbind(df[, c("cname", "year")], "var" = is.na(df[, var]))
  miss_map <- miss_map[miss_map$year >= 1945, ]
  p <- ggplot(miss_map, aes(year, cname))
  p <- p + geom_tile(aes(fill = var))
  p <- p + labs(x = "year", y = "state")
  p <- p + scale_fill_brewer(name = label, type = "qual")
  ggsave(paste0(dir_prefix, "figures/", outfile, ".png"), p, width = 10, height = 20)
}
