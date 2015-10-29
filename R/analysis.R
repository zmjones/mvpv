set.seed(1987)

## read command line args (set in makefile) or set defaults
args <- commandArgs(TRUE)
cores <- as.integer(args[1])
if (is.na(cores)) {
  warning("cores argument not set, default is 8")
  cores <- 8
}

## load and preprocess data
pkgs <- c("party", "edarf",
          "foreach", "parallel", "doParallel", "plyr",
          "dplyr", "tidyr", "stringr", "ggplot2", "stringr")
invisible(sapply(pkgs, library, character.only = TRUE))

path <- unlist(str_split(getwd(), "/"))
dir_prefix <- ifelse(path[length(path)] == "R", "../", "./")

read_csv <- function(file) read.csv(paste0(dir_prefix, "data/", file), stringsAsFactors = TRUE)

df <- read_csv("rep.csv")
df$latent_mean <- df$latent_mean * -1 ## rescale so that higher = more abuse
ciri_levels <- c("none", "occasional", "frequent")
ciri_vars <- c("disap", "tort", "kill", "polpris")
for (x in ciri_vars)
  df[, x] <- factor(df[, x], levels = ciri_levels, ordered = TRUE)

id <- c("ccode", "year")
outcomes <- c("cwar", "max_hostlevel", "ns_fat", "osv_fat", "latent_mean",
              "terror_killed", "terror_events", "disap", "tort", "kill", "polpris")
polity_variables <- c("xrcomp", "xropen", "xconst", "parcomp", "xrreg", "parreg")
regime_variables <- c("comp", "uds", "geddes")
explanatory_variables <- c("year", "rgdppc", "pop", "exclpop", "oilpc", "ethfrac", "newstate")

dropped <- df[apply(df[, outcomes], 1, function(x) any(is.na(x))), ]
write.csv(dropped, paste0(dir_prefix, "data/", "dropped_obs.csv"), row.names = FALSE)
df <- df[apply(df[, outcomes], 1, function(x) !any(is.na(x))), ]

## set up arguments for estimation
## no tuning for now
## stratified bootstrap by country
control <- cforest_unbiased(mtry = 3, ntree = 1000, trace = FALSE)
ccodes <- unique(df$ccode)
weights <- sapply(1:control@ntree, function(x) {
  tab <- table(sample(ccodes, length(ccodes), TRUE))
  out <- tab[match(df$ccode, names(tab))]
})
weights[is.na(weights)] <- 0

## psock cluster for hpc
registerDoParallel(makeCluster(cores))

fitter <- function(outcomes, explanatory_variables, regime_variables, weights, control) {
  form <- paste0(paste0(outcomes, collapse = "+"), "~",
                 paste0(c(explanatory_variables,
                          regime_variables), collapse = "+"))
  cforest(as.formula(form), data = df, weights = weights, controls = control)
}

## estimate and compute partial dependence for each regime type variable on all outcomes
uds_fit <- fitter(outcomes, explanatory_variables, "uds", weights, control)
uds_pd <- partial_dependence(uds_fit, df, var = "uds", parallel = TRUE, clean_names = FALSE)
save(uds_pd, file = paste0(dir_prefix, "output/uds_pd.rda"))

polity_fit <- fitter(outcomes, explanatory_variables, polity_variables, weights, control)
xrcomp_pd <- partial_dependence(polity_fit, var = "xrcomp", parallel = TRUE, clean_names = FALSE)
xropen_pd <- partial_dependence(polity_fit, var = "xropen", parallel = TRUE, clean_names = FALSE)
xconst_pd <- partial_dependence(polity_fit, var = "xconst", parallel = TRUE, clean_names = FALSE)
parcomp_pd <- partial_dependence(polity_fit, var = "parcomp", parallel = TRUE, clean_names = FALSE)
parreg_pd <- partial_dependence(polity_fit, var = "parreg", parallel = TRUE, clean_names = FALSE)
xrreg_pd <- partial_dependence(polity_fit, var = "xrreg", parallel = TRUE, clean_names = FALSE)
save(xrcomp_pd, xropen_pd, xconst_pd, parcomp_pd,
     parreg_pd, xrreg_pd, file = paste0(dir_prefix, "output/polity_pd.rda"))

comp_fit <- fitter(outcomes, explanatory_variables, "comp", weights, control)
comp_pd <- partial_dependence(comp_fit, var = "comp", parallel = TRUE, clean_names = FALSE)
save(comp_pd, file = paste0(dir_prefix, "output/comp_pd.rda"))

geddes_fit <- fitter(outcomes, explanatory_variables, "geddes", weights, control)
geddes_pd <- partial_dependence(geddes_fit, var = "geddes", parallel = TRUE, clean_names = FALSE)
save(geddes_pd, file = paste0(dir_prefix, "output/geddes_pd.rda"))

## clean pd output for plots
uds_plt <- gather(uds_pd, outcome, value, -uds)
xrcomp_plt <- gather(xrcomp_pd, outcome, value, -xrcomp)
xropen_plt <- gather(xropen_pd, outcome, value, -xropen)
xconst_plt <- gather(xconst_pd, outcome, value, -xconst)
parcomp_plt <- gather(parcomp_pd, outcome, value, -parcomp)
parreg_plt <- gather(parreg_pd, outcome, value, -parreg)
xrreg_plt <- gather(xrreg_pd, outcome, value, -xrreg)
comp_plt <- gather(comp_pd, outcome, value, -comp)
geddes_plt <- gather(geddes_pd, outcome, value, -geddes)

## todos
## need to do more processessing for polity/geddes
## relabel values, order them intelligently, no lines (or partial lines)
## need to create polity components with the uncoded values set to be missing
## probably as well

relabel_outcomes <- function(df, var) {
  df[[var]][df[[var]] == "cwar"] <- "Civil War"
  df[[var]][df[[var]] == "cwar.civil.war"] <- "Civil War"
  df[[var]][df[[var]] == "ns_fat"] <- "Non-State Conflict (fatalities, UCDP)"
  df[[var]][df[[var]] == "osv_fat"] <- "One Sided Violence (fatalities, UCDP)"
  df[[var]][df[[var]] == "latent_mean"] <- "Latent Respect for Physical Integrity (posterior mean)"
  df[[var]][df[[var]] == "terror_events"] <- "Terrorist Attacks (GTD)"
  df[[var]][df[[var]] == "terror_killed"] <- "Fatalities from Terrorist Attacks (GTD)"
  df[[var]][which(df[[var]] == "polpris")] <- "Political Imprisonment (CIRI)"
  df[[var]][which(df[[var]] == "disap")] <- "Disappearances (CIRI)"
  df[[var]][which(df[[var]] == "tort")] <- "Torture (CIRI)"
  df[[var]][which(df[[var]] == "kill")] <- "Extrajudicial Killings (CIRI)"
  df[[var]][which(df[[var]] == "max_hostlevel")] <- "Maximium Hostility Level (MID)"
  df
}

relabel_regimetype <- function(df, var) {
  df[[var]][df[[var]] == "uds"] <- "UDS (posterior mean)"
  df[[var]][df[[var]] == "polity"] <- "Polity IV"
  df[[var]][df[[var]] == "comp"] <- "Polyarchy"
  df[[var]][df[[var]] == "geddes"] <- "Authoritarian Regimes"
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

make_plots <- function(plt, xlab, var, ordered = TRUE) {
  fact <- is.factor(plt[, var])
  if (fact) {
    plt[[var]] <- factor(plt[[var]])
    plt$x <- as.integer(plt[, var])
    xvals <- sort(unique(plt$x))
  } else plt$x <- plt[, var]
  
  ciri_plt <- ciri_grouper(plt)
  p <- ggplot(ciri_plt, aes_string("x", "value", color = "outcome")) +
    facet_wrap(~ component, scales = "free_y", ncol = 4) + geom_point() +
    labs(x = xlab, y = "Partial Predicted Probability") + theme_bw()
  if (ordered) p <- p + geom_line()

  if (fact)  {
    p <- p + scale_x_continuous(breaks = xvals, labels = levels(plt[, var]))
    p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  }
  ggsave(paste0(dir_prefix, "figures/ciri_", var, ".png"), width = 10, height = 4)

  mid_plt <- mid_grouper(plt)
  p <- ggplot(mid_plt, aes_string("x", "value", color = "outcome")) +
    geom_point() + 
    labs(x = xlab, y = "Partial Predicted Probability") + theme_bw()

  if (ordered) p <- p + geom_line()
  
  if (fact)  {
    p <- p + scale_x_continuous(breaks = xvals, labels = levels(plt[, var]))
    p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  }
  ggsave(paste0(dir_prefix, "figures/mid_", var, ".png"), width = 8, height = 5)

  other_plt <- filter(plt, outcome %in% c("ns_fat", "osv_fat", "cwar.civil.war", "latent_mean",
                                          "terror_killed", "terror_events"))
  other_plt$outcome <- as.character(other_plt$outcome)
  other_plt <- relabel_outcomes(other_plt, "outcome")

  p <- ggplot(other_plt, aes_string("x", "value", group = "outcome")) +
    facet_wrap(~ outcome, ncol = 3, scales = "free_y") +
    geom_point() + 
    labs(x = xlab, y = "Partial Prediction") + theme_bw()

  if (ordered) p <- p + geom_line()
  
  if (fact)  {
    p <- p + scale_x_continuous(breaks = xvals, labels = levels(plt[, var]))
    p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  }
  ggsave(paste0(dir_prefix, "figures/other_", var, ".png"), width = 12, height = 8)
}

xrcomp_plt$xrcomp <- factor(xrcomp_plt$xrcomp,
                            labels = c("Unregulated", "Selection", "Dual/Transitional", "Election"),
                            ordered = TRUE)
xropen_plt$xropen <- factor(xropen_plt$xropen,
                            labels = c("Unregulated", "Closed", "Dual Executive-Designation",
                                       "Dual Executive-Election", "Open"),
                            ordered = TRUE)
xrreg_plt$xrreg <- factor(xrreg_plt$xrreg,
                          labels = c("Selection", "Dual/Transitional", "Election"),
                          ordered = TRUE)
xconst_plt$xconst <- factor(xconst_plt$xconst,
                            labels = c("Unregulated", "Unlimited", "Intermediate 1", "Slight/Moderate",
                                       "Intermediate 2", "Substantial", "Parity/Subordination"),
                            ordered = TRUE)
parcomp_plt$parcomp <- factor(parcomp_plt$parcomp,
                              labels = c("N/A", "Repressed", "Suppressed", "Factional",
                                         "Transitional", "Competitive"),
                              ordered = TRUE)
parreg_plt$parreg <- factor(parreg_plt$parreg,
                            labels = c("Unregulated", "Multiple Identity", "Sectarian", "Restricted", "Regulated"),
                            ordered = TRUE)

make_plots(uds_plt, "Unified Democracy Score (posterior mean)", "uds")
make_plots(xrcomp_plt, "Executive Competition", "xrcomp")
make_plots(xropen_plt, "Executive Openness", "xropen")
make_plots(xrreg_plt, "Executive Regulation", "xrreg")
make_plots(xconst_plt, "Executive Constraint", "xconst")
make_plots(parcomp_plt, "Participation Competitiveness", "parcomp")
make_plots(parreg_plt, "Participation Regulation", "parreg")
make_plots(comp_plt, "Polyarchy", "comp")
make_plots(geddes_plt, "Authoritarian Regimes", "geddes", FALSE)

## compute aggregate error for each outcome
mse <- function(a, b) mean((a - b)^2)
mae <- function(a, b) mean(abs(a - b))
brier <- function(a, b) {
  if (is.matrix(a) & is.matrix(b))
    apply((a - b)^2, 2, mean)
  else
    mse(a, b)
}

loss <- list(brier, brier, mae, mae, mae, mae, mae, brier, brier, brier, brier)
names(loss) <- outcomes

pred <- lapply(list(uds_fit, polity_fit, comp_fit, geddes_fit), function(x) predict(x, OOB = TRUE))
pred <- lapply(pred, function(x) do.call("rbind", x))
names(pred) <- c("uds", "polity", "comp", "geddes")

error <- foreach(y = outcomes) %:% foreach(p = pred, .combine = "rbind") %do% {
  if (is.factor(df[[y]])) {
    tmp <- model.matrix(as.formula(paste0(" ~ -1 + ", y)), data = df)
    colnames(tmp) <- sapply(as.character(levels(df[[y]])), function(x) str_replace_all(paste(y, x), " ", "."))
    loss[[y]](p[, grepl(paste0("^", y), colnames(p))], tmp)
  } else
    loss[[y]](p[, grepl(y, colnames(p))], df[[y]])
}

error <- lapply(error, function(x) {
  if (ncol(x) > 1) {
    x <- as.data.frame(x)
    x$model <- c("uds", "polity", "comp", "geddes")
    x$outcome <- str_extract(colnames(x)[1], "^([^.])+(?=\\.)")
    out <- gather(x, level, value, -one_of("model", "outcome"))
    out$level <- gsub(paste0(outcomes, collapse = "|"), "", out$level)
    out$level <- gsub("\\.", " ", out$level)
    out$level <- gsub("^ ", "", out$level)
  } else {
    x <- as.data.frame(x)
    colnames(x) <- "value"
    x$model <- c("uds", "polity", "comp", "geddes")
    out <- x
  }
  out
})
names(error) <- outcomes

ciri_error <- ldply(error[ciri_vars])
ciri_error <- relabel_outcomes(ciri_error, "outcome")
ciri_error <- relabel_regimetype(ciri_error, "model")
p <- ggplot(ciri_error, aes(model, value, group = level, color = level)) +
  geom_point() +
  facet_wrap(~ outcome, scales = "free_y") +
  labs(x = "Regime Type Measure", y = "Brier Score", title = "Stratified Class OOB Error Rate") +
  theme_bw()
ggsave(paste0(dir_prefix, "figures/", "ciri_error.png"), p, width = 12, height = 8)
mid_error <- error["max_hostlevel"][[1]]
mid_error <- mid_error[mid_error$level != "no dispute", ]
mid_error <- relabel_outcomes(mid_error, "outcome")
mid_error <- relabel_regimetype(mid_error, "model")
p <- ggplot(mid_error, aes(model, value, group = level, color = level)) +
  geom_point() +
  labs(x = "Regime Type Measure", y = "Brier Score", title = "Stratified OOB Error Rate") +
  theme_bw()
ggsave(paste0(dir_prefix, "figures/", "mid_error.png"), p, width = 10, height = 6)
other_error <- error[-which(names(error) %in% c("max_hostlevel", ciri_vars))]
other_error$cwar <- other_error$cwar[other_error$cwar$level != "no civil war", ]
other_error$cwar$level <- NULL
other_error$cwar$outcome <- NULL
other_error <- ldply(other_error)
other_error <- relabel_outcomes(other_error, ".id")
 sother_error <- relabel_regimetype(other_error, "model")
p <- ggplot(other_error, aes(model, value)) +
  geom_point() + facet_wrap(~ .id, scales = "free") +
  labs(x = "Regime Type Measure", y = "Mean Absolute Error (Civil War panel displays Brier Score)",
       title = "Stratified OOB Error Rate") +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(paste0(dir_prefix, "figures/", "other_error.png"), p, width = 11.5, height = 9)
