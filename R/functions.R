ccode_fix <- function(df) {
  df$ccode[df$year >= 1991 & df$year <= 2008 & df$ccode == 255] <- 260
  df$ccode[df$year >= 2006 & df$year <= 2008 & df$ccode == 340] <- 345
  df$ccode[df$year >= 1991 & df$year <= 2008 & df$ccode == 678] <- 670
  df
}

relabel_outcomes <- function(data, var, reverse = FALSE) {
  labs <- rbind(c("cwar", "Civil Conflict (UCDP)"),
                c("cwar.civil.war", "Civil Conflict (UCDP)"),
                c("civil.war", "Civil Conflict (UCDP)"),
                c("ns_fat", "Non-State Conflict (fatalities, UCDP)"),
                c("osv_fat", "One Sided Violence (fatalities, UCDP)"),
                c("latent_mean", "Respect for Physical Integrity\n (Fariss, posterior mean)"),
                c("terror_events", "Terrorist Attacks (GTD)"),
                c("terror_killed", "Fatalities from Terrorist Attacks (GTD)"),
                c("polpris", "Political Imprisonment (CIRI)"),
                c("disap", "Disappearances (CIRI)"),
                c("tort", "Torture (CIRI)"),
                c("kill", "Extrajudicial Killings (CIRI)"),
                c("max_hostlevel", "Maximum Hostility Level (MID IV)"),
                c("nonviolent_protest", "Non-Violent Protest (IDEA)"),
                c("violent_protest", "Violent Protest (IDEA)"),
                c("max_hostlevel.use.of.force", "Use of Force (MID IV)"),
                c("use.of.force", "Use of Force (MID IV)"))
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
