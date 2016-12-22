regime <- list(
  c("part", "Polyarchy"),
  c("xpolity", "X-Polity (w/ missing categories)"),
  c("xpolity_nas", "X-Polity"),
  c("uds_xpolity", "X-UDS"))
regime <- as.data.frame(do.call(rbind, regime), stringsAsFactors = FALSE)
colnames(regime) <- c("name", "label")

explanatory <- list(
  c("year", "Year"),
  c("rgdppc", "Real GDP per Capita"),
  c("pop", "Population"),
  c("exclpop", "Excluded Population"),
  c("oilpc", "Oil Exports"),
  c("ethfrac", "Ethnic Fractionalization"),
  c("newstate", "New State"),
  c("durable", "Regime Durability")
)
explanatory <- as.data.frame(do.call(rbind, explanatory), stringsAsFactors = FALSE)
colnames(explanatory) <- c("name", "label")

outcomes <- list(
  c("cwar_count", "Civil War (count)"),
  c("cconflict_count", "Civil Conflict (count)"),
  c("cwar_onset", "Civil War (onset)"),
  c("cconflict_onset", "Civil Conflict (onset)"),
  c("use.of.force", "Use of Force (MID IV)"),
  c("latent_mean", "Respect for Physical Integrity"),
  c("terror_killed", "Terrorism Deaths"),
  c("terror_events", "Terrorism Events"),
  c("nonviolent_protest", "Non-Violent Protest"),
  c("violent_protest", "Violent Protest"),
  c("osv_deaths", "Deaths from One-Sided Violence"),
  c("nsv_deaths", "Deaths from Non-State Violence")
)
outcomes <- as.data.frame(do.call(rbind, outcomes), stringsAsFactors = FALSE)
colnames(outcomes) <- c("name", "label")

all <- rbind(regime, explanatory, outcomes)
