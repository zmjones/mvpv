regime = list(
  c("xpolity_nas", "X-Polity"),
  c("xpolity", "X-Polity (w/ missing categories)"),
  c("uds_xpolity", "X-UDS"))
regime = as.data.frame(do.call(rbind, regime), stringsAsFactors = FALSE)
colnames(regime) = c("name", "label")

explanatory = list(
  c("year", "Year"),
  c("rgdppc", "Real GDP per Capita"),
  c("pop", "Population"),
  c("exclpop", "Excluded Population"),
  c("oilpc", "Oil Exports"),
  c("ethfrac", "Ethnic Fractionalization"),
  c("newstate", "New State"),
  c("durable", "Regime Durability")
)
explanatory = as.data.frame(do.call(rbind, explanatory), stringsAsFactors = FALSE)
colnames(explanatory) = c("name", "label")

outcomes = list(
  c("cwar_count", "Civil War (count)"),
  c("cconflict_count", "Civil Conflict (count)"),
  c("cwar_onset", "Civil War (onset)"),
  c("cconflict_onset", "Civil Conflict (onset)"),
  c("max_hostlevel.use.of.force", "Use of Force (MID IV)"),
  c("latent_mean", "Repression"),
  c("terror_killed", "Terrorism Deaths"),
  c("terror_events", "Terrorism Events"),
  c("nonviolent_protest", "Non-Violent Dissent Events"),
  c("violent_protest", "Violent Dissent Events"),
  c("osv_deaths", "Deaths from One-Sided Violence"),
  c("nsv_deaths", "Deaths from Non-State Violence")
)
outcomes = as.data.frame(do.call(rbind, outcomes), stringsAsFactors = FALSE)
colnames(outcomes) = c("name", "label")

outcomes_1970 = outcomes[!str_detect(outcomes$name, "protest|osv_deaths|nsv_deaths"), ]

all = rbind(regime, explanatory, outcomes)

## xpolity labels
## use these as predictors (not used currently)
miss_codes = c(-88, -77, -66)
miss_labels = c("transition", "interregnum", "interruption")
xrcomp_labels = c("unregulated", "selection", "dual/transitional", "election")
xropen_labels = c("unregulated", "closed", "dual executive-designation", "dual executive-election", "open")
xrreg_labels = c("selection", "dual/transitional", "election")
parreg_labels = c("unregulated", "multiple identity", "sectarian", "restricted", "regulated")
xconst_labels = c("unregulated", "unlimited", "intermediate 1", "slight/moderate", "intermediate 2",
  "substantial", "party/subordination")

xrcomp_labels = c(miss_labels, xrcomp_labels)
xropen_labels = c(miss_labels, xropen_labels)
xrreg_labels = c(miss_labels, xrreg_labels)
parreg_labels = c(miss_labels, parreg_labels)
xconst_labels = c(miss_labels, xconst_labels)
