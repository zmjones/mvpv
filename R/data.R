pkgs <- c("dplyr", "assertthat", "rio", "stringr", "lubridate", "countrycode")
invisible(sapply(pkgs, library, character.only = TRUE))

source("functions.R")
source("global.R")

path <- unlist(str_split(getwd(), "/"))
dir_prefix <- ifelse(path[length(path)] == "R", "../", "./")
data_prefix <- paste0(dir_prefix, "data/")

## xpolity data (from yon, i guess from vreeland?)
xpolity <- import(paste0(data_prefix, "xpolity.csv")) %>%
  rename(xpolity_nas = x_polity) %>%
  mutate(xpolity = ifelse(xpolity_nas < -6, NA, xpolity_nas),
    year = as.integer(year))

## fariss' repression estimates (APSR paper, posterior mean)
## exclude subdivision of israel
fariss <- import(paste0(data_prefix, "fariss.csv")) %>%
  select(ccode = COW, year = YEAR, latent_mean = latentmean) %>%
  filter(!ccode %in% c(666.001, 666.002, 666.003),
    !(ccode == 255 & year == 1990),
    !(ccode == 679 & year == 1990)) %>% ## exclusions for transitions?
  mutate(ccode = as.integer(recode(ccode, `255` = 260,
    `679` = 678)))

## gtd data from website
gtd <- import(paste0(data_prefix, "gtd.csv")) %>%
  select(year = iyear, ccode = country, nkill) %>%
  group_by(ccode, year) %>%
  summarise(terror_events = length(nkill), terror_killed = sum(nkill, na.rm = TRUE)) %>%
  ungroup()

## mid data from website
mid <- import(paste0(data_prefix, "mid.csv")) %>%
  select(ccode, StYear, EndYear, HostLev) %>%
  expand_years() %>%
  group_by(ccode, year) %>%
  summarize(max_hostlevel = max(HostLev, na.rm = TRUE)) %>%
  ungroup()

## ucdp conflict onset from website
ucdp_conflict <- import(paste0(data_prefix, "ucdp_onset.csv")) %>%
  select(onset = onset1v414, intensity = maxintyearv414, ccode = gwno, year,
    count = nototconfv414) %>%
  mutate(cwar_onset = ifelse(onset == 1 & intensity == 2, 1, 0),
    cconflict_onset = ifelse(onset == 1 & intensity == 1, 1, 0),
    cwar_count = ifelse(intensity == 2, count, 0),
    cconflict_count = ifelse(intensity == 1, count, 0)) %>%
  select(ccode, year, cwar_onset, cconflict_onset, cwar_count, cconflict_count)

## ucdp ged from website
ged <- import(paste0(data_prefix, "ged.csv")) %>%
  filter(type_of_violence %in% c("2", "3")) %>%
  mutate(ccode = countrycode(country, "country.name", "cown"),
    osv_deaths = as.integer(ifelse(type_of_violence == 3, best_est, NA)),
    nsv_deaths = as.integer(ifelse(type_of_violence == 2, best_est, NA)),
    year = as.integer(year)) %>%
  select(ccode, year, osv_deaths, nsv_deaths) %>%
  group_by(year, ccode) %>%
  summarise(osv_deaths = sum(osv_deaths, na.rm = TRUE),
    nsv_deaths = sum(nsv_deaths, na.rm = TRUE)) %>%
  ungroup()

## uds data from website (posterior mean)
uds <- import(paste0(data_prefix, "uds.csv"))

## posterior mean from re-fit uds w/ xpolity data (using their package and uds.R)
uds_xpolity <- import(paste0(data_prefix, "uds_xpolity.csv")) %>%
  select(ccode = cowcode, year = year, uds_xpolity = mean) %>%
  mutate(year = as.integer(year), ccode = as.integer(ccode))

## polarchy data (via yon)
poly <- import(paste0(data_prefix, "polyarchy.dta")) %>%
  select(ccode = ssno, year, part) %>%
  mutate(year = as.integer(year),
    ccode = as.integer(ccode),
    part = as.numeric(part))

## polity data from website
polity <- import(paste0(data_prefix, "polity.csv")) %>%
  select(ccode, year, xrcomp, xropen, xrreg, parreg, xconst, durable) %>%
  mutate(durable = ifelse(durable == 0, 1, 0))

## gleditsch and ward membership data from gledtisch's website
gw <- read.delim(paste0(data_prefix, "iisystem.dat"), header = FALSE)
colnames(gw) <- c("ccode", "abb", "name", "start", "end")
gw$start <- dmy(gw$start)
gw$end <- dmy(gw$end)
gw <- lapply(1:nrow(gw), function(x) {
  years <- year(gw[x, "start"]):year(gw[x, "end"])
  cbind("ccode" = gw[x, "ccode"], "year" = years)
})
gw <- as.data.frame(do.call("rbind", gw))
gw <- gw %>% group_by(ccode) %>%
  mutate(start_year = min(year)) %>%
  mutate(newstate = ifelse((year - start_year) <= 2 & start_year > 1816, 1, 0),
    start_year = NULL) %>%
  ungroup()

## geddes data (via yon)
geddes <- import(paste0(data_prefix, "geddes.dta"))[, c(1:2,5)] %>%
  select(ccode = cowcode, year = year, geddes = gwf_regimetype) %>%
  mutate(ccode = as.integer(ccode), year = as.integer(year))

## gleditsch filled-in real gdp per capita data from website
ksg <- import(paste0(data_prefix, "ksg.txt")) %>%
  select(ccode = statenum, year, pop, rgdppc) %>%
  mutate(pop = log(pop), rgdppc = log(rgdppc))

## epr data from website
epr_exclpop <- import(paste0(data_prefix, "epr_exclpop.txt")) %>%
  rename(ccode = cowcode)

## epr data from website
epr_oil <- import(paste0(data_prefix, "epr_oilpc.txt")) %>%
  rename(ccode = cowcode)

## fearon's elf data (via yon)
fearon_elf <- import(paste0(data_prefix, "fearon_elf.csv")) %>%
  rename(ccode = gwno)

## idea data (via yon)
idea <- import(paste0(data_prefix, "idea.dta")) %>%
  select(year, ccode = gwno, violent_protest = violprot_idea,
    nonviolent_protest = nonvprot_idea) %>%
  mutate(ccode = recode(ccode, `255` = 260, `679` = 678)) %>%
  mutate_each(funs(as.integer)) %>%
  filter(year <= 2004 & !is.na(ccode) & ccode != 9999999 &
           !(ccode == 260 & is.na(violent_protest)) &
           ccode != 679 &
           !(ccode == 678 & is.na(violent_protest)))

## banks data (via yon)
banks <- import(paste0(data_prefix, "banks_wilson.dta")) %>%
  select(year, ccode = gwno,
    violent_protest = violprot_banks,
    nonviolent_protest = nonvprot_banks) %>%
  mutate_each(funs(as.integer)) %>%
  filter(year > 2004)
## combine idea and banks counts (banks inserted starting in 2004)
protest <- rbind(idea, banks)

## apply ccode fixes
data_list <- list(gw, fariss, gtd, mid, ucdp_conflict, uds, uds_xpolity, poly, polity,
  geddes, ksg, epr_exclpop, epr_oil, xpolity, ged, protest)

## join everything together
df <- Reduce(function(x, y) left_join(x, y, by = c("ccode", "year")), data_list) %>%
  left_join(., fearon_elf, by = "ccode")

## fill in zeroes for some missings
## setup factors with appropriate labels
df <- df %>% mutate(
  max_hostlevel = ifelse(is.na(max_hostlevel) & year >= 1816, 0, max_hostlevel),
  cwar_onset = ifelse(is.na(cwar_onset) & year >= 1946, 0, cwar_onset),
  cconflict_onset = ifelse(is.na(cconflict_onset) & year >= 1946, 0, cconflict_onset),
  cwar_count = ifelse(is.na(cwar_count) & year >= 1946, 0, cwar_count),
  cconflict_count = ifelse(is.na(cconflict_count) & year >= 1946, 0, cconflict_count),
  terror_killed = ifelse(is.na(terror_killed) & year >= 1970, 0, terror_killed),
  terror_events = ifelse(is.na(terror_events) & year >= 1970, 0, terror_events),
  osv_deaths = ifelse(is.na(osv_deaths) & year >= 1989, 0, osv_deaths),
  nsv_deaths = ifelse(is.na(nsv_deaths) & year >= 1989, 0, nsv_deaths),
  xrcomp = factor(xrcomp, labels = xrcomp_labels),
  xropen = factor(xropen, labels = xropen_labels),
  xrreg = factor(xrreg, labels = xrreg_labels),
  parreg = factor(parreg, labels = parreg_labels),
  xconst = factor(xconst, labels = xconst_labels),
  geddes = as.factor(ifelse(is.na(geddes) | geddes == "NA", "non-autocracy", geddes)),
  max_hostlevel = factor(max_hostlevel,
    labels = c("no dispute",
      "no militarized action",
      "threat to use force",
      "display of force",
      "use of force",
      "war"), ordered = TRUE),
  newstate = factor(newstate,
    labels = c("not a recent entrant", "entry into system in last two years"))
)

## check for duplicate country years
assert_that(!anyDuplicated(df[, c("ccode", "year")]))

## write two different time periods to file
export(df[df$year <= 2008 & df$year >= 1990, ], paste0(data_prefix, "1990_2008_rep.csv"))
export(df[df$year <= 2008 & df$year >= 1970, ], paste0(data_prefix, "1970_2008_rep.csv"))
