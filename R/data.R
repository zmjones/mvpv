pkgs = c("assertthat", "data.table", "stringr", "lubridate", "countrycode", "foreign",
  "dplyr", "dtplyr")
invisible(sapply(pkgs, library, character.only = TRUE))

source("functions.R")
source("global.R")

path = unlist(str_split(getwd(), "/"))
dir_prefix = ifelse(path[length(path)] == "R", "../", "./")
data_prefix = paste0(dir_prefix, "data/")

## ccode fixing rules

## for cow coded transition years (new code for post-transition)
## if there is a duplicate for the transition year
## take the values from the observation that corresponds to the new code

## (1) Vietnam
## 818 is a new code for Vietnam (COW)
## so replace all instances of 818 with 816
## (2) Yemen
## COW has the following codes
## 678 South Yemen
## 679 Unified Yemen
## 680 North Yemen
## GW only has 678 and 680 as they code 678 as being Unified Yemen
## so, recode 679 to 678
## (3) Russia
## 364 is NA in both GW and COW
## 365 is Russia (Soviet Union) (1816-2012 in GW, same in COW)
## x/polity using 364 for the years 1922-1992
## so i just recoded 364 for those years in xpolity and polity
## (4) Germany
## 255 is Germany (Prussia, 1816-1945 in GW, 1816-2011 in COW)
## 260 is the GFR (1949-2012 in GW, 1955-1990 in COW)
## 265 is the GDR (1949-1990 in GW, 1954-1990 in COW)
## recode 255 to 260 after 1990

## xpolity data (from yon, i guess from vreeland?)
xpolity = fread(paste0(data_prefix, "xpolity.csv")) %>%
  rename(xpolity = x_polity) %>%
  mutate(xpolity_nas = ifelse(xpolity < -6, NA, xpolity),
    year = as.integer(year))
## xpolity[xpolity$ccode %in% c(678, 679) & xpolity$year == 1990, ]
## taking the non-missing one
xpolity = xpolity[!(year == 1990 & ccode == 679), ]
xpolity$ccode[xpolity$ccode == 679] = 678
## both are the same for 1976, so drop one
## xpolity[xpolity$ccode %in% c(816, 818) & xpolity$year == 1976, ]
xpolity = xpolity[!(ccode == 818 & year == 1976), ]
xpolity$ccode[xpolity$ccode == 818] = 816
## 365 is missing between 1922 and 1992
## 364 runs over these years, so just recode 364 to 365
xpolity = xpolity[!(ccode == 364 & year == 1922), ]
xpolity$ccode[xpolity$ccode == 364] = 365
## drop the one duplicate 255/260 year and then recode all 255 after 1990 as 260
xpolity = xpolity[!(ccode == 260 & year == 1990), ]
xpolity$ccode[xpolity$ccode == 255 & xpolity$year >= 1990] = 260

## fariss' repression estimates (APSR paper, posterior mean)
## exclude subdivision of israel
fariss = fread(paste0(data_prefix, "fariss.csv")) %>%
  select(ccode = COW, year = YEAR, latent_mean = latentmean) %>%
  filter(!ccode %in% c(666.001, 666.002, 666.003))
## fariss[fariss$ccode %in% c(678, 679, 680) & fariss$year == 1990, ]
## going to take the one that corresponds to the 'new' ccode
fariss = fariss[!(ccode == 679 & year == 1990), ]
fariss$ccode[fariss$ccode == 679] = 678
## there is overlap between 255 and 260 in 1990
## consistent with the other input data i will drop 255 here
fariss = fariss[!(ccode == 255 & year == 1990), ]
fariss$ccode[fariss$ccode == 255 & fariss$year >= 1990] = 260

## gtd data from yon
gtd = fread(paste0(data_prefix, "gtd_corrected.csv"))

## mid data from website
mid = fread(paste0(data_prefix, "mid.csv")) %>%
  select(ccode, StYear, EndYear, HostLev) %>%
  expand_years() %>%
  group_by(ccode, year) %>%
  summarize(max_hostlevel = max(HostLev, na.rm = TRUE)) %>%
  ungroup()
mid$ccode[mid$ccode == 679] = 678
mid$ccode[mid$ccode == 255 & mid$year >= 1990] = 260

## ucdp conflict onset from website
ucdp_conflict = fread(paste0(data_prefix, "ucdp_onset.csv")) %>%
  select(onset = onset1v414, intensity = maxintyearv414, ccode = gwno, year,
    count = nototconfv414) %>%
  mutate(cwar_onset = ifelse(onset == 1 & intensity == 2, 1, 0),
    cconflict_onset = ifelse(onset == 1 & intensity == 1, 1, 0),
    cwar_count = ifelse(intensity == 2, count, 0),
    cconflict_count = ifelse(intensity == 1, count, 0)) %>%
  select(ccode, year, cwar_onset, cconflict_onset, cwar_count, cconflict_count)

## ucdp ged from website
ged = fread(paste0(data_prefix, "ged.csv")) %>%
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
## uds = fread(paste0(data_prefix, "uds.csv"))

## posterior mean from re-fit uds w/ xpolity data (using their package and uds.R)
uds_xpolity = fread(paste0(data_prefix, "uds_xpolity.csv")) %>%
  select(ccode = cowcode, year = year, uds_xpolity = mean) %>%
  mutate(year = as.integer(year), ccode = as.integer(ccode))

## polarchy data (via yon)
## poly = read.dta(paste0(data_prefix, "polyarchy.dta")) %>%
##   select(ccode = ssno, year, part) %>%
##   mutate(year = as.integer(year),
##     ccode = as.integer(ccode),
##     part = as.numeric(part))

## polity data from website
polity = fread(paste0(data_prefix, "polity.csv")) %>%
  select(ccode, year, durable) %>%
  mutate(durable = ifelse(durable == 0, 1, 0))
polity = polity[!(year == 1990 & ccode == 679), ]
polity$ccode[polity$ccode == 679] = 678
## both are the same for 1976, so drop one
## polity[polity$ccode %in% c(816, 818) & polity$year == 1976, ]
polity = polity[!(ccode == 818 & year == 1976), ]
polity$ccode[polity$ccode == 818] = 816
## 365 is missing between 1922 and 1992
## 364 runs over these years, so just recode 364 to 365
polity = polity[!(ccode == 364 & year == 1922), ]
polity$ccode[polity$ccode == 364] = 365
## drop the one duplicate 255/260 year and then recode all 255 after 1990 as 260
polity = polity[!(ccode == 260 & year == 1990), ]
polity$ccode[polity$ccode == 255 & polity$year >= 1990] = 260

## gleditsch and ward membership data from gledtisch's website
gw = fread(paste0(data_prefix, "iisystem.dat"), header = FALSE)
setnames(gw, colnames(gw), c("ccode", "abb", "name", "start", "end"))
gw$start = dmy(gw$start)
gw$end = dmy(gw$end)
setDF(gw)
gw = lapply(1:nrow(gw), function(x) {
  years = year(gw[x, "start"]):year(gw[x, "end"])
  cbind("ccode" = gw[x, "ccode"], "year" = years)
})
gw = as.data.frame(do.call("rbind", gw))
gw = gw %>% group_by(ccode) %>%
  mutate(start_year = min(year)) %>%
  mutate(newstate = ifelse((year - start_year) <= 2 & start_year > 1816, 1, 0),
    start_year = NULL) %>%
  ungroup()

## geddes data (via yon)
## geddes = import(paste0(data_prefix, "geddes.dta"))[, c(1:2,5)] %>%
##   select(ccode = cowcode, year = year, geddes = gwf_regimetype) %>%
##   mutate(ccode = as.integer(ccode), year = as.integer(year))

## gleditsch filled-in real gdp per capita data from website
ksg = fread(paste0(data_prefix, "ksg.txt")) %>%
  select(ccode = statenum, year, pop, rgdppc) %>%
  mutate(pop = log(pop), rgdppc = log(rgdppc))

## epr data from website
epr_exclpop = fread(paste0(data_prefix, "epr_exclpop.txt")) %>%
  rename(ccode = cowcode)
epr_exclpop$ccode[epr_exclpop$ccode == 679] = 678 ## no overlap in this case
epr_exclpop$ccode[epr_exclpop$ccode == 255 & epr_exclpop$year >= 1990] = 260

## epr data from website
epr_oil = fread(paste0(data_prefix, "epr_oilpc.txt")) %>%
  rename(ccode = cowcode)
epr_oil$ccode[epr_oil$ccode == 679] = 678
epr_oil$ccode[epr_oil$ccode == 255 & epr_oil$year >= 1990] = 260

## fearon's elf data (via yon)
fearon_elf = fread(paste0(data_prefix, "fearon_elf.csv")) %>%
  rename(ccode = gwno)

## idea data (via yon)
idea = read.dta(paste0(data_prefix, "idea.dta")) %>%
  select(year, ccode = gwno, violent_protest = violprot_idea,
    nonviolent_protest = nonvprot_idea) %>%
  mutate_all(funs(as.integer)) %>%
  filter(year <= 2004 & !is.na(ccode) & ccode != 9999999) %>%
  filter(!(is.na(violent_protest) & is.na(nonviolent_protest)))
idea$ccode[idea$ccode == 255 & idea$year >= 1990] = 260
idea$ccode[idea$ccode == 679] = 678

## banks data (via yon)
## banks = read.dta(paste0(data_prefix, "banks_wilson.dta")) %>%
##   select(year, ccode = gwno,
##     violent_protest = violprot_banks,
##     nonviolent_protest = nonvprot_banks) %>%
##   mutate_each(funs(as.integer)) %>%
##   filter(year > 2004)

## icews from
## https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/28075
## from 2005 to 2008
icews.2008 = fread(paste0(data_prefix, "events.2008.20150313084156.tab"))
icews.2007 = fread(paste0(data_prefix, "events.2007.20150313083959.tab"))
icews.2006 = fread(paste0(data_prefix, "events.2006.20150313083752.tab"))
icews.2005 = fread(paste0(data_prefix, "events.2005.20150313083555.tab"))
icews = rbind(icews.2005, icews.2006, icews.2007, icews.2008)
icews = icews %>% select("date" = "Event Date", "source" = "Source Name",
  "target" = "Target Country", "code" = "CAMEO Code", "target.type" = "Target Sectors",
  "source.type" = "Source Sectors") %>%
  mutate(nonviolent_protest = ifelse(code %in% 140:144, 1, 0),
    violent_protest = ifelse(code == 145, 1, 0)) %>%
  filter(!(grepl("Government|Military|Army|Police|Unspecified|Unidentified", source.type) & !grepl("International Government Organization|Out of Government", source.type))) %>%
  filter(grepl("Government|In Government|Military|Army|Police", target.type) &
           !grepl("International Government Organization|Out of Government", target.type)) %>%
  filter(nonviolent_protest == 1 | violent_protest == 1) %>%
  mutate(date = ymd(date), year = year(date)) %>%
  group_by(target, year) %>%
  summarize(violent_protest = sum(violent_protest),
    nonviolent_protest = sum(nonviolent_protest)) %>%
  mutate(ccode = countrycode(target, "country.name", "cown", warn = FALSE)) %>%
  group_by(year) %>%
  mutate(nonviolent_protest = ifelse(target == "China",
    sum(nonviolent_protest[target %in% c("Macao", "China", "Hong Kong")], na.rm = TRUE),
    nonviolent_protest),
    violent_protest = ifelse(target == "China", sum(violent_protest[target %in% c("Macao", "China", "Hong Kong")], na.rm = TRUE), violent_protest),
    ccode = case_when(target == "Serbia" ~ 345L, TRUE ~ ccode)) %>%
  select(year, ccode, violent_protest, nonviolent_protest) %>%
  filter(!is.na(ccode))

## combine idea and banks counts (banks inserted starting in 2004)
## protest = rbind(idea, banks)

## combine idea and icews data
protest = rbind(idea, icews)

## join everything together
data_list = list(gw, fariss, gtd, mid, ucdp_conflict, uds_xpolity,
  ksg, epr_exclpop, epr_oil, xpolity, ged, protest, polity)
df = Reduce(function(x, y) left_join(x, y, by = c("ccode", "year")), data_list) %>%
  left_join(., fearon_elf, by = "ccode")

## fill in zeroes for some missings
## setup factors with appropriate labels
df = df %>% mutate(
  max_hostlevel = ifelse(is.na(max_hostlevel) & year >= 1816, 0, max_hostlevel),
  cwar_onset = ifelse(is.na(cwar_onset) & year >= 1946, 0, cwar_onset),
  cconflict_onset = ifelse(is.na(cconflict_onset) & year >= 1946, 0, cconflict_onset),
  cwar_count = ifelse(is.na(cwar_count) & year >= 1946, 0, cwar_count),
  cconflict_count = ifelse(is.na(cconflict_count) & year >= 1946, 0, cconflict_count),
  terror_killed = ifelse(is.na(terror_killed) & year >= 1970, 0, terror_killed),
  terror_events = ifelse(is.na(terror_events) & year >= 1970, 0, terror_events),
  osv_deaths = ifelse(is.na(osv_deaths) & year >= 1989, 0, osv_deaths),
  nsv_deaths = ifelse(is.na(nsv_deaths) & year >= 1989, 0, nsv_deaths),
  violent_protest = ifelse(is.na(violent_protest), 0, violent_protest),
  nonviolent_protest = ifelse(is.na(nonviolent_protest), 0, nonviolent_protest),
  ## xrcomp = factor(xrcomp, labels = xrcomp_labels),
  ## xropen = factor(xropen, labels = xropen_labels),
  ## xrreg = factor(xrreg, labels = xrreg_labels),
  ## parreg = factor(parreg, labels = parreg_labels),
  ## xconst = factor(xconst, labels = xconst_labels),
  ## geddes = as.factor(ifelse(is.na(geddes) | geddes == "NA", "non-autocracy", geddes)),
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
df = df[df$year <= 2008 & df$year >= 1970, ]
assert_that(!anyDuplicated(df[, c("ccode", "year")]))

## missingness dumps
for (x in colnames(df)[-c(1:2)])
  write.csv(df[is.na(df[[x]]), c("ccode", "year")],
    paste0(data_prefix, x, "_missing.csv"), row.names = FALSE)

## write two different time periods to file
fwrite(df[df$year <= 2008 & df$year >= 1990, ], paste0(data_prefix, "1990_2008_rep.csv"))
fwrite(df[df$year <= 2008 & df$year >= 1970, ], paste0(data_prefix, "1970_2008_rep.csv"))
