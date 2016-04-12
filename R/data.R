args <- commandArgs(TRUE)
start_year <- as.integer(args[1])
end_year <- as.integer(args[2])

if (is.na(start_year)) {
  warning("start_year argument missing, default is 1981")
  start_year <- 1981
}

if (is.na(end_year)) {
  warning("end_year argument missing, default is 2008")
  end_year <- 2008
}

pkgs <- c("dplyr", "assertthat", "rio", "countrycode", "stringr", "lubridate", "ggplot2")
invisible(sapply(pkgs, library, character.only = TRUE))

source("functions.R")

path <- unlist(str_split(getwd(), "/"))
dir_prefix <- ifelse(path[length(path)] == "R", "../", "./")
data_prefix <- paste0(dir_prefix, "data/")

xpolity <- import(paste0(data_prefix, "xpolity.csv"))
assert_that(!anyDuplicated(xpolity))

fariss <- import(paste0(data_prefix, "fariss.csv"))[, c(1:2,18:19)]
colnames(fariss) <- c("ccode", "year", "latent_mean", "latent_sd")
assert_that(!anyDuplicated(fariss))

gtd <- import(paste0(data_prefix, "gtd.csv"))
colnames(gtd)[3:4] <- c("terror_events", "terror_killed")
assert_that(!anyDuplicated(gtd))

mid <- import(paste0(data_prefix, "mid.csv"))[, c(4,7,10,18)]
mid <- expand_years(mid)
mid <- mid %>% group_by(ccode, year) %>% summarize(max_hostlevel = max(HostLev, na.rm = TRUE))
assert_that(!anyDuplicated(mid))

ucdp_cwar <- import(paste0(data_prefix, "ucdp_cwar.dta"))
colnames(ucdp_cwar)[2] <- "ccode"
ucdp_cwar$ccode[ucdp_cwar$ccode == -99] <- NA
ucdp_cwar <- rename(ucdp_cwar, cwar = ucdpcivilwar)
assert_that(!anyDuplicated(ucdp_cwar))

ucdp_cwar <- import(paste0(data_prefix, "ucdp_cwar_types.dta"))
colnames(ucdp_cwar)[2:5] <- c("ccode", "cwar", "cwar_territory", "cwar_government")
ucdp_cwar$ccode[ucdp_cwar$ccode == -99] <- NA

ucdp_ns <- import(paste0(data_prefix, "ucdp_ns.dta"))[, c(1,4,7,16:17,21)]
colnames(ucdp_ns)[5:6] <- c("ns_fat_est", "ccode")

ucdp_ns <- expand_ccodes(ucdp_ns)
ucdp_ns <- ucdp_ns %>%
  group_by(ccode, year) %>%
  summarize(ns_fat = sum(ns_fat_est, na.rm = TRUE))
assert_that(!anyDuplicated(ucdp_ns))

ucdp_osv <- import(paste0(data_prefix, "ucdp_osv.dta"))[, c(1,4:5,10)]
colnames(ucdp_osv) <- c("osv_id", "year", "osv_fat_est", "ccode")
ucdp_osv <- expand_ccodes(ucdp_osv)
ucdp_osv$ccode <- as.integer(ucdp_osv$ccode)
ucdp_osv <- ucdp_osv %>%
  group_by(ccode, year) %>%
  summarize(osv_fat = sum(osv_fat_est, na.rm = TRUE))
assert_that(!anyDuplicated(ucdp_osv))

uds <- import(paste0(data_prefix, "uds.csv"))
assert_that(!anyDuplicated(uds))

uds_xpolity <- import(paste0(data_prefix, "uds_xpolity.csv"))[, c(3,2,4)]
colnames(uds_xpolity)[c(1,3)] <- c("ccode", "uds_xpolity")
uds_xpolity$year <- as.integer(uds_xpolity$year)
assert_that(!anyDuplicated(uds_xpolity))

poly <- import(paste0(data_prefix, "polyarchy.dta"))[, c(2,4,6)]
colnames(poly)[3] <- "ccode"
assert_that(!anyDuplicated(poly))

polity <- import(paste0(data_prefix, "polity.csv"))[, c(2,5,11,12:18)]
assert_that(!anyDuplicated(polity))

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
  mutate(newstate = ifelse((year - start_year) <= 2 & start_year > 1816, 1, 0))
gw$start_year <- NULL
gw <- ungroup(gw)

geddes <- import(paste0(data_prefix, "geddes.dta"))[, c(1:2,5)]
colnames(geddes) <- c("ccode", "year", "geddes")
geddes$ccode <- as.numeric(geddes$ccode)
geddes$year <- as.numeric(geddes$year)
assert_that(!anyDuplicated(geddes))

ciri <- import(paste0(data_prefix, "ciri.csv"))[, c(2,4,10:13)]
colnames(ciri) <- tolower(colnames(ciri))
ciri <- rename(ciri, ccode = cow)
ciri <- ciri[!duplicated(ciri), ]
assert_that(!anyDuplicated(ciri))

ksg <- import(paste0(data_prefix, "ksg.txt"))[, c(1,3:4,6)]
colnames(ksg)[1] <- "ccode"
ksg$pop <- log(ksg$pop)
ksg$rgdppc <- log(ksg$rgdppc)
assert_that(!anyDuplicated(ksg))

epr_exclpop <- import(paste0(data_prefix, "epr_exclpop.txt"))
names(epr_exclpop)[2] <- "ccode"
assert_that(!anyDuplicated(epr_exclpop))

epr_oil <- import(paste0(data_prefix, "epr_oilpc.txt"))
names(epr_oil)[2] <- "ccode"
assert_that(!anyDuplicated(epr_oil))

fearon_elf <- import(paste0(data_prefix, "fearon_elf.csv"))
names(fearon_elf)[1] <- "ccode"

idea <- import(paste0(data_prefix, "idea.dta"))
colnames(idea)[2:4] <- c("ccode", "violent_protest", "nonviolent_protest")
assert_that(!anyDuplicated(idea))

df <- Reduce(function(x, y) left_join(x, y, by = c("ccode", "year")),
             list(gw, fariss, gtd, mid, ucdp_cwar, ucdp_ns, ucdp_osv,
                  uds, uds_xpolity, poly, polity, geddes, ciri, ksg, epr_exclpop,
                  epr_oil, idea, xpolity))
df <- left_join(df, fearon_elf, by = "ccode")
df <- df[!duplicated(df), ] ## not sure where this is coming from, investigate further
assert_that(!anyDuplicated(df))

## NAs are not really 0s but we are doing this anyway
df$max_hostlevel[is.na(df$max_hostlevel)] <- 0
df$ns_fat[is.na(df$ns_fat)] <- 0
df$osv_fat[is.na(df$osv_fat)] <- 0
df$cwar[is.na(df$cwar)] <- 0
df$terror_killed[is.na(df$terror_killed)] <- 0
df$terror_events[is.na(df$terror_events)] <- 0
df$violent_protest[is.na(df$violent_protest)] <- 0
df$nonviolent_protest[is.na(df$nonviolent_protest)] <- 0

## use these as predictors
miss_codes <- c(-88, -77, -66)
miss_labels <- c("transition", "interregnum", "interruption")
xrcomp_labels <- c("unregulated", "selection", "dual/transitional", "election")
xropen_labels <- c("unregulated", "closed", "dual executive-designation", "dual executive-election", "open")
xrreg_labels <- c("selection", "dual/transitional", "election")
parreg_labels <- c("unregulated", "multiple identity", "sectarian", "restricted", "regulated")
xconst_labels <- c("unregulated", "unlimited", "intermediate 1", "slight/moderate", "intermediate 2",
                   "substantial", "party/subordination")

xrcomp_labels <- c(miss_labels, xrcomp_labels)
xropen_labels <- c(miss_labels, xropen_labels)
xrreg_labels <- c(miss_labels, xrreg_labels)
parreg_labels <- c(miss_labels, parreg_labels)
xconst_labels <- c(miss_labels, xconst_labels)

df$xrcomp <- factor(df$xrcomp, labels = xrcomp_labels)
df$xropen <- factor(df$xropen, labels = xropen_labels)
df$xrreg <- factor(df$xrreg, labels = xrreg_labels)
df$parreg <- factor(df$parreg, labels = parreg_labels)
df$xconst <- factor(df$xconst, labels = xconst_labels)

df$disap[df$disap < 0 | df$disap > 2] <- NA
df$kill[df$kill < 0 | df$kill > 2] <- NA
df$polpris[df$polpris < 0 | df$polpris > 2] <- NA
df$tort[df$tort < 0 | df$tort > 2] <- NA

df$geddes[is.na(df$geddes) | df$geddes == "NA"] <- "non-autocracy"
df$geddes <- as.factor(df$geddes)

df$cwar <- factor(df$cwar, labels = c("no civil war", "civil war"))
df$cwar_territory <- factor(df$cwar_territory, labels = c("no terr. civil war", "terr. civil war"))
df$cwar_government <- factor(df$cwar_government, labels = c("no gov. civil war", "gov. civil war"))

df$max_hostlevel <- factor(df$max_hostlevel,
                           labels = c("no dispute",
                                      "no militarized action",
                                      "threat to use force",
                                      "display of force",
                                      "use of force",
                                      "war"), ordered = TRUE)
ciri_labels <- c("frequent", "occasional", "none")
df$disap <- factor(df$disap, labels = ciri_labels, ordered = TRUE)
df$tort <- factor(df$tort, labels = ciri_labels, ordered = TRUE)
df$kill <- factor(df$kill, labels = ciri_labels, ordered = TRUE)
df$polpris <- factor(df$polpris, labels = ciri_labels, ordered = TRUE)
df$newstate <- factor(df$newstate, labels = c("not a recent entrant", "entry into system in last two years"))

## df$cname <- countrycode(df$ccode, "cown", "country.name", TRUE)
## miss_mapper(df, "disap", "disappearences", "disap_missing")
## miss_mapper(df, "tort", "torture", "tort_missing")
## miss_mapper(df, "kill", "killings", "kill_missing")
## miss_mapper(df, "polpris", "political imprisonment", "polpris_missing")
## miss_mapper(df, "latent_mean", "fariss missing", "fariss_missing")
## miss_mapper(df, "cwar", "civil war missing", "cwar_missing")
## miss_mapper(df, "max_hostlevel", "max hostility level missing", "max_hostlevel_missing")
## miss_mapper(df, "terror_killed", "terrorist atacks missing", "terror_killed_missing")
## miss_mapper(df, "terror_events", "terrorism deaths missing", "terror_events_missing")
## miss_mapper(df, "ns_fat", "fatalities from ns missing", "ns_fat_missing")
## miss_mapper(df, "osv_fat", "fatalities from osv missing", "osv_fat_missing")

out_file <- paste0(dir_prefix, "data/", "rep.csv")
df <- df[df$year <= end_year & df$year >= start_year, ]
export(df, out_file, "csv")
