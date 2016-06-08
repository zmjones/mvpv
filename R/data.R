args <- commandArgs(TRUE)
start_year <- as.integer(args[1])
end_year <- as.integer(args[2])

if (is.na(start_year)) {
  warning("start_year argument missing, default is 1990")
  start_year <- 1990
}

if (is.na(end_year)) {
  warning("end_year argument missing, default is 2008")
  end_year <- 2008
}

pkgs <- c("dplyr", "assertthat", "rio", "stringr", "lubridate", "feather")
invisible(sapply(pkgs, library, character.only = TRUE))

source("functions.R")

path <- unlist(str_split(getwd(), "/"))
dir_prefix <- ifelse(path[length(path)] == "R", "../", "./")
data_prefix <- paste0(dir_prefix, "data/")

xpolity <- import(paste0(data_prefix, "xpolity.csv"))
assert_that(!anyDuplicated(xpolity))

fariss <- import(paste0(data_prefix, "fariss.csv"))[, c(1:2,18:19)]
colnames(fariss) <- c("ccode", "year", "latent_mean", "latent_sd")
fariss$ccode <- as.integer(fariss$ccode)
assert_that(!anyDuplicated(fariss))

gtd <- read_feather(paste0(data_prefix, "gtd.feather")) %>%
  select(year = iyear, ccode = country, nkill)
gtd <- gtd %>% group_by(ccode, year) %>%
  summarise(terror_events = length(nkill), terror_killed = sum(nkill))
assert_that(!anyDuplicated(gtd))

mid <- import(paste0(data_prefix, "mid.csv"))[, c(4,7,10,18)]
mid <- expand_years(mid)
mid <- mid %>% group_by(ccode, year) %>% summarize(max_hostlevel = max(HostLev, na.rm = TRUE))
assert_that(!anyDuplicated(mid))

ucdp_conflict <- import(paste0(data_prefix, "ucdp_onset.csv")) %>%
  select(onset = onset1v414, intensity = maxintyearv414, ccode = gwno, year,
         count = nototconfv414) %>%
  mutate(cwar_onset = ifelse(onset == 1 & intensity == 2, 1, 0),
         cconflict_onset = ifelse(onset == 1 & intensity == 1, 1, 0),
         cwar_count = ifelse(intensity == 2, count, 0),
         cconflict_count = ifelse(intensity == 1, count, 0)) %>%
  select(ccode, year, cwar_onset, cconflict_onset, cwar_count, cconflict_count)

uds <- import(paste0(data_prefix, "uds.csv"))
assert_that(!anyDuplicated(uds))

uds_xpolity <- import(paste0(data_prefix, "uds_xpolity.csv"))[, c(3,2,4)]
colnames(uds_xpolity)[c(1,3)] <- c("ccode", "uds_xpolity")
uds_xpolity$year <- as.integer(uds_xpolity$year)
assert_that(!anyDuplicated(uds_xpolity))

poly <- import(paste0(data_prefix, "polyarchy.dta"))[, c(2,4,6)]
colnames(poly)[3] <- "ccode"
assert_that(!anyDuplicated(poly))

polity <- import(paste0(data_prefix, "polity.csv"))
polity <- polity %>% select(ccode, year, xrcomp, xropen, xrreg, parreg, xconst, durable) %>%
  mutate(durable = ifelse(durable == 0, 1, 0))
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
geddes$ccode <- as.integer(geddes$ccode)
geddes$year <- as.integer(geddes$year)
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
fearon_elf$ccode[fearon_elf$ccode == 255] <- 260
fearon_elf$ccode[fearon_elf$ccode == 340] <- 345
fearon_elf$ccode[fearon_elf$ccode == 678] <- 670

idea <- import(paste0(data_prefix, "idea.dta"))
colnames(idea)[2:4] <- c("ccode", "violent_protest", "nonviolent_protest")
idea$year <- as.integer(idea$year)
idea$ccode <- as.integer(idea$ccode)
assert_that(!anyDuplicated(idea))

data_list <- list(gw, fariss, gtd, mid, ucdp_conflict, uds, uds_xpolity, poly, polity,
                  geddes, ciri, ksg, epr_exclpop, epr_oil, idea, xpolity)
data_list <- lapply(data_list, function(x) ccode_fix(x))

df <- Reduce(function(x, y) left_join(x, y, by = c("ccode", "year")), data_list)
df <- left_join(df, fearon_elf, by = "ccode")
df <- df[!duplicated(df), ] ## not sure where this is coming from, investigate further
assert_that(!anyDuplicated(df))

df <- df %>% mutate(max_hostlevel = ifelse(is.na(max_hostlevel) & year >= 1816, 0, max_hostlevel),
                    cwar_onset = ifelse(is.na(cwar_onset) & year >= 1946, 0, cwar_onset),
                    cconflict_onset = ifelse(is.na(cconflict_onset) & year >= 1946, 0, cconflict_onset),
                    cwar_count = ifelse(is.na(cwar_count) & year >= 1946, 0, cwar_count),
                    cconflict_count = ifelse(is.na(cconflict_count) & year >= 1946, 0, cconflict_count),
                    terror_killed = ifelse(is.na(terror_killed) & year >= 1970, 0, terror_killed),
                    terror_events = ifelse(is.na(terror_events) & year >= 1970, 0, terror_events),
                    violent_protest = ifelse(is.na(violent_protest) & year >= 1990, 0, violent_protest),
                    nonviolent_protest = ifelse(is.na(nonviolent_protest) & year >= 1990, 0, nonviolent_protest))
  
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

out_file <- paste0(dir_prefix, "data/", start_year, "_", end_year, "_", "rep.csv")
export(df[df$year <= end_year & df$year >= start_year, ], out_file, "csv")
