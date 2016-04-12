library(devtools)
library(rio)
library(countrycode)

install("uds")

library(uds)

xpolity <- import("xpolity.dta")
xpolity$cowcode <- countrycode(xpolity$country_name, "country.name", "cown", warn = TRUE)
xpolity <- xpolity[, colnames(xpolity) %in% c("cowcode", "year", "xpolity")]
xpolity <- xpolity[!apply(is.na(xpolity), 1, any), ]
xpolity <- xpolity[xpolity$year >= 1946 & xpolity$year <= 2000, ]
xpolity$xpolity[xpolity$xpolity < -10] <- NA
colnames(xpolity)[2] <- "polity"

data(democracy1946.2008)

democracy[["polity"]] <- NULL
democracy <- merge(democracy, xpolity, by = c("cowcode", "year"), all.x = TRUE)

data <- prepare.uds(democracy)

post.uds <- mroprobit(data, 
                      sigma.mh = c(0.015, 0.065, 0.022, 0.006, 0.043, 0.044, 0.0025, 0.024, 0.012, 0.01),
                      save.traits = TRUE, save.perceived.traits = TRUE, mcmc = 500000, burnin = 500000,
                      thin = 100, verbose = 100000, seed = 3333, chains = 2, disperse = FALSE)
make.z.summary(post.uds, democracy$cowcode, thin = 1, file = "../data/uds_xpolity.csv")






