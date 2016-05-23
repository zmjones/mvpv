seed <- 1987
set.seed(seed)

pkgs <- c("devtools", "rio", "stringr", "uds")
invisible(sapply(pkgs, library, character.only = TRUE))
path <- unlist(str_split(getwd(), "/"))
dir_prefix <- ifelse(path[length(path)] == "R", "../", "./")
r_dir_prefix <- ifelse(path[length(path)] == "mvpv", "R/", "./")

source(paste0(r_dir_prefix, "functions.R"))

xpolity <- import(paste0(dir_prefix, "data/xpolity.csv"))[-3]
colnames(xpolity)[1] <- "cowcode"

xpolity <- xpolity[, colnames(xpolity) %in% c("cowcode", "year", "xpolity_nas")]
xpolity <- xpolity[!apply(is.na(xpolity), 1, any), ]
xpolity <- xpolity[xpolity$year >= 1946 & xpolity$year <= 2000, ]
colnames(xpolity)[3] <- "polity"

data(democracy1946.2008)

democracy[["polity"]] <- NULL
democracy <- merge(democracy, xpolity, by = c("cowcode", "year"), all.x = TRUE)

data <- prepare.uds(democracy)

post.uds <- mroprobit(data, 
                      sigma.mh = c(0.015, 0.065, 0.022, 0.006, 0.043, 0.044, 0.0025, 0.024, 0.012, 0.01),
                      save.traits = TRUE, save.perceived.traits = TRUE, mcmc = 500000, burnin = 500000,
                      thin = 100, verbose = 100000, seed = seed, chains = 2, disperse = FALSE)
make.z.summary(post.uds, democracy$cowcode, thin = 1, file = "../data/uds_xpolity.csv")






