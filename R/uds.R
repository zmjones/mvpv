seed <- 1987
set.seed(seed)

pkgs <- c("devtools", "stringr", "uds", "coda")
invisible(sapply(pkgs, library, character.only = TRUE))
path <- unlist(str_split(getwd(), "/"))
dir_prefix <- ifelse(path[length(path)] == "R", "../", "./")
r_dir_prefix <- ifelse(path[length(path)] == "mvpv", "R/", "./")

source(paste0(r_dir_prefix, "functions.R"))

xpolity <- read.csv(paste0(dir_prefix, "data/xpolity.csv"))
colnames(xpolity)[1] <- "cowcode"

xpolity <- xpolity[xpolity$year >= 1946 & xpolity$year <= 2000, ]
colnames(xpolity)[3] <- "polity"

data(democracy1946.2008)

democracy[["polity"]] <- NULL
democracy <- merge(democracy, xpolity, by = c("cowcode", "year"), all.x = TRUE)

data <- prepare.uds(democracy)

post.uds <- mroprobit(data, 
  sigma.mh = c(0.015, 0.065, 0.022, 0.006, 0.043, 0.044, 0.0025, 0.024, 0.012, 0.01),
  save.traits = TRUE, save.perceived.traits = TRUE, mcmc = 1000000, burnin = 500000,
  thin = 200, verbose = 100000, seed = seed, chains = 4, disperse = TRUE)

geweke <- geweke.diag(post.uds$z)

make.z.summary(post.uds, democracy$cowcode, thin = 1, file = "../data/uds_xpolity.csv")






