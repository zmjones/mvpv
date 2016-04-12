make.sigmasq.csv <- function (post, file=NULL, chain=NULL, thin=NULL) {
  if (!is.null(chain) & is.mcmc.list(post$sigmasq))
    post <- post$sigmasq[[chain]]
  else if (is.mcmc.list(post$sigmasq))
    post <- concat.mcmc.list(post$sigmasq)
  else
    post <- post$sigmasq

  if (!is.null(thin))
    post <- post[seq(1, nrow(post), thin), ]

  rater <- colnames(post)
  m <- apply(post, 2, mean)
  s <- apply(post, 2, sd)
  res <- data.frame(rater, m, s, t(post))
  colnames(res) <- c("rater", "mean", "sd",
                     paste("sigmasq", 1:(ncol(res)-3), sep=""))
  if (!is.null(file))
    write.csv(res, file, row.names=FALSE)
  return(res)
}

make.sigmasq.summary <- function (post, file=NULL, chain=NULL, thin=NULL) {
  if (!is.null(chain) & is.mcmc.list(post$sigmasq))
    post <- post$sigmasq[[chain]]
  else if (is.mcmc.list(post$sigmasq))
    post <- concat.mcmc.list(post$sigmasq)
  else
    post <- post$sigmasq

  if (!is.null(thin))
    post <- post[seq(1, nrow(post), thin), ]

  rater <- colnames(post)
  m <- apply(post, 2, mean)
  s <- apply(post, 2, sd)
  pct <- t(apply(post, 2, quantile, probs=c(.025, .975)))
  med <- apply(post, 2, median)
  res <- data.frame(rater, m, s, med, pct)
  colnames(res) <- c("rater", "mean", "sd", "median", "pct025", "pct975")
  if (!is.null(file))
    write.csv(res, file, row.names=FALSE)
  return(res)
}

make.gamma.csv <- function (post, file=NULL, chain=NULL, thin=NULL) {
  if (!is.null(chain) & is.mcmc.list(post$sigmasq))
    post <- post$gamma[[chain]]
  else if (is.mcmc.list(post$gamma))
    post <- concat.mcmc.list(post$gamma)
  else
    post <- post$gamma

  if (!is.null(thin))
    post <- post[seq(1, nrow(post), thin), ]

  n <- strsplit(colnames(post), ' ')
  m <- apply(post, 2, mean)
  s <- apply(post, 2, sd)
  rater <- unlist(lapply(n, function (x) x[[1]]))
  cut <- unlist(lapply(n, function (x) x[[2]]))
  res <- data.frame(rater, cut, m, s, t(post))
  colnames(res) <- c("rater", "cut", "mean", "sd",
                     paste("gamma", 1:(ncol(res)-4), sep=""))
  if (!is.null(file))
    write.csv(res, file, row.names=FALSE)
  return(res)
}

make.gamma.summary <- function (post, file=NULL, chain=NULL, thin=NULL) {
  if (!is.null(chain) & is.mcmc.list(post$sigmasq))
    post <- post$gamma[[chain]]
  else if (is.mcmc.list(post$gamma))
    post <- concat.mcmc.list(post$gamma)
  else
    post <- post$gamma

  if (!is.null(thin))
    post <- post[seq(1, nrow(post), thin), ]

  n <- strsplit(colnames(post), ' ')
  m <- apply(post, 2, mean)
  s <- apply(post, 2, sd)
  pct <- t(apply(post, 2, quantile, probs=c(.025, .975)))
  med <- apply(post, 2, median)
  rater <- unlist(lapply(n, function (x) x[[1]]))
  cut <- unlist(lapply(n, function (x) x[[2]]))
  res <- data.frame(rater, cut, m, s, med, pct)
  colnames(res) <- c("rater", "cut", "mean", "sd", "median", "pct025",
    "pct975")
  if (!is.null(file))
    write.csv(res, file, row.names=FALSE)
  return(res)
}

# XXX assumes colname format is "# country name year"
make.z.csv <- function (post, cow, file=NULL, chain=NULL, thin=NULL) {
  if (!is.null(chain) & is.mcmc.list(post$z))
    post <- post$z[[chain]]
  else if (is.mcmc.list(post$z))
    post <- concat.mcmc.list(post$z)
  else
    post <- post$z
  if (!is.null(thin))
    post <- post[seq(1, nrow(post), thin), ]
    
  n <- strsplit(colnames(post), ' ')
  m <- apply(post, 2, mean)
  s <- apply(post, 2, sd)
  country <- unlist(lapply(n, function (x) {
    l <- length(x) - 1
    paste(x[2:l], collapse=' ')
  }))
  year <- unlist(lapply(n, function (x) x[[length(x)]]))
  res <- data.frame(country, year, cow, m, s, t(post))
  colnames(res) <- c("country", "year", "cowcode", "mean", "sd",
                     paste("z", 1:(ncol(res)-5), sep=""))
  if (!is.null(file)) {
    write.csv(res, file, row.names=FALSE)
    return("file written");
  }
  return(res)
}

make.z.summary <- function (post, cow, file=NULL, chain=NULL, thin=NULL) {
if (!is.null(chain) & is.mcmc.list(post$z))
  post <- post$z[[chain]]
else if (is.mcmc.list(post$z))
  post <- concat.mcmc.list(post$z)
else
  post <- post$z
if (!is.null(thin))
  post <- post[seq(1, nrow(post), thin), ]
  
n <- strsplit(colnames(post), ' ')
m <- apply(post, 2, mean)
med <- apply(post, 2, median)
s <- apply(post, 2, sd)
pct <- t(apply(post, 2, quantile, probs=c(.025, .975)))

country <- unlist(lapply(n, function (x) {
  l <- length(x) - 1
  paste(x[2:l], collapse=' ')
}))
year <- unlist(lapply(n, function (x) x[[length(x)]]))
res <- data.frame(country, year, cow, m, s, med, pct)
colnames(res) <- c("country", "year", "cowcode", "mean", "sd",
  "median", "pct025", "pct975")
if (!is.null(file)) {
  write.csv(res, file, row.names=FALSE)
  return("file written");
}
return(res)
}

make.t.csv <- function (post, rater, original, 
                        file=NULL, chain=NULL, thin=NULL) {
  if (!is.null(chain) & is.mcmc.list(post$t[[rater]]))
    post <- post$t[[rater]][[chain]]
  else if (is.mcmc.list(post$t[[rater]]))
    post <- concat.mcmc.list(post$t[[rater]])
  else
    post <- post$t[[rater]]

  if (!is.null(thin))
    post <- post[seq(1, nrow(post), thin), ]
    
  n <- strsplit(colnames(post), ' ')
  m <- apply(post, 2, mean)
  s <- apply(post, 2, sd)
  post.country <- unlist(lapply(n, function (x) {
    l <- length(x) - 1
    paste(x[3:l], collapse=' ')
  }))
  post.year <- unlist(lapply(n, function (x) x[[length(x)]]))
  cow <- unlist(sapply(1:length(post.country), function (i) subset(original,
    country==post.country[i] & year == post.year[i])$cowcode))
  res <- data.frame(post.country, post.year, cow, m, s, t(post))
  colnames(res) <- c("country", "year", "cowcode", "mean", "sd",
                     paste("t", 1:(ncol(res)-5), sep=""))
  if (!is.null(file)) {
    write.csv(res, file, row.names=FALSE)
    return("file written");
  }
  return(res)
}

make.t.summary <- function (post, rater, original, 
                        file=NULL, chain=NULL, thin=NULL) {
  if (!is.null(chain) & is.mcmc.list(post$t[[rater]]))
    post <- post$t[[rater]][[chain]]
  else if (is.mcmc.list(post$t[[rater]]))
    post <- concat.mcmc.list(post$t[[rater]])
  else
    post <- post$t[[rater]]

  if (!is.null(thin))
    post <- post[seq(1, nrow(post), thin), ]
    
  n <- strsplit(colnames(post), ' ')
  m <- apply(post, 2, mean)
  s <- apply(post, 2, sd)
  med <- apply(post, 2, median)
  pct <- t(apply(post, 2, quantile, probs=c(.025, .975)))
  post.country <- unlist(lapply(n, function (x) {
    l <- length(x) - 1
    paste(x[3:l], collapse=' ')
  }))
  post.year <- unlist(lapply(n, function (x) x[[length(x)]]))
  cow <- unlist(sapply(1:length(post.country), function (i) subset(original,
    country==post.country[i] & year == post.year[i])$cowcode))
  res <- data.frame(post.country, post.year, cow, m, s, med, pct)
  colnames(res) <- c("country", "year", "cowcode", "mean", "sd",
                     "median", "pct025", "pct975")
  if (!is.null(file)) {
    write.csv(res, file, row.names=FALSE)
    return("file written");
  }
  return(res)
}

