# data - a matrix of integers (gets rounded to the nearest int if not)
#   representing ordinal rankings.  One column per rater, one row per
#   rated observation (nXJ).  Values <= 0 and NAs are treated as missing
#   values.
# sigma.h - a vector of real values containing metropolis-hasting
#   tuning parameters, one per rater.  Default value 0.4 / max(C_j),
#   where max(C_j) is the number of categories for rater j.
# alpha - a scalar real IG prior shape parameter for all sigmasq
# lambda - a scalar real IG prior scale parameter for all sigmasq
# mu0 - normal prior mean for the Zs, either a scalar real applied to
#   each Z, or a vector of means, one for each observation
# sigmasq0 - normal prior variance for the Zs, either a scalar real
#   (>0) applied to each Z, or a vector of variances, one for each
#   observation.
# sigmasq.start - start values for the variances.  A numeric vector of
#   length J, all items > 0
# gamma.start - start values for the cutoffs.  A numeric vector of
#   length sum(k_C)-J with all values > -10 and  < 10.
# t.start - start values for the rater perceptions.  An nXJ numeric
#   matrix with ratings for every corresponding non-missing element in
#   data.  You can put whatever (NaN, NA, or a number even) in the
#   elements that correspond to NAs or values <= 0 in data.
# disperse - should function disperse starting values?  If true, then
#   adds large random shocks to starting values.  Typically used for
#   multiple chain runs.
# chains - the number of chains to fit.
# save.traits - should we save Zs?
# save.perceived.traits - should we save ts?  WARNING: can take a ton
#   of RAM.
# mcmc - number of mcmc its
# burnin - number of burnin its
# thin - thinning interval. must divide evenly into mcmc.
# verbose - how many its between status prints?
# seed - the random number seed.
#
# TODO: Allow multiple starting values, one per chain.

mroprobit <- function (data, sigma.mh=NULL, alpha = 0.2, lambda = 0.1,
                       mu0 = 0, sigmasq0 = 1, sigmasq.start = NULL,
                       gamma.start = NULL, t.start = NULL,
                       disperse = FALSE, chains = 1, save.traits = TRUE,
                       save.perceived.traits = FALSE, mcmc = 5000,
                       burnin = 1000, thin = 1, verbose = 1, seed = 12345) 
{
  # Check input
  if (!is.numeric(alpha) | length(alpha) != 1)
    stop("alpha must contain a single real value")
  if (!is.numeric(lambda) | length(lambda) != 1)
    stop("lambda must contain a single real value")
  if (!is.numeric(mu0) | (length(mu0) != 1 & length(mu0) != nrow(data)))
    stop("mu0 must contain a single real value, or as many reals as data has rows")
  if (!is.numeric(sigmasq0) | 
      (length(sigmasq0) != 1 & length(sigmasq0) != nrow(data)) |
      any(sigmasq0 <= 0))
    stop("sigmasq0 must contain a single real value > 0, or as many reals > 0 as data has rows")
  if (!is.numeric(chains) | length(chains) != 1 | chains < 1)
    stop("chains should be a scalar and strictly positive integer")
  if (!is.logical(save.traits) | length(save.traits) != 1)
    stop("save.traits must contain a single logical value")
  if (!is.logical(save.perceived.traits)|length(save.perceived.traits) != 1)
    stop("save.perceived.traits must contain a single logical value")
  if (!is.numeric(mcmc) | length(mcmc) != 1 | mcmc < 1)
    stop("mcmc must be a scalar integer > 0")
  if (!is.numeric(burnin) | length(burnin) != 1 | burnin < 0)
    stop("burnin must be a scalar integer >= 0")
  if (!is.numeric(verbose) | length(verbose) != 1 | verbose < 1)
    stop("verbose must be a scalar integer > 0")
  if (!is.numeric(thin) | length(thin) != 1 | thin < 1 | thin > mcmc)
    stop("thin must be a scalar integer > 0 and < mcmc")
  if (mcmc %% thin != 0)
    stop("thin does not divide mcmc evenly")
  if (!is.numeric(mcmc) | length(mcmc) != 1 | seed < 1)
    stop("mcmc must be a scalar integer > 0")
  if (!is.logical(disperse) | length(disperse) != 1)
    stop("disperse should be a single logical value")

  # Get data in the correct format for the C code
  data[is.na(data)] <- 0

  # Setup sigma.mh default and check sigma.mh input
  if (is.null(sigma.mh))
    sigma.mh <- 0.4 / apply(data, 2, max)
  if (!is.numeric(sigma.mh) | length(sigma.mh) != ncol(data))
    stop("sigma.mh must be a real-valued vector with the same number of elements as data has columns")

 # convert mcmc, burnin, etc
  mcmc <- as.integer(mcmc)
  burnin <- as.integer(burnin)
  thin <- as.integer(thin)
  verbose <- as.integer(verbose)

  # dimensions
  n <- as.integer(nrow(data))
  J <- as.integer(ncol(data))

  # setup multiple chains
  chains <- as.integer(chains)

  # calculate posterior storage sizes
  sigmasq.len <- (mcmc/thin) * J * chains
  gamma.len <- (mcmc/thin) * (J * (max(data) + 1)) * chains
  z.len <- 0
  t.len <- 0
  if (save.traits)
    z.len <- (mcmc/thin) * n * chains
  if (save.perceived.traits)
    t.len <- (mcmc/thin) * n * J * chains

  # prior length info
  mu0.long <- length(mu0) > 1
  sigmasq0.long <- length(sigmasq0) > 1

  # Check and setup start values
  have.t.start <- FALSE
  if (! is.null(t.start)) {
    if (! is.numeric(t.start) | nrow(t.start) != n | ncol(t.start) != J)
      stop("t.start must be a nXJ numeric matrix")
    t.start <- as.double(t(t.start)) # transpose to row order
    have.t.start <- TRUE
  }

  have.gamma.start <- FALSE
  if (! is.null(gamma.start)) {
    gamma.lengths <- apply(data, 2, max, na.rm=T)
    if (! is.numeric(gamma.start) | 
        length(gamma.start) != sum(gamma.lengths)-J |
        any(abs(gamma.start)) > 10)
      stop("gamma.start should be a numeric vector of length ",
           sum(gamma.lengths-J), " with no values < -10 or > 10")
    gamma.tmp <- gamma.start
    gamma.start <- matrix(NaN, nrow=max(gamma.lengths)+1, ncol=J)
    cur <- 1
    for (j in 1:J) {
      gamma.start[0, j] <- -10
      gamma.start[gamma.lengths[j]+1, j] <- 10
      for (c in 2:(gamma.lengths[j])) {
        gamma.start[c, j] <- gamma.start[cur]
        cur <- cur + 1
      }
    }
    gamma.start <- as.double(gamma.start)
    have.gamma.start <- TRUE
  }

  have.sigmasq.start <- FALSE
  if (! is.null(sigmasq.start)) {
    if (! is.numeric(sigmasq.start) | any(sigmasq.start <= 0) |
        length(sigmasq.start) != J)
      stop("sigmasq.start should be a J-length numeric vector > 0")
    sigmasq.start <- as.double(sigmasq.start)
    have.sigmasq.start <- TRUE
  }

  posterior <- .C("mrop", n, J, as.integer(t(data)), as.double(sigma.mh),
    as.double(alpha), as.double(lambda), as.double(mu0),
    as.integer(mu0.long), as.double(sigmasq0),
    as.integer(sigmasq0.long), as.double(sigmasq.start),
    have.sigmasq.start, as.double(gamma.start), have.gamma.start,
    as.double(t.start), have.t.start, chains, as.integer(disperse),
    as.integer(save.traits), as.integer(save.perceived.traits), mcmc,
    burnin, thin, verbose, as.integer(seed),
    sigmasq=double(sigmasq.len), gamma=double(gamma.len),
    z=double(z.len), t=double(t.len), failed=integer(chains), NAOK=T,
    DUP=F)
  
  for (i in 1:chains)
    if (posterior$failed[i] != 0)
        stop("Sampler for chain ", i, " stopped prematurely")

  # postprocess
  rnames <- colnames(data)
  onames <- rownames(data)

  sigmasq <- gamma <- t <- z <- NULL
  for (c in 1:chains) {
    sigmasq[[c]] <- matrix(posterior$sigmasq[((c-1)*(mcmc/thin)*J + 1):(c*(mcmc/thin)*J)], ncol=J, byrow=T)
    colnames(sigmasq[[c]]) <- rnames
    sigmasq[[c]] <- mcmc(sigmasq[[c]], start=burnin+1,
                         end=burnin+mcmc, thin=thin)
  }
  posterior$sigmasq <- NULL

  gamma.lengths <- apply(data, 2, max, na.rm=T)
  gamma.maxlen <- max(data)+1
  for (c in 1:chains) {
    gamma.tmp <- matrix(posterior$gamma[((c-1)*(mcmc/thin)*(J*(max(data)+1))+1):(c*(mcmc/thin)*(J*(max(data)+1)))], ncol=J*gamma.maxlen, byrow=T)

    gammac <- NULL
    cur <- 1
    i <- 1
    for (len in gamma.lengths) {
      gamma.cur <- as.matrix(gamma.tmp[, (cur+1):(cur+gamma.lengths[i] - 1)])
      colnames(gamma.cur) <- paste(rnames[i], 1:ncol(gamma.cur))
      gammac <- cbind(gammac, gamma.cur)
      cur <- gamma.maxlen*i + 1
      i <- i + 1
    }
    gamma[[c]] <- mcmc(gammac, start=burnin+1, end=burnin+mcmc, thin=thin)
  }
  posterior$gamma <- NULL

  if (save.traits) {
    for (c in 1:chains) {
      z[[c]] <- matrix(posterior$z[((c-1)*(mcmc/thin)*n+1):(c*(mcmc/thin)*n)], ncol=n, byrow=T)
      colnames(z[[c]]) <- onames
      z[[c]] <- mcmc(z[[c]], start=burnin+1, end=burnin+mcmc, thin=thin)
    }
    posterior$z <- NULL
  }

  if (save.perceived.traits) {
    for (c in 1:chains) {
      tc <- matrix(posterior$t[((c-1)*(mcmc/thin)*n*J+1):(c*(mcmc/thin)*n*J)], byrow=T, ncol=n*J)
      colnames(tc) <- unlist(lapply(rnames, function (x)
        paste(x, onames)))
      keep <- !is.nan(tc[1,])
      for (j in 1:J) {
        start <- (j-1) * n + 1
        end <- j * n
        t[[rnames[j]]][[c]] <- mcmc(tc[,(start:end)[keep[start:end]]], 
                                    start=burnin+1, end=burnin+mcmc,
                                    thin=thin)
      }
      tc <- NULL
    }
    posterior$t <- NULL
  }

  if (chains == 1) {
    sigmasq <- sigmasq[[1]]
    gamma <- gamma[[1]]
    z <- z[[1]]
    for (j in 1:J)
      t[[j]] <- t[[j]][[1]]
  } else {
    sigmasq <- mcmc.list(sigmasq)
    gamma <- mcmc.list(gamma)
    if (save.traits)
      z <- mcmc.list(z)
    if (save.perceived.traits)
      for (j in 1:J)
        t[[rnames[j]]] <- mcmc.list(t[[rnames[j]]])
  }
 
  # Turn missing values back into NAs
  data[data<1] <- NA

  # Return fitted model object
  posterior <- list(sigmasq=sigmasq, gamma=gamma, z=z, t=t,
    data=data, sigma.mh=sigma.mh, alpha=alpha, lambda=lambda,
    mu0=mu0, sigmasq0=sigmasq0, seed=seed)
  class(posterior) <- "mroprobit.fit"

  return(posterior)
}
