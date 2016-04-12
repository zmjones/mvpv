concat.mcmc.list <- function (post) {
  res <- NULL
  for (i in 1:length(post)) {
    res <- rbind(res, post[[i]])
  }
  return(res)
}
