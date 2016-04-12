varpic <- function(post, intfunc=HPDinterval, names=NULL,
                   margins=c(5,5,2,2), fg="black", bg="white", chain=NULL)
{
  if (!is.null(chain) & is.mcmc.list(post$sigmasq))
    post <- post$sigmasq[[chain]]
  else if (is.mcmc.list(post$sigmasq))
    post <- mcmc(concat.mcmc.list(post$sigmasq))
  else
    post <- post$sigmasq
  
  means <- apply(post, 2, mean)
  interval <- intfunc(post)

  if (is.null(names))
    names <- colnames(post)

  par(mar=margins)
  plot(0,0, xlim=c(1,length(means)), ylim=c(0,.3), type='n',
    ylab='Error Variance', xlab='', axes=F, cex.lab=1, col.lab=fg)

  axis(1, at=1:length(names), labels=names, cex.axis=.9, col.axis=fg)
  axis(2, cex.axis=.9, col.axis=fg)
  points(means, pch=20, lwd=2)

  for (i in 1:length(means)) {
    segments(i, interval[i,1], i, interval[i,2], lwd=1)
    segments(i-.2, interval[i,1], i+.2, interval[i,1], lwd=1)
    segments(i-.2, interval[i,2], i+.2, interval[i,2], lwd=1)
  }
}


# A default label position vector
labelpos<-c(1, 7, 9, 18, 30, 37, 38, 58, 68, 71, 78)
colorpos<-c(1, 7, 9, 18, 30, 37, 39, 58, 68, 71, 78)

# Generate a graph of measure cutpoint estimates with quantiles.
cutpic <- function(post, intfunc=HPDinterval, names=NULL,
									 margins=c(6,5,0,1), fg="black", bg="white",
                   spacer=2, chain=NULL, nguides=5,
                   guidebounds=c(-2,2))
{
  if (!is.null(chain) & is.mcmc.list(post$gamma))
    post <- post$gamma[[chain]]
  else if (is.mcmc.list(post$gamma))
    post <- mcmc(concat.mcmc.list(post$gamma))
  else
    post <- post$gamma
  
  intervals <- intfunc(post)

  rawnames <- unlist(lapply(strsplit(colnames(post), ' '), function (x) x[[1]]))
  if (is.null(names))
    names <- unique(rawnames)

  
  raterpos <- 1
  labelpos <- 1
  for (i in 2:ncol(post)) {
    if (rawnames[i] != rawnames[i-1]) {
      raterpos <- c(raterpos, i)
      labelpos <- c(labelpos, i+(spacer*(length(raterpos)-1)))
    }
  }

  end <- length(intervals[,1])+spacer*(length(labelpos)-1)
  ylim <- c(min(intervals)-.5,max(intervals)+.2)
	par(bg=bg, fg=fg, mar=c(6,5,0,1))
  plot(0, 0, xlim=c(1,end+1), ylim=ylim, type='n',
       ylab='Unified Democracy Level', xlab='', axes=F, cex.lab=1,
			 col.lab=fg)
  axis(1, at=c(labelpos, end), labels=c(names, ""), las=3, cex.axis=.9,
			 col.axis=fg)
  axis(2, cex.axis=.9, col.axis=fg)
  
  cur <- "white"
  for (i in 1:(length(labelpos)-1)) {
    rect(labelpos[i]-1, guidebounds[1], labelpos[i+1]-2, guidebounds[2],
         col=cur, border=NA)
    if (cur == "white")
      cur <- "lightgrey"
    else
      cur <- "white"
  }
  rect(labelpos[length(labelpos)]-1, guidebounds[1], end+1, guidebounds[2],
       col=cur, border=NA)

  if (!is.null(nguides))
    for (h in seq(guidebounds[1], guidebounds[2], length.out=nguides))
      abline(h=h, col="lightgrey", lty=2)

  cur <- 1
  for (i in 1:length(intervals[,1])) {
    if (i != 1 & i %in% raterpos)
      cur <- cur + spacer
    segments(cur, intervals[i,1], cur, intervals[i,2], lwd=1)
    segments(cur-.2, intervals[i,1], cur+.2, intervals[i,1], lwd=1)
    segments(cur-.2, intervals[i,2], cur+.2, intervals[i,2], lwd=1)
    cur <- cur + 1
  }
}


latentpic <- function(zs, names, clen=length(names), xlim=c(-2.5, 3.5),
                      tdist=.5, cex.xaxis=1, line.xaxis = 0,
                      padj.top = 0, padj.bottom = 0, cex.yaxis=.8,
                      lwd.conf=1, lwd.pnt=1, mar=c(2, 8, 2, 1),
                      topaxis=T, bottomaxis=T, fname=NULL,
                      fwidth=6, fheight=6)
{
  zint <- HPDinterval(zs)
  zsum <- cbind(apply(zs, 2, mean), zint[,1], zint[,2])
  working <- data.frame(zsum)
  working$name <- names
  working <- working[do.call(order, working),]
  size <- length(working[,1])

  if (is.null(fname))
    par(mar=mar, mfcol=c(1, ceiling(size / clen)))
  
  remaining <- size
  start <- 1
  page <- 0
  while (remaining > 0) {
    page <- page + 1
    len <- min(remaining, clen)

    if (! is.null(fname)) {
      pdf(paste(fname, page, ".pdf", sep=""), width=fwidth, height=fheight)
      par(mar=mar)
    }

    plot(0, 0, xlim=xlim, ylim=c(1, len), type='n', axes=F, xlab='',ylab='')
    if (bottomaxis) {
      axis(1, at=seq(xlim[1], xlim[2], tdist), cex.axis=cex.xaxis,
           line=line.xaxis, padj=padj.bottom)

    }
    if (topaxis) {
      axis(3, at=seq(xlim[1], xlim[2], tdist), cex.axis=cex.xaxis,
           line=line.xaxis, padj=padj.top)
    }
    axis(2, at = 1:len, cex.axis=cex.yaxis, las = 1, lty = 0, tick=FALSE,
         labels=as.character(working$name[start:(start + len - 1)]))
    segments(seq(xlim[1], xlim[2], tdist), 1, seq(xlim[1], xlim[2], tdist),
             len, lty=2)

    x <- 1
    for (i in start:(start + len - 1)) {
      segments(working[i, 2], x, working[i, 3], x, lwd=lwd.conf)
      x <- x + 1
    }
    points(working[start:(start+len-1), 1], 1:len, pch=20, lwd=lwd.pnt)

    remaining <- remaining - clen
    start <- start + clen

    if (! is.null(fname))
      dev.off()
  }
}

measure.hist <- function (orig, samples, d=.25, ...) {
  # Drop out-of-sample predictions
  samples[is.na(orig),] <- NA

  # How many sims
  sims <- dim(samples)[2]
  
  # Calculate the post-pred 95 and 50 pctiles
  bins <- apply(samples, 2, function (x) tabulate(x[!is.na(x)]))
  pctiles <- apply(bins, 1, function (x) sort(x)[sims*c(.025, .25, .75, .975)])

  #hpd50 <- HPDinterval(mcmc(t(bins)), prob=.5)
  #hpd95 <- HPDinterval(mcmc(t(bins)))

  #pctiles <- rbind(hpd95[,1], hpd50[,1], hpd50[,2], hpd95[,2])

  #print(pctiles)
  # Draw the histogram
  nbins <- length(unique(orig[!is.na(orig)]))
  truehist(orig, nbins = nbins, prob=FALSE, col="gray", ymax=max(pctiles),
           xlab='Rating Levels', ylab='Rating Counts', ...)

  for (i in 1:nbins) {
    l <- i + d
    r <- i + 1 - d
    c <- i + .5

    if (nbins < 4) {
      l <- i + d/3
      r <- i + .5 - d/3
      c <- i + .25
    }

    segments(l, pctiles[1, i], r, pctiles[1, i], lty=1)
    segments(l, pctiles[4, i], r, pctiles[4, i], lty=1)

    polygon(x=c(l, r, r, l),
            y=c(rep(pctiles[2, i], 2), rep(pctiles[3, i], 2)), lty=1)
  
    segments(c, pctiles[1, i], c, pctiles[4, i], lty=1)
  }
}


ppd.hist <- function (post, samples, d=.25, 
                           names=c("Arat", "BLM", "Bollen", "F. House",
                                   "Hadenius", "PACL", "Polity",
                                   "Polyarchy", "PRC", "Vanhanen"))
{
  orig <- post$data

  len <- length(names)
  par(mfrow=c(2, ceiling(len / 2)))
  for (i in 1:len) {
    measure.hist(orig[,i], samples[,i,], d, main=names[i])
  }
}
