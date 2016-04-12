# Convert the democracy dataset into a format suitable for analysis
# with mroprobit, discretizing the continuous measures.  We use the
# following cuptoints for the continuous measures: Arat: 50-100, by
# tens; Bollen: 10-90, by 10s; Hadenius: 1, 2, 3, 4, 7, 8, 9;
# Vanhanen: 5-35, by fives. We are forced to jump between cuts at
# Hadenius scores of four and seven because of a dearth of
# observations at levels five and six. In general, we must sacrifice
# some information and reduce the number of levels in the continuous
# scores in order to have enough observations at each level to
# identify cutpoints.

prepare.uds <- function(data, include.extra=FALSE)
{
  # arat
  data$arat.new[! is.na(data$arat)] <- 7
  data$arat.new[data$arat < 50] <- 1
  data$arat.new[data$arat >= 50 & data$arat < 60] <- 2
  data$arat.new[data$arat >= 60 & data$arat < 70] <- 3
  data$arat.new[data$arat >= 70 & data$arat < 80] <- 4
  data$arat.new[data$arat >= 80 & data$arat < 90] <- 5
  data$arat.new[data$arat >= 90 & data$arat < 100] <- 6

  # Hadenius
  data$hadenius.new[! is.na(data$hadenius)] <- 8
  data$hadenius.new[data$hadenius < 1] <- 1
  data$hadenius.new[data$hadenius >= 1 & data$hadenius < 2] <- 2
  data$hadenius.new[data$hadenius >= 2 & data$hadenius < 3] <- 3
  data$hadenius.new[data$hadenius >= 3 & data$hadenius < 4] <- 4
  data$hadenius.new[data$hadenius >= 4 & data$hadenius < 7] <- 5
  data$hadenius.new[data$hadenius >= 7 & data$hadenius < 8] <- 6
  data$hadenius.new[data$hadenius >= 8 & data$hadenius < 9] <- 7

  # Bollen
  data$bollen.new[! is.na(data$bollen)] <- 10
  data$bollen.new[data$bollen < 10] <- 1
  data$bollen.new[data$bollen >= 10 & data$bollen < 20] <- 2
  data$bollen.new[data$bollen >= 20 & data$bollen < 30] <- 3
  data$bollen.new[data$bollen >= 30 & data$bollen < 40] <- 4
  data$bollen.new[data$bollen >= 40 & data$bollen < 50] <- 5
  data$bollen.new[data$bollen >= 50 & data$bollen < 60] <- 6
  data$bollen.new[data$bollen >= 60 & data$bollen < 70] <- 7
  data$bollen.new[data$bollen >= 70 & data$bollen < 80] <- 8
  data$bollen.new[data$bollen >= 80 & data$bollen < 90] <- 9

  # Vanhanen
  data$vanhanen.new[! is.na(data$vanhanen)] <- 8
  data$vanhanen.new[data$vanhanen < 5] <- 1
  data$vanhanen.new[data$vanhanen >= 5 & data$vanhanen < 10] <- 2
  data$vanhanen.new[data$vanhanen >= 10 & data$vanhanen < 15] <- 3
  data$vanhanen.new[data$vanhanen >= 15 & data$vanhanen < 20] <- 4
  data$vanhanen.new[data$vanhanen >= 20 & data$vanhanen < 25] <- 5
  data$vanhanen.new[data$vanhanen >= 25 & data$vanhanen < 30] <- 6
  data$vanhanen.new[data$vanhanen >= 30 & data$vanhanen < 35] <- 7

  final <- NULL

  if (include.extra) {
    # Munck, more than half of obs equal 1, the top value
    data$munck.new[! is.na(data$munck)] <- 4
    data$munck.new[data$munck < .5] <- 1
    data$munck.new[data$munck >= .5 & data$munck < .75] <- 2
    data$munck.new[data$munck >= .75 & data$munck < 1] <- 3

    # Create the data matrix.  Every measure is already ordinal but we
    # want them on a 1 and up scale.  This does this.
    final <- data.frame(cbind(
      unclass(factor(data$arat.new)), unclass(factor(data$blm)),
      unclass(factor(data$bollen.new)),
      unclass(factor(data$freedomhouse)),
      unclass(factor(data$hadenius.new)), unclass(factor(data$pacl)),
      unclass(factor(data$polity)), unclass(factor(data$polyarchy)),
      unclass(factor(data$prc)), unclass(factor(data$vanhanen.new)),
      unclass(factor(data$munck.new)), unclass(factor(data$mainwaring))))

    # Set the column names
    names(final)<-c('arat', 'blm', 'bollen', 'freedomhouse',
      'hadenius', 'pacl', 'polity', 'polyarchy', 'prc', 'vanhanen',
      'munck', 'mainwaring')
  } else {
    # Create the data matrix.  Every measure is already ordinal but we
    # want them on a 1 and up scale.  This does this.
    final <- data.frame(cbind(
      unclass(factor(data$arat.new)), unclass(factor(data$blm)),
      unclass(factor(data$bollen.new)),
      unclass(factor(data$freedomhouse)),
      unclass(factor(data$hadenius.new)), unclass(factor(data$pacl)),
      unclass(factor(data$polity)), unclass(factor(data$polyarchy)),
      unclass(factor(data$prc)), unclass(factor(data$vanhanen.new))))

    # Set the column names
    names(final)<-c('arat', 'blm', 'bollen', 'freedomhouse',
      'hadenius', 'pacl', 'polity', 'polyarchy', 'prc', 'vanhanen')
  }

  # XXX # in there cause of dups
  rownames(final) <- paste(1:length(data$country), data$country, data$year)
  return (final)
}

# Like the previous function but breaks continuous measures at
# quantiles of their distributions.  Defaults to deciles.

prepare.quantile <- function (data, probs=seq(.1, .9, .1), ...)
{
  # arat
  index <- 1
  prevcut <- -1000
  for (cut in c(quantile(data$arat, probs=probs, na.rm=T, ...), 
                max(data$arat, na.rm=T)+1)) {
    data$arat.new[data$arat >= prevcut & data$arat < cut] <- index
    index <- index + 1
    prevcut <- cut
  }

  # Hadenius
  index <- 1
  prevcut <- -1000
  for (cut in c(quantile(data$hadenius, probs=probs, na.rm=T, ...), 
                max(data$hadenius, na.rm=T)+1)) {
    data$hadenius.new[data$hadenius >= prevcut & data$hadenius < cut] <- index
    index <- index + 1
    prevcut <- cut
  }

  # Bollen
  index <- 1
  prevcut <- -1000
  for (cut in c(quantile(data$bollen, probs=probs, na.rm=T, ...), 
                max(data$bollen, na.rm=T)+1)) {
    data$bollen.new[data$bollen >= prevcut & data$bollen < cut] <- index
    index <- index + 1
    prevcut <- cut
  }

  # Vanhanen
  index <- 1
  prevcut <- -1000
  for (cut in c(quantile(data$vanhanen, probs=probs, na.rm=T, ...), 
                max(data$vanhanen, na.rm=T)+1)) {
    data$vanhanen.new[data$vanhanen >= prevcut & data$vanhanen < cut] <- index
    index <- index + 1
    prevcut <- cut
  }

  # Create the data matrix.  Every measure is already ordinal but we
  # want them on a 1 and up scale.  This does this.
  final <- data.frame(cbind( unclass(factor(data$arat.new)),
    unclass(factor(data$blm)), unclass(factor(data$bollen.new)),
    unclass(factor(data$freedomhouse)),
    unclass(factor(data$hadenius.new)), unclass(factor(data$pacl)),
    unclass(factor(data$polity)), unclass(factor(data$polyarchy)),
    unclass(factor(data$prc)), unclass(factor(data$vanhanen.new))))

  # Set the column names
  names(final)<-c('arat', 'blm', 'bollen', 'freedomhouse', 'hadenius',
    'pacl', 'polity', 'polyarchy', 'prc', 'vanhanen')

  rownames(final) <- paste(1:length(data$country), data$country, data$year)
  return (final)
}
