\name{summary.threshpt}
\alias{summary.threshpt}

\title{
Summary informations for a fitted \code{threshpt} object.
}

\description{
\code{summary} provides summary statistics for a \code{threshpt} object produced by \code{threshpt()}
}

\usage{
\method{summary}{threshpt}(object, ...)
}

\arguments{
  \item{object}{
a fitted \code{threshpt} object produced by \code{threshpt()}
}
  \item{\dots}{
Not used
}
}
\value{
\code{summary.threshpt} produces a list of summary informations for a fitted \code{threshpt} object with components

  \item{Formula }{the formula which is used in the threshpt function}
  \item{Best fit }{estimated parameter coefficients of model with the minimum deviance}
  \item{Deviance }{deviance of a fitted \code{threshpt} model}
  \item{Threshold }{threshold value of the model with the minimum deviance}
}

\author{
Youn-Hee Lim, Il-Sang Ohn, and Ho Kim
}


\examples{

# read the Seoul data set and create lag variables
data(mort)
seoul = read6city(mort, 11)
seoul_lag = lagdata(seoul, c("meantemp", "mintemp", "meanpm10", "meanhumi"), 5)

# find a optimal threshold and conduct piecewise linear regression
mythresh = threshpt(nonacc ~ meantemp_m3 + meanpm10_m2 +  meanhumi + ns(sn, 4*10) + factor(dow), 
			     expvar = "meantemp_m3", family = "poisson", data = seoul_lag,
			     startrng = 23, endrng = 33, searchunit = 0.2)

# provide summary informations
summary(mythresh)
}