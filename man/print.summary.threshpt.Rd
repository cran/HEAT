\name{print.summary.threshpt}
\alias{print.summary.threshpt}

\title{
Summary for a Threshpt fit
}
\description{
A default print method for summary informations for a \code{threshpt} object.
}
\usage{
\method{print}{summary.threshpt}(x, ...)
}

\arguments{
  \item{x}{
Summary informations for a fitted \code{threshpt} object produced by threshpt() 
}
  \item{\dots}{
Not used
}
}

\value{
  \item{Formula }{the formula which is used in the threshpt function}
  \item{Best fit }{estimated parameter coefficients of model with the minimum deviance}
  \item{Deviance }{deviance of fitted model}
  \item{Threshold }{threshold value of the model with the minimum deviance}
}


\author{
Youn-Hee Lim, Il-Sang Ohn,  and Ho Kim
}

