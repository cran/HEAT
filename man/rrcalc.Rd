\name{rrcalc}
\alias{rrcalc}

\title{
Estimation of relative risk}

\description{
\code{rrcalc} caculates relative risks below and above a threshold. Relative risks and the 95\% C.I.s of lower unit and upper unit based on the threshold are estimated.
}

\usage{
rrcalc(object, rrunit = 1)
}

\arguments{
  \item{object}{
a fitted \code{threshpt} object produced by \code{threshpt()}.
}
  \item{rrunit}{
Unit of relative risk. 
}
}

\details{
In GLM with log link, the coefficients of the exposure are equal to log values of RR. \code{rrcalc} gives relative risks in log link GLM, particularly, Poisson regression model. 
}


\value{
The results of "<Threshold" mean that the relative risk and 95\% confidence interval when the exposure increases by \code{rrunit} below threshold.
The results of ">=Threshold" mean those when the exposure increases by \code{rrunit} above threshold. \cr
RR = exp(beta*rrunit) and 95\% C.I = exp((beta-1.96*s.e(beta), beta+1.96*s.e(beta))*rrunit)
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


# calculate relative risks
rrcalc(mythresh)
}