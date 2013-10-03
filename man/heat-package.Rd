\name{HEAT-package}
\alias{HEAT-package}
\alias{HEAT}
\docType{package}

\title{
Health Effects of Air pollution and Temperature (HEAT)
}

\description{
Package HEAT provides Korea data of mortality and environment variables for time-series analysis. The package includes several functions to read specific city information, generate single and moving average lag days, and estimate a threshold point in a nonlinear relationship.
}

\details{
\tabular{ll}{
Package: \tab HEAT\cr
Type: \tab Package\cr
Version: \tab 1.2\cr
Date: \tab 2013-10-03\cr
License: \tab GPL-2\cr
LazyLoad: \tab yes\cr
}
The package can be used to analyze Korea mortality and environment data, providing following functions: 
function \code{read6city} to load a single city's data,
function \code{lagdata} to generate exposure variables at single and moving average lag days,
function \code{threshpt} to estimate threshold point of a nonlinear relationship (e.g., U-, V-, or J-shape),
function \code{summary.threshpt} to give summary informations for a fitted threshpt object,
function \code{plot.threshpt} to produce some informative plots, and
function \code{rrcalc} to calculate relative risks and their 95\% confidence intervals below and above a threshold. \cr
 \cr
The package was supported by the Basic Science Research Program (#2010-0009581), International Research & Development Program (#2012K1A3A1A12054839),  Women Scientist Research Program (#2012R1A1A3005549) and Global Research Lab (#K21004000001-10A0500-00710) through the National Research Foundation of Korea (NRF) funded by the Korea Ministry of Science, ICT (Information and Communication Technologies) and Future Planning.
}

\author{
Youn-Hee Lim, Il-Sang Ohn, and Ho Kim

Maintainer: Il-Sang Ohn <byeolbaragi@gmail.com>
}

\keyword{ package }
\seealso{
read6city, lagdata, threshpt
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