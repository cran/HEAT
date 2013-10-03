\name{threshpt}
\alias{threshpt}

\title{
Conducting Piecewise Linear Regression
}

\description{
Function \code{threshpt} estimates a optimal threshold point of non-linear relationship using a piecewise linear regression analysis. A user needs to give approximate threshold point range to start, then \code{threshpt} finds optimal threshold point within the given approximate range using grid search method (see details).
}

\usage{
threshpt(formula = formula, family = family, data = data, expvar = expvar, 
startrng = startrng, endrng = endrng, searchunit = searchunit, ...)
}

\arguments{
  \item{formula}{
	An object of class \code{formula}, which is same as the \code{formula} in the \code{glm}.
}
  \item{family}{
	Family of distribution which is same as the \code{family} in the \code{glm} syntax.	
}
  \item{data}{
	Data to be used
}
  \item{expvar}{
	Main exposure variable (X)
}
  \item{startrng}{
	Approximate starting point
}
  \item{endrng}{
	Approximate ending point
}
  \item{searchunit}{
	Unit to search for a threshold point
}
  \item{\dots}{
	Not used
}
}

\details{
\code{threshpt()} estimates a optimal threshold point based on grid search method which finds the point that produces minimum deviance among the equally spaced grid points. This method widely used in environmetal epidemiologic studies.
}

\value{
\code{threshpt} returns a optimal threshold and estimates/etandard errors/p-values for the coefficients of a fitted model with optimal threshold.
}

\references{
Lerman P. Fitting segmented regression models by grid search. Applied Statistics, 23(1):77-84, 1980. \cr
Kim H, Ha JS, and Park J. High temperature, heat index, and mortality in 6 major cities in south korea. Archives of environmental & occupational health, 61(6):265-270, 2006. \cr
Kim SY, Lee JT, Hong YC, Ahn KJ, and Kim H. Determining the threshold effect of ozone on daily mortality: an analysis of ozone and mortality in seoul, korea, 1995-1999. Environmental research, 94(2):113-119, 2004. \cr
Stafoggia M et al. Short-term associations between fine and coarse particulate matter and hospitalizations in Southern Europe: results from the MED-PARTICLES project. Environmental health perspective, 121(8):932-938, 2013.
}

\author{
Youn-Hee Lim, Il-Sang Ohn, and Ho Kim
}

\seealso{
modTempEff
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
mythresh
}
