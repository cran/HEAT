\name{lagdata}
\alias{lagdata}

\title{
Single and moving average (MVA) lag days
}
\description{
Create single and moving average (MVA) lag days
}
\usage{
lagdata(mydata, varlist, laglength)
}

\arguments{
  \item{mydata}{
	Data includes lagged variables
}
  \item{varlist}{
	Variable name to be lagged
}
  \item{laglength}{
	Number of lag days
}
}

\value{
varname_sxx for single lag days and varname_mxx for moving average lag days
}
\references{
Peng and Dominici. 2010. Statistical Methods for Environmental Epidemiology with R. Springer
}
\author{
Youn-Hee Lim and Ho Kim
}

\examples{

data(mort)
seoul=read6city(mort, 11)
seoul_lag=lagdata(seoul, c("meantemp","mintemp","meanpm10","meanhumi"), 5)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
