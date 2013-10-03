\name{lagdata}
\alias{lagdata}

\title{
Creating single lagged and moving average variables
}
\description{
Function \code{lagdata} creates single lagged and moving average variables of the lag number that the user designate.
}
\usage{
lagdata(data, varlist, laglength)
}

\arguments{
  \item{data}{
	Data includes lagged variables.
}
  \item{varlist}{
	List of variables to be lagged.
}
  \item{laglength}{
	Number of lag days.
}
}

\details{
Certain exposure on the previous days has an effect on the event on now day. This effect is referred to as the lagged effects. Studies wanting to estimate lagged effects would include the exposure value for previous days in the time series model, and those wanting to estimate cumulative effect of the same day and the previous days would include the moving average value of the exposure.
}

\value{
\code{lagdata} gives single lagged variables (varname_sxx, xx indicates lag length) and moving average variables (varname_mxx).
}

\references{
Dominici F. Time-series analysis of air pollution and mortality: a statistical review. Research report
(Health Effects Institute), (123):3, 2004. \cr
Gasparrini A and Armstrong B. Time series analysis on the health effects of temperature: advancements
and limitations. Environmental research, 110(6):633-638, 2010.
}

\author{
Youn-Hee Lim, Il-Sang Ohn, and Ho Kim
}

\examples{
# read the data
data(mort)
seoul = read6city(mort, 11)

# create lagged and moving average variables
seoul_lag = lagdata(seoul, c("meantemp", "mintemp", "meanpm10", "meanhumi"), 5)
}

