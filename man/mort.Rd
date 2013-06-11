\name{mort}
\alias{mort}
\docType{data}
\title{
Mortality data with air pollution and weather data
}
\description{
Korean 6 cities
}
\usage{data(mort)}
\format{
  A data frame with 20454 observations on the following 24 variables.
  \describe{
    \item{\code{ccode}}{city code}
    \item{\code{cname}}{a factor with levels \code{} \code{bs} \code{dg} \code{dj} \code{gj} \code{ic} \code{sl} \code{ul}}
    \item{\code{yy}}{Year}
    \item{\code{mm}}{Month}
    \item{\code{dd}}{Day}
    \item{\code{date}}{Date}
    \item{\code{nonacc}}{Total moratliy excluding accidental mortality}
    \item{\code{cardio}}{Cardiovascular mortality}
    \item{\code{respir}}{Respiratory mortality}
    \item{\code{influenza}}{Influenza epidemics (0,1)}
    \item{\code{meanpm10}}{Daily average PM10 concentrations (ug/m3)}
    \item{\code{meanso2}}{Daily average SO2 concentrations (ppb)}
    \item{\code{meanno2}}{Daily average NO2 concentrations (ppb)}
    \item{\code{meanco}}{Daily average CO concentrations (10ppm)}
    \item{\code{maxco}}{Daily Maximum CO concentrations (10ppm)}
    \item{\code{maxo3}}{Daily Maximum O3 concentrations (ppb)}
    \item{\code{meantemp}}{Daily mean temperature (celcius)}
    \item{\code{maxtemp}}{Daily maximum temperature (celcius)}
    \item{\code{mintemp}}{Daily min temperature (celcius)}
    \item{\code{meanhumi}}{Daily mean relative humidity (percent)}
    \item{\code{meanpress}}{Daily mean air pressure (hPa)}
    \item{\code{season}}{1=spring, 2=summer, 3=fall, 4=winter}
    \item{\code{dow}}{day of week, 1=Sun, ..., 7=Sat}
    \item{\code{sn}}{sequential number of observations}  }
}
\examples{
data(mort)
## maybe str(mort) ; plot(mort) ...
}
\keyword{datasets}
