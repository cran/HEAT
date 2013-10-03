\name{plot.threshpt}

\alias{plot.threshpt}

\title{
Plots from a threshpt Object
}

\description{
Following four plots from a fitted \code{threshpt} object are provided: \cr
Plot 1: plot for a response variable on the natural cubic spline of a exposure variable \cr
Plot 2: plot for fitted response values with other mean covariates and 95\% confidence intervals \cr
Plot 3: plot for fitted response values \cr
Plot 4: deviance by threshold point plot \cr
}

\usage{
\method{plot}{threshpt}(x, select = NULL, se = T, expdf = 4, 
xlim = NULL, ylim = NULL, xaxt = NULL, yaxt = NULL, 
col.value = NULL, col.preval = NULL, col.ci = NULL, col.vline = NULL,
lwd = NULL, pch = NULL, pch.preval = NULL, main = NULL, xlab = NULL, ylab = NULL, ...)
}

\arguments{
  \item{x}{
A fitted \code{threshpt} object produced by \code{threshpt()}
}
  \item{select}{
Select a plot type
}
  \item{se}{
When TRUE (default) upper and lower confidence interval lines are added to the plot 1 and 4. When FALSE, otherwise.
}
  \item{expdf}{
Degree of freedom of natural cubic spline function for the main exposure variable in plot 1; default value is four.
}
  \item{xlim}{
The x limits of the plot.
}
  \item{ylim}{
The y limits of the plot.
}
  \item{xaxt}{
A character which specifies the x axis type. 
}
  \item{yaxt}{
A character which specifies the y axis type. 
}
  \item{col.value}{
The color of main values or line in all kind of plot.
}
  \item{col.preval}{
The color of fitted value in plot 3.
}
  \item{col.ci}{
The color of confidence interval lines in plot 1 and 3.
}
  \item{col.vline}{
The color of vertical line that represents a optimum threshold in plot 2, 3 and 4.
}
  \item{lwd}{
Line width for plot 1
}
  \item{pch}{
Either an integer specifying a symbol or a single character to be used in plotting points in plot 2 and 3. 
}
  \item{pch.preval}{
Either an integer specifying a symbol or a single character to be used in plotting fitted values in plot 3. 
}
  \item{main}{
Overall title of the plot.
}
  \item{xlab}{
A title for the x axis of the plot
}
  \item{ylab}{
A title for the y axis of the plot
}
  \item{\dots}{
Not used.
}
}
\value{
Generated four kind of plots from a fitted \code{threshpt} object
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

# obtain plots
plot(mythresh, select = 1, se = TRUE, expdf=8, col.value = "blue", col.ci = "light blue")
plot(mythresh, select = 2, se = FALSE, col.vline = "orange")
plot(mythresh, select = 3, pch = 1, pch.preval = 2)
plot(mythresh, select = 4)
}