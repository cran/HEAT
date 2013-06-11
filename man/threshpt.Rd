\name{threshpt}
\alias{threshpt}

\title{
Estimation of a threshold point of non-linear relationship.
}
\description{
Estimation of a threshold point of non-linear relationship using a piecewise linear regression analysis. 
A User needs to give approximate threshold point range to start. 
Using Generalized Additive Model, a plot is drawn to visualize the relationship between a response variable and a exposure variable. 
A piecewise linear regression analysis is applied to estimate a threshold point withing the given approximate range.
If plots=T then five type of plots are provided, which are nonlinear plot of response on explanatory variable, time-series plot, deviance plot, estimated values, predicted response value and its confidence interval. 
 time-series plots of response variable and explanatory variable are provided and Deviance~Threshold point are plotted
}
\usage{
threshpt(myformula, expvar, expdf,  nonparm,myfamily,mydata, 
startrng, endrng,searchunit, plots, title0, title1_1, title1_2, 
title2,title3, title4, xlab0, xlab1_1, xlab1_2,xlab2, xlab3, 
xlab4,ylab0,  ylab1_1,ylab1_2, ylab2, ylab3, ylab4)
}
\arguments{
  \item{myformula}{ An object of class "formula", which is same as the "formula" in the "glm" syntax. e.g.) nonacc~meantemp+meanpm10+ns(sn,40)
	
}
  \item{expvar}{
	An Exposure variable (X)
}
  \item{expdf}{
	Degree of freedom of the exposure variable (X) when we plot the response variable on natural cubic spline of explanatory variable . The default value is four.
}
  \item{nonparm}{
	Variable list with assumptin of a non-linear association with a response variable
}
  \item{myfamily}{
	Family of distribution which is same as the "family" in the "glm" syntax.	
}
  \item{mydata}{
	Data to be used
}
  \item{startrng}{
	Approximate starting point
}
  \item{endrng}{
	Approximate ending point
}
  \item{searchunit}{
	Unit to search a threshold point
}
  \item{plots}{
	logical value of plotting 1) plot of response variable on natural cubic spline of explanatory variable  2) timeseries plot, 3) deviance~threshold point, 4) Estimated response value, 5) predicted response value with other mean covariates and 95\% C.I.  
}
  \item{title0}{
	an overall title for the first nonlinear plot 
}
  \item{xlab0}{
	a title for the x axis of the first nonlinear plot
}
  \item{ylab0}{
	a title for the y axis of the first nonlinear plot
}
  \item{title1_1}{
	an overall for the timeseries plot of response variable
}
  \item{xlab1_1}{
	a title for the x axis of the timeseries plot of response variable
}
  \item{ylab1_1}{
	a title for the y axis of the timeseries plot of response variable
}
  \item{title1_2}{
	an overall for the timeseries plot of explanatory variable
}
  \item{xlab1_2}{
	a title for the x axis of the timeseries plot of explanatory variable
}
  \item{ylab1_2}{
	a title for the y axis of the timeseries plot of explanatory variable
}
  \item{title2}{
	an overall title for the deviance plot
}
  \item{xlab2}{
	a title for the x axis of the deviance plot
}
  \item{ylab2}{
	a title for the y axis of the deviance plot
}
  \item{title3}{
	an overall title for the estimated response plot
}
  \item{xlab3}{
	a title for the x axis of the estimated response plot
}
  \item{ylab3}{
	a title for the y axis of the estimated response plot
}
  \item{title4}{
	an overall title for the predicted response plot
}
  \item{xlab4}{
	a title for the x axis of the predicted response plot
}
  \item{ylab4}{
	a title for the y axis of the predicted response plot
}
}

\value{
Estimate/Standard error/P-value for the below and above threshold point.
}
\references{
Sun-Young Kim, Jong-Tae Lee, Yun-Chul Hong, Kyu-jeong Ahn, Ho Kim, 
"Determining the threshold effect of ozone on daily mortality: 
An analysis of ozone and mortality in Seoul, Korea, 1995-1999" Environmental Research 2004;94(2):113-119 
}
\author{
Youn-Hee Lim and Ho Kim
}

\seealso{
modTempEff
}
\examples{

data(mort)
seoul=read6city(mort, 11)
seoul_lag=lagdata(seoul, c("meantemp","mintemp","meanpm10","meanhumi"), 5)

#### thresh pt


myformula=nonacc~meantemp_m3+meanpm10_m4+meanpress+meanhumi+ns(sn, 4*10)+dow ; 
expvar="meantemp_m3"; 
expdf=4; 
nonparm="sn";
myfamily="poisson"; 
mydata=seoul_lag; 

mydata$dow=as.factor(mydata$dow); 
#if you want to add categorical variable to the model, you should factorize 
# the variable and add the variable instead of fac(variable)

startrng=23; 
endrng=30;
searchunit=0.2; 
plots=TRUE; 
title0="spline function of meantemp_m3"; 
title1_1="timeseries plot of nonacc";
title1_2="timeseries plot of meantemp_m3"; 
title2="Deviance plot";
title3="Estimated nonacc vs meantemp_m3";
title4="Predicted nonacc"
xlab0="meantemp_m3";
xlab1_1="time";
xlab1_2="time";
xlab2="threshold";
xlab3="meantemp_m3";
xlab4="meantemp_m3"; 
ylab0="log(E(nonacc))";  
ylab1_1="nonacc";
ylab1_2="meantemp_m3"
ylab2="deviance";
ylab3="nonacc";
ylab4="Predicted E(nonacc)";

mythresh=threshpt(myformula, expvar=expvar, expdf=4,  nonparm=nonparm, 
myfamily=myfamily,mydata=mydata, startrng=startrng, endrng=endrng, 
searchunit=searchunit, plots=plots, title0, title1_1, title1_2, title2, 
title3, title4, xlab0, xlab1_1, xlab1_2,xlab2, xlab3, xlab4, 
ylab0,  ylab1_1,ylab1_2, ylab2, ylab3, ylab4)

}


\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line