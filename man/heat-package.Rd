\name{HEAT-package}
\alias{HEAT-package}
\alias{HEAT}
\docType{package}
\title{
Health Effects of Air pollution and Temperature (HEAT)
}
\description{
Package provides Korea data of mortality and environment variables for time-series analysis. 
The package also includes several functions to read specific city information, generate single and moving average lag days, and 
estimate threshold points in a nonlinear relationship.
}
\details{
\tabular{ll}{
Package: \tab HEAT\cr
Type: \tab Package\cr
Version: \tab 1.1\cr
Date: \tab 2013-07-08\cr
License: \tab GPL-2\cr
LazyLoad: \tab yes\cr
}
The package can be used to anlayze Korea mortality and environment data. 
Function 'read6city' to load a single city's data
Function 'lagdata' to generate exposure variables at single and moving average lag days
Function 'threshpt' to estimate threshold point of a nonlinear relationship (e.g., U-, V-, or J-shape)\cr
 \cr
The package was supported by the Basic Science Research Program (#2010-0009581), International Research & Development Program (#2012K1A3A1A12054839),  Women Scientist Research Program (#2012R1A1A3005549) and Global Research Lab (#K21004000001-10A0500-00710) through the National Research Foundation of Korea (NRF) funded by the Ministry of Education, Science, and Technology
}
\author{
Youn-Hee Lim and Ho Kim

Maintainer: Il-Sang Ohn <byeolbaragi@gmail.com>

}
\references{
Lim YH, Kim H, "Manual for HEAT package", 2012
}
\keyword{ package }
\seealso{
read6city, lagdata, threshpt
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

summ.threshpt(mythresh)

rrcalc(mythresh)

}
