\name{summ.threshpt}
\alias{summ.threshpt}
\title{
Summary for a Threshpt fit
}
\description{
Threshpt object produced by threshpt() are summarized
}
\usage{
summ.threshpt(obj)
}

\arguments{
  \item{obj}{
a fitted threshpt object as produced by threshpt()
}
}
\details{
formula, deviance, estimated threshold value, estimated coefficitents are provided.
}
\value{

  \item{Formula }{the formula which is used in the threshpt function}
  \item{Deviance }{deviance of fitted model}
  \item{Threshold }{threshold value of the model with the minimum deviance}
  \item{Best fit }{estimated parameter coefficients of model with the minimum deviance}

}
\references{
Sun-Young Kim, Jong-Tae Lee, Yun-Chul Hong, Kyu-jeong Ahn, Ho Kim, 
"Determining the threshold effect of ozone on daily mortality: 
An analysis of ozone and mortality in Seoul, Korea, 1995-1999" Environmental Research 2004;94(2):113-119 
}
\author{
Youn-Hee Lim and Ho Kim
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

}

\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
