\name{read6city}
\alias{read6city}

\title{
read6city
}
\description{
Read 6 major cities in Korea
}
\usage{
read6city(mydata, code)
}

\arguments{
  \item{mydata}{
	mydata can be 'mort' from data(mort)
	}
  \item{code}{
	code must be one of the following:
	  11 for Seoul,
	  23 for Incheon
	  25 for Daejeon
	  22 for Daegu
	  21 for Busan
	  24 for Gwangju
}
}
\details{
	Extract a city from data
}


\author{
Youn-Hee Lim and Ho Kim
}


\examples{
data(mort)
seoul=read6city(mort, 11)

}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
