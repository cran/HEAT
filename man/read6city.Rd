\name{read6city}
\alias{read6city}

\title{
Read data for six major cities in Korea
}

\description{
\code{read6city} extracts a city-specific data set from \code{mort} data set. 
}
\usage{
read6city(data, code)
}

\arguments{
  \item{data}{
	data can be \code{mort} from \code{data(mort)}.
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


\author{
Youn-Hee Lim, Il-Sang Ohn, and Ho Kim
}


\examples{
data(mort)
seoul = read6city(mort, 11)
}

\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
