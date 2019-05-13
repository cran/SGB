\name{carseg}
\alias{carseg}
\docType{data}

\title{
carseg dataset
}
\description{
Segment shares of car sales in five categories according to the size of the car chassis, with explanatory variables.
}
\usage{data("carseg")}
\format{
  A data frame with 152 observations on the following 13 variables.
  \describe{
    \item{SA}{		Segment share in category A
    }
    \item{SB}{		Segment share in category B
    }
    \item{SC}{		Segment share in category C
    }
    \item{SD}{		Segment share in category D
    }
    \item{SE}{		Segment share in category E
    }
    \item{expend}{	quarterly household expenditures
    }
    \item{sent}{	monthly confidence indicator made up of several branches
    }
    \item{FBCF}{	monthly households investment
    }
    \item{PAC}{		binary vector indicating the incentive period
    }
    \item{PIB}{		Gross domestic product
    }
    \item{price}{	gas oil price
    }
    \item{rates}{	monthly short term interest rates
    }
    \item{month}{	sequential month number (1 to 150)
    }
  }
}
\details{
This dataset consists of simulated monthly segment market shares (SA to SE) corresponding to the 5 segments of a certain brand during 150 consecutive months (01/2003 to 08/2015). The set of explanatory variables was selected by Morais and Thomas-Agnan (2019) as being the most meaningful to explain the segment shares. Names have been simplified.
}

\references{
Morais, J. and Thomas-Agnan, C. (2019), Impact of economic context on automobile market segment shares: a compositional approach, \emph{Case Studies in Business, Industry and Government Statistics}, in press. 
}
\examples{
data(carseg)
summary(carseg[,(6:12)])
}
\keyword{datasets}
