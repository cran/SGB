\name{arc}
\alias{arc}
\docType{data}
\title{
  arc dataset
}
\description{
39 (sand,silt,clay) compositions in an Arctic lake in function of depth.
}
\usage{data("arc")}
\format{
  A data frame with 39 observations on the following 4 variables.
  \describe{
    \item{sand}{	sand part
    }
    \item{silt}{	silt part
    }
    \item{clay}{	clay part
    }
    \item{depth}{	depth (m)
    }
  }
}

\source{
Aitchison, J. (1986). \emph{The Statistical Analysis of Compositional Data.}.  Monographs on Statistics
and Applied Probability. Chapman and Hall Ltd (reprinted 2003 with additional material by
the Blackburn Press, London (UK).}
\references{
Coakley, J.P. and Rust, B.R. (1968). Sedimentation in an Arctic lake. \emph{J. Sed. Petrology}, \bold{38}, 1290-1300.
}
\examples{
data(arc)
str(arc) 
}
\keyword{datasets}
