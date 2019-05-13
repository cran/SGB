\name{summaryA.SGB}
\alias{summaryA.SGB}
\alias{MeanA.SGB}
\alias{ModeA.SGB}
\alias{MeanAobj.SGB}
\alias{ModeAobj.SGB}

\title{
Aitchison expectation and mode under the SGB distribution
}
\description{
The expectation and mode in the log-ratio space, transformed back to the simplex.
}
\usage{
MeanA.SGB(shape1, scale, shape2)
ModeA.SGB(shape1, scale, shape2)
MeanAobj.SGB(obj)
ModeAobj.SGB(obj)
}

\arguments{
  \item{shape1}{
 overall shape parameter. \code{shape1} = 1 for a Dirichlet composition.
}
  \item{scale}{
 vector of length \eqn{D} or matrix with \eqn{D} columns containing the scales of parts. 
}
  \item{shape2}{
  vector of length \eqn{D} containing the (Dirichlet) shapes for each part.
}
  \item{obj}{
list, result of regSGB. See \code{\link{regSGB}}.
}
}
\details{
\code{MeanA}, \code{ModeA} compute Aitchison expectation and mode in function of the SGB distribution parameters, whereas \code{MeanAobj}, \code{ModeAobj} compute Aitchison expectation and mode in function of the model variables in an SGB regression object.
}
\value{
 A matrix or vector of  dimensions \eqn{(n \times D)}. 
 Each row gives the Aitchison expectation for compositions having the corresponding set of auxiliary variables.
  
}
\references{
Aitchison, J. (1986). \emph{The Statistical Analysis of Compositional Data}. Monographs on Statistics and Applied Probability. Chapman and Hall Ltd (reprinted 2003 with additional material by the Blackburn Press, London (UK).
}

\seealso{
\code{\link{oilr}}.
}

\examples{
   set.seed(1234)
   x <- c(2,rnorm(4,0,1),1.8,3.1,4.0)
   d <- c(3.2,4.6)
   V <- t(matrix(c(1/sqrt(2),-1/sqrt(2),0,
                 1/sqrt(6),1/sqrt(6),-2/sqrt(6)),
                 nrow=2,byrow=TRUE))
   D <- 3
   shape1 <- x[1]
   scale <- bval(D,x,d,V)
   shape2 <- x[(length(x)-D+1):length(x)]
# Expectation
   MeanA.SGB(shape1,scale,shape2)
# Mode
   ModeA.SGB(shape1, scale, shape2)

## Arctic lake data
# oilr is a SGB regression object
data(oilr) 
MeanAobj.SGB(oilr)   # is the same as oilr[["meanA"]]
ModeAobj.SGB(oilr)
}

\keyword{Utilities}
