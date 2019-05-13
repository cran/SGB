\name{SGButil}
\alias{SGButil}
\alias{bval}
\alias{zval}

\title{
Computation of scales and z-vectors
}
\description{
bval computes the scale for each observed composition from the parameters and auxiliary variables for that observation. \cr
zval computes the z-vector for each observed composition, i.e. the transform that is Dirichlet distributed under the  SGB model for the observed composition.
}
\usage{
bval(D, x, d, V)
zval(u, x, d, V)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{D}{ 
number of parts
}
  \item{x}{
vector of parameters (\code{shape1},\code{coefi},\code{shape2}) where
\code{shape1} is the overall shape, \code{coefi} is the vector of regression coefficients (see \code{\link{initpar.SGB}}) and \code{shape2} the vector of \eqn{D} Dirichlet shape parameters
}
  \item{d}{ 
\eqn{(n \times m)} - data matrix of explanatory variables (variables corresponding to \code{coefi}); \eqn{n}: sample size, \eqn{m}: number of auxiliary variables
}
  \item{u}{
\eqn{(n \times D)} - data matrix of compositions (independent variables); \eqn{D}: number of parts
}
  \item{V}{ 
\eqn{D \times (D-1)} - matrix specifying the full rank transformation of log(parts) into log-ratios
}
}

\details{
See Graf (2017), Equation (8), or the vignette "SGB regression", Equation (1).
}
\value{ transformed composition of length \eqn{D}.
}
\references{
Graf, M. (2017). A distribution on the simplex of the Generalized Beta type. \emph{In J. A. Martin-Fernandez (Ed.), Proceedings CoDaWork 2017}, University of Girona (Spain), 71-90.
}

\examples{

## Example with 2 compositions
u <- matrix(c(0.2,0.4,0.5,0.5,0.3,0.2),nrow=2,byrow=TRUE)
u
D <- NCOL(u)  # number of parts

## auxiliary variable
d <- matrix(c(3.2,4.6),ncol=1)

## log-ratio transformation
V <- matrix(c(c(1,-1,0)/sqrt(2),c(1,1,-2)/sqrt(6)),ncol=2)

## vector of parameters:
shape1 <- 2.00
coefi <- c(-0.78,  0.06,  0.96, -0.11)
shape2 <- c(1.80,  3.10,  4.00) 
x <-c(shape1, coefi, shape2)
bval(D,x,d,V)
zval(u,x,d,V)
}

\keyword{Utilities}

