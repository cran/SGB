\name{EqualityConstr}
\alias{EqualityConstr}
\alias{heqa.SGB}
\alias{heqa.SGB.jac}
\alias{heqb.SGB}
\alias{heqb.SGB.jac}
\alias{heqab.SGB}
\alias{heqab.SGB.jac}

\title{
Equality constraints for overall shape and/or regression parameters and jacobian
}
\description{
Setting of equality constraints on parameters. \cr
\code{heqa.SGB} sets the overall shape parameter to \code{shape1}. \cr
\code{heqb.SGB} sets specified regression parameters to 0.  \cr
\code{heqab.SGB} is a combination of both.  \cr
\code{heqa.SGB.jac}, \code{heqb.SGB.jac}, \code{heqab.SGB.jac} compute the corresponding Jacobians.
}
\usage{
heqa.SGB(x, d, u, bound, shape1, ...)
heqa.SGB.jac(x, ...)
heqb.SGB(x, d, u, bound, shape1, index, ...)
heqb.SGB.jac(x, d, u, bound, shape1, index, ...)
heqab.SGB(x, d, u, bound, shape1, index, ...)
heqab.SGB.jac(x, d, u, bound, shape1, index, ...)
}

\arguments{
  \item{x}{
current vector of parameters (\code{shape1}, \code{coefi}, \code{shape2}) where
\code{shape1} is the overall shape, \code{coefi} is the vector of regression coefficients (see \code{\link{initpar.SGB}}) and \code{shape2} the vector of \eqn{D} Dirichlet shape parameters.
}
  \item{d}{
data matrix of explanatory variables (without constant vector) \eqn{(N \times m)}; \eqn{N}: sample size, \eqn{m}: number of explanatory variables.
}
  \item{u}{
data matrix of compositions (independent variables) \eqn{(N \times D)}; \eqn{D}: number of parts.
}
  \item{bound}{
not used.
}
  \item{shape1}{
chosen fixed value of the overall shape parameter. \cr 
Default \code{shape1} = 1 for \code{heqa.SGB} and \code{heqab.SGB}. \cr
\code{shape1} is not fixed in \code{heqb.SGB}.
}
\item{index}{
vector of length equal to the number of fixed parameters; specifies the indices of the fixed components in the vector of parameters \code{x}, such that for \cr 
\code{heqa.SGB, heqa.SGB.jac}: \code{index}=1. The fixed value of the overall shape parameter is \code{shape1} (by default 1). \cr 
\code{heqb.SGB, heqb.SGB.jac}: \code{index}= c(...) with ... the indices of regression parameters to be set to 0. \cr 
\code{heqab.SGB, heqab.SGB.jac}: \code{index}=c(1,...); \code{shape1} is the fixed value of the overall shape parameter, and ... the indices of the regression parameters to be set to 0. 
}
  \item{\dots}{
not used.
}
}
\details{
These functions are invoked by \code{\link{regSGB}} through the specification of the function name, \code{shape1} and/or \code{index}.
}
\value{
\code{heqa.SGB}, \code{heqb.SGB}, \code{heqab.SGB}: vector of the same length as \code{index} specifying the current value of \code{x[index]} or \code{x[1]-shape1}, where \code{x} is the current vector of parameters. It should be near zero at convergence of the regression algorithm. \cr
\code{heqa.SGB.jac}, \code{ heqb.SGB.jac}, \code{ heqab.SGB.jac}: the corresponding jacobian matrices of dimensions \eqn{length(index) \times length(x)}.
}
\seealso{
\code{\link{regSGB}}, \code{\link{summary.regSGB}}
}
\examples{
## parameter vector for a 3 parts composition with one explanatory variable (+ intercept):
x <- c(1,3.2,0.04,0.05,6,7:9)

## shape1 fixed to 1.5:
heqa.SGB(x,d,u,bound,1.5)
heqa.SGB.jac(x)

## Parameters 3 (first slope) and 4 (second intercept) fixed to 0:
heqb.SGB(x,d,u,bound,shape1,c(3,4))
heqb.SGB.jac(x,d,u,bound,shape1,c(3,4))

## Parameters 1, 3, 4 fixed to 1.5, 0, 0 respectively:
heqab.SGB(x,d,u,bound,1.5,c(1,3,4))
heqab.SGB.jac(x,d,u,bound,1.5,c(1,3,4))
}

\keyword{Utilities }
