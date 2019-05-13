\name{InequalityConstr}
\alias{InequalityConstr}
\alias{hin.SGB}
\alias{hin.SGB.jac}
\title{
Inequality constraints and jacobian
}
\description{
Setting of inequality constraints on shape parameters. \cr
\code{hin.SGB} sets inequality constraints on the shape parameters in a SGB regression.\cr
\code{hin.SGB.jac} defines the corresponding Jacobian.

}
\usage{
hin.SGB(x, d, u, bound, ...)
hin.SGB.jac(x, d, u, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
vector of parameters (\code{shape1}, \code{coefi}, \code{shape2}) where
\code{shape1} is the overall shape, \code{coefi} is the vector of regression coefficients (see \code{\link{initpar.SGB}}) and \code{shape2} the vector of \eqn{D} Dirichlet shape parameters.
}
  \item{d}{
data matrix of explanatory variables (without constant vector) \eqn{(N 	\times m)}; \eqn{N}: sample size, \eqn{m}: number of auxiliary variables.
}
  \item{u}{
data matrix of compositions (independent variables) \eqn{(N \times D)}; \eqn{D}: number of parts.
}
  \item{bound}{
the estimates of shapes are constrained by \code{shape1*shape2[i] > bound, i=1,...,D.} By default \code{bound = 2.1}. 
}
  \item{\dots}{
not used.
}
}
\details{
These functions are invoked internally by \code{regSGB} with \code{bound} specified by the user. \cr
\code{shape1} is constrained to be larger than 0.1, in order to avoid numerical problems and \code{shape2} must be positive. \cr
Moments of ratios of parts only exist up to \code{bound}. Thus \code{bound = 2.1} guarantees the existence of variances of ratios of parts.
}
\value{
\code{hin.SGB} : vector of length \eqn{D+1} with the current value of \code{c(shape1-0.1,shape1*shape2-bound)}. It should be non-negative at convergence of the regression algorithm. \cr
\code{hin.SGB.jac} : corresponding jacobian matrix of dimensions \eqn{(D+1) \times}  \code{length(x)}.
}

\examples{
## Parameter vector for a 3 parts composition with one explanatory variable (+ intercept):
x <- c(1,3.2,0.04,0.05,6,7:9)
bound <- 2.1
u <- t(c(0.1,0.5,0.4))  # only used to compute the number of parts.
hin.SGB(x, d, u, bound)
# = c(shape1-0.1, shape1*shape2-bound,shape2)
# all must be positive.
}


