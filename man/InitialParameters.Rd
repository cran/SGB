\name{InitialParameters}
\alias{InitialParameters}
\alias{initpar.SGB}
\alias{compushape2}
\alias{condshape2}

\title{
Initial parameters estimates and comparison
}
\description{
\code{initpar.SGB} computes an initial vector of parameters. \cr
\code{condshape2} computes the \code{shape2} parameters by the same method as \code{initpar.SGB}, but from an arbitrary set of parameters (\code{shape1},\code{coefi})  (e.g. the result of a SGB regression fit). These approximations are compared with the \code{shape2} estimates. \cr
\code{compushape2} is internally called by \code{initpar.SGB} and \code{condshape2}. It computes \code{shape2} parameters in function of \code{shape1} and given regression parameters \code{coefi}.
}
\usage{
initpar.SGB(d, u, V, weight = rep(1, dim(u)[1]), shape1 = 1, Mean2 = TRUE) 
condshape2(x,d,u,V)
compushape2(shape1, coefi, d, u, V)
}
%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{d}{
data matrix of explanatory variables (without constant vector) \eqn{(n \times m)};  \eqn{n}: sample size, \eqn{m}: number of auxiliary variables
}
  \item{u}{
data matrix of compositions (independent variables) \eqn{(n \times D)}; \eqn{D}: number of parts
}
  \item{V}{
full rank transformation of log(parts) into log-ratios, matrix \eqn{D \times (D-1)}
}
  \item{weight}{
vector of length \eqn{n};  positive observation weights, default \code{rep(1,n)}. Should be scaled to sum to \eqn{n}.
}
  \item{shape1}{
positive number, overall shape parameter
}
  \item{Mean2}{
logical, if TRUE (default), the computed \code{shape2} parameters are each replaced by their average.
}
  \item{coefi}{
vector of regression coefficients of length \eqn{(m+1)*(D-1)}, resp.  \eqn{D-1} constants, then \eqn{D-1} coef. of the 1st expl. variable,..., \eqn{D-1} coef. of the \eqn{m}-th expl. variable
}
  \item{x}{
  fitted SGB regression parameters, see \code{\link{regSGB}}.
}
}

\details{
The main function here is \code{initpar.SGB}. The initial value of \code{shape1} must be specified by the user; by default, it takes the value 1.
In the initial regression model, each column of 
 \code{log(u) \% * \% V}  
is regressed by OLS on the columns of \code{d}. \code{coefi} is the vector of regression parameters, first the \eqn{D-1} terms associated with the first explanatory variable in \code{d}, and so on similarily for each explanatory variable. The initial scale compositions are computed by back-transforming the predicted values to the simplex and used to compute the vector \eqn{ z=C[(u/scale)^{shape1}]}, where \eqn{C[.]} is the closure operation. Wicker et al. (2008), see also Ng et al. (2011) p.74-75, describe a procedure to find initial values for the shape parameters in a Dirichlet distribution. Their method is used on the (approximate) Dirichlet vector \eqn{ z}. \cr

}

\value{
\code{initpar.SGB}:  \cr
vector of length \eqn{(1+ (D-1)*(m+1) + D)} containing initial  values for (\code{shape1},\code{coefi},\code{shape2}). \cr
\code{condshape2}: \cr
list with two components: 1. title and 2. data-frame with 2 columns: fitted \code{shape2} and Wicker's  approximation.
}

\references{
Wicker, N., J. Muller, R. K. R. Kalathur, and O. Poch (2008). A maximum likelihood approximation method for Dirichlet's parameter estimation. \emph{Computational Statistics & Data Analysis} \bold{52} (3), 1315-1322.

Kai Wang Ng, Guo-Liang Tian, Man-Lai Tang (2011). \emph{Dirichlet and Related Distributions: Theory, Methods and Applications}. Wiley Series in Probability and Statistics.
}

\examples{
## Explanatory variable
da <- data.frame(l.depth=log(arc[["depth"]]))  
damat <- as.matrix(da)
## Compositions
ua <- arc[,1:3]

## alr transforms
Va <- matrix(c(1,0,-1,0,1,-1),nrow=3)   
colnames(Va) <- c("alr1","alr2")
Va

## Initial values
initpar.SGB(damat,ua,Va)
initpar.SGB(damat,ua,Va,Mean2=FALSE)

## Conditional shape2 values; same as parameters computed with initpar
condshape2(initpar.SGB(damat,ua,Va,Mean2=FALSE),damat,ua,Va)

## Comparison with fitted parameters
oa <- regSGB(damat, as.matrix(ua), Va)
condshape2(oa[["par"]],damat,ua,Va)

}

\keyword{Utilities} 
