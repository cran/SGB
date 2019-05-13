\name{covest.SGB}
\alias{covest.SGB}

\title{
Classical and robust asymptotic covariance matrix
} 
\description{
Computation of two covariance matrices of the estimators of parameters in a SGB regression. The first is based on the Hessian and the second is the sandwich estimator.
}
\usage{
covest.SGB(x, d, u, V, weight=rep(1,dim(d)[1]), x0 = NULL, hessian = NULL, ind = NULL, 
    shape1 = NULL)
}

\arguments{
  \item{x}{
vector of parameters (shape1,coefi,shape2) where
shape1 is the overall shape, coefi is the vector of regression coefficients (see \code{\link{initpar.SGB}}) and shape2 the vector of the \eqn{D} Dirichlet shape parameters; \eqn{D}: number of parts.
shape1 and shape2 must be positive.
}
  \item{d}{
data matrix of explanatory variables (with constant vector if required in the model) \eqn{(n \times m)}; \eqn{n}: sample size, \eqn{m}: number of auxiliary variables.
}
  \item{u}{
data matrix of compositions (variables to be explained) \eqn{n \times D}.
}
  \item{V}{
full rank transformation of log(parts) into log-ratios, matrix \eqn{D \times (D-1)}.
}
  \item{weight}{
vector of length \eqn{n};  positive observation weights, default \code{rep(1,n)}. Should be scaled to sum to \eqn{n}.
}
  \item{x0}{
specification of the initial parameter vector of length \eqn{npar} (optional), default: NULL, no specification.
}
  \item{hessian}{
Hessian matrix (optional), see \code{\link{regSGB}}, default: NULL, no specification. In this case the Hessian is computed numerically.
}
  \item{ind}{
vector of length equal to the number of fixed parameters; specifies the indices of the fixed components in the vector of parameters \eqn{x} (possible for \code{shape1} and \code{coefi} (regression coefficients) only).
}
  \item{shape1}{
fixed value of the overall shape parameter, if \code{heq = heqa.SGB} or 
\code{heq = heqab.SGB}. Default is 1.
}
}
\details{
This function is internally called by regSGB. In this case the Hessian is the output of \code{\link[alabama]{auglag}} and is numerically computed. \cr
A design based covariance matrix of the parameters can be obtained by linearization as the covariance matrix of the \code{scores}.
}
\value{
 a list with
\item{summary }{Data frame with \cr
\code{Initial = x0} (if specified), \cr
\code{Estimate = x}, \cr
\code{StdErr1} = ordinary asymptotic standard error of parameters, \cr
\code{StdErr} = robust asymptotic standard error, \cr
\code{p.value} = asymptotic normal p-value based on \code{StdErr}. For \code{shape1}, \eqn{H_0} is "shape1=shape1", or "shape1=1" if \code{shape1=NULL}. The other parameters are tested against 0. \cr
\code{signif} = significance code based on p.value.
}
\item{scores }{matrix \eqn{n \times npar}. Each row contains the (unweighted) derivatives of the log-density at a data point w.r.t the parameters.}
\item{vcov1}{ordinary asymptotic covariance matrix, inverse of minus the Hessian.}
\item{StdErr1}{vector of ordinary asymptotic standard error of parameters.}
\item{varest2}{robust asymptotic covariance matrix.}
\item{StdErr}{vector of robust asymptotic standard error of parameters.}
}
\references{
Huber, P. J. (1967). The behavior of maximum likelihood estimates under nonstandard conditions. In \emph{Proceedings of the Fifth Berkeley Symposium on Mathematical Statistics and Probability}, Volume 1, pp. 221-233.
}

\seealso{
 \code{\link{regSGB}} for creating \code{oilr}.
}
\examples{
data(arc)
data(oilr)

## compositions
da <- as.matrix(log(arc[["depth"]]),ncol=1)
ua <- as.matrix(arc[,1:3])

## ilr transforms
 c1 <- 1/sqrt(2)
 c2 <- 1/sqrt(6)
 Vilr <- matrix(c(-c1,c1,0,-c2,-c2,2*c2),nrow=3)
 colnames(Vilr) <- c("ilr1","ilr2")
 Vilr
 
 covs <- covest.SGB(oilr[["par"]], da, ua, Vilr)

## Compare the ordinary and robust correlation matrices of parameters estimates.

## (Ordinary) covariance based on inverse Hessian
vcov1 <- covs[["vcov1"]] 
StdErr1 <- covs[["StdErr1"]]
## Estimated correlation matrix
vcor1 <- diag(1/StdErr1) \%*\% vcov1 \%*\% diag(1/StdErr1)
round(vcor1,2)

## Robust (Huber's sandwich estimator):
StdErr <- covs[["StdErr"]]
vcov <- covs[["vcov"]]

## Estimated correlation matrix
round(diag(1/StdErr) \%*\% vcov \%*\% diag(1/StdErr),2)

}

\keyword{Utilities}
