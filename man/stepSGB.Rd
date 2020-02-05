\name{stepSGB}
\alias{stepSGB}
\alias{stepSGB.default}

\title{
Stepwise backward elimination for SGB regression
}
\description{
Stepwise elimination of the non significant regression parameters. Possibility to assign a fixed value   \code{ shape1} to the overall shape parameter.
}
\usage{
stepSGB(obj0, d, u,  weight = rep(1, dim(d)[1]), shape10 = obj0[["par"]][1],
  bound = 2.1, shape1 = NULL, Mean2 = TRUE, maxiter = 10, 
  control.optim = list(fnscale = -1), 
  control.outer = list(itmax = 1000, ilack.max = 200, trace = TRUE,
   kkt2.check = TRUE, method = "BFGS") )
}

\arguments{
  \item{obj0}{
object of class regSGB, see \code{\link{regSGB}}.
}
  \item{d}{
data matrix of explanatory variables (without constant vector) \eqn{(n \times m)}; \eqn{n}: sample size, 
\eqn{m}: number of auxiliary variables.
}
  \item{u}{
data matrix of compositions (independent variables) \eqn{(n \times D)}; \eqn{D}: number of parts.
}
  \item{weight}{
vector of length \eqn{n}; positive observation weights, default \code{rep(1,n)}. Should be scaled to sum to  \eqn{n}.
}
  \item{shape10}{
positive number, initial value of the overall shape parameter, 
default obj0[["par"]][1].
}
  \item{bound}{
inequality constraints on the estimates of shapes:\cr
 \code{shape1*shape2[i] > bound, i=1,...,D.} \cr
By default \code{bound = 2.1}, see \code{\link{InequalityConstr}}. 
}
  \item{shape1}{
fixed value of the overall shape parameter. Default is NULL (no fixed value).
}
  \item{Mean2}{
logical, if TRUE (default), the initial shape2 parameters are each replaced by their average. See \code{\link{initpar.SGB}}.
}
  \item{maxiter}{
maximum number of iterations, i.e. attempts to set a parameter to 0.
}
  \item{control.optim}{
  list of control parameters for optim, see \code{\link{optim}}. Default is from \code{auglag}, except \code{list(fnscale = -1)}. Always specify \code{fnscale = -1}.
}
  \item{control.outer}{
 list of control parameters to be used by the outer loop in constrOptim.nl, see \code{\link[alabama]{auglag}}. 
Default is from \code{auglag}, except \code{list(itmax = 1000, ilack.max = 200)}.
}
}
\details{
This is an experimental procedure for searching a set of non-significant parameters that will be set to zero. The shape parameters are excluded from the elimination procedure. The algorithm starts with \code{obj0}, output of regSGB.  The p-values for the regression parameters in \code{summary(obj0)} are taken in decreasing order. The parameter with the largest p-value is set to zero and \code{regSGB} computes the regression with this constraint. If the AIC value is smaller than the AIC in \code{obj0}, the parameter with the next largest p-value in \code{obj0} is set to zero and the regression with the two constraints is computed. The process iterates until either a larger AIC is found or \code{maxiter} is attained.\cr
 The initial value of the overall shape parameter is set to the estimated value in the full model \code{obj0}. The other initial values are computed as in \code{\link{regSGB}}. \cr
There is the possibility to fix the value of the overvall shape parameter, if \code{shape1} is given a positive number \eqn{a_0} (default NULL, no fixed value). \cr
If \code{regSGB} was called without \code{Formula}, the data-frame with auxiliary variables for \code{stepSGB} follows the same rules as for the initial regSGB object, see Example 1 in \code{\link{regSGB}}.
}
\value{
A list of class 'stepSGB' with the following 5 components:\cr
  \item{reg }{A list with the following components: \cr
             \code{full} Object of class regSGB, same as \code{obj0}, see \code{\link{regSGB}}. \cr
             \code{iter1} Object of class regSGB obtained at iteration 1. \cr
             ... \cr
             \code{iterk} Object of class regSGB obtained at iteration k.
             } \cr
  \item{Formula }{The original formula, or NULL}
  \item{iter }{Value of k, the last iteration.}
  \item{tab }{Data frame with \code{k+1} columns, overall results and \code{k}  iterations. The rows are \cr
             \code{value} Log-likelihood \cr
             \code{n.par} Total number of parameters (including the shape2 param.) \cr
             \code{n.par.fixed} Number of fixed parameters. \cr
             \code{AIC} Value of the AIC criterion. \cr
             \code{convergence} 0 if converged. \cr
             \code{kkt1} 1 if first Karush-Kuhn-Tucker criterion fulfilled, zero otherwise. \cr
             \code{kkt2} 1 if second Karush-Kuhn-Tucker criterion fulfilled, zero otherwise. \cr 
             \code{counts.function} Number of times the objective function (the log-likelihood) was evaluated. \cr
             \code{counts.gradient} Number of times  the gradient was evaluated. \
}
  \item{call }{Arguments for calling \code{stepSGB}.}
}
\references{
\href{../doc/vignette.pdf}{\code{vignette("SGB regression", package = "SGB")}}
}

\seealso{
 \code{\link{regSGB}}, \code{\link{initpar.SGB}}, \code{\link[alabama]{auglag}}.
}
\examples{
data(carseg)
## Extract the compositions
uc <- as.matrix(carseg[,(1:5)])
## Initial regression
data(ocar)

step_ocar <- stepSGB(ocar, carseg, uc, bound=2.1, control.outer=list(trace=FALSE))

summary(step_ocar[["reg"]][["full"]])
summary(step_ocar[["reg"]][["iter4"]])
step_ocar[["tab"]]

}

\keyword{Regression}
\keyword{Multivariate Techniques}
