\name{regSGB}
\alias{regSGB}
\alias{regSGB.default}
\alias{regSGB.formula}
\alias{summary.regSGB}
\alias{print.regSGB}

\title{
Regression for compositions following a SGB distribution
}
\description{
Explanatory variables may influence the scale vector through a linear model applied to a log-ratio transform of the compositions. The shape parameters do not depend on explanatory variables. The overall shape parameter \code{shape1} is common to all parts, whereas the Dirichlet shape parameters vector \code{shape2} are specific to each part, i.e. \code{shape2[j]} is the Dirichlet parameter for \code{u[i,j]}, \code{i=1,...,n}, (\code{n}=number of compositions in the dataset \code{u}).
}
\usage{
regSGB(d, ...)

\method{regSGB}{default}(d, u, V, weight=rep(1,dim(d)[1]), 
    shape10 = 1, bound = 2.1, ind = NULL, shape1 = NULL, Mean2 = TRUE, 
    control.optim = list(trace=0,fnscale=-1),
    control.outer = list(itmax=1000,ilack.max=200,trace=TRUE, kkt2.check =TRUE,
    method = "BFGS"),\dots)
       
\method{regSGB}{formula}(Formula, data= list(), weight=rep(1,dim(d)[1]), 
    shape10 = 1, bound = 2.1, ind = NULL, shape1 = 1,  Mean2=TRUE,
    control.optim = list(trace=0,fnscale=-1),
    control.outer = list(itmax=1000,ilack.max=200,trace=TRUE,kkt2.check =TRUE, 
    method = "BFGS"),\dots)
         
\method{print}{regSGB}(x, \dots)

\method{summary}{regSGB}(object, digits=3,\dots)

}

\arguments{
  \item{Formula}{
formula of class Formula, see \code{\link[Formula]{Formula}}.
}
  \item{d}{
data matrix of explanatory variables (without constant vector) \eqn{(n \times m)}; \eqn{n}: sample size, \eqn{m}: number of auxiliary variables.
}
  \item{u}{
data matrix of compositions (independent variables) \eqn{(n \times D)}; \eqn{D}: number of parts.
}
  \item{V}{
log-ratio transformation matrix \eqn{(D \times (D-1))}.
}
\item{data}{ a list with 3 components \code{d}, \code{u} and \code{V}. 
}
  \item{weight}{
vector of length \eqn{n};  positive observation weights, default rep(1,n). Should be scaled to sum to \eqn{n}.
}
  \item{shape10}{
positive number, initial value of the overall shape parameter, default 1.
}
  \item{bound}{
inequality constraints on the estimates of shapes:\cr
 \code{shape1*shape2[i] > bound, i=1,...,D.} \cr
By default \code{bound = 2.1}, see \code{\link{InequalityConstr}}. 
}
  \item{ind}{
vector of length equal to the number of fixed parameters; see \code{index} in \code{\link{EqualityConstr}}. 
Default \code{ind} = NULL (no fixed parameters).
}
  \item{shape1}{
fixed value of the overall shape parameter if \code{min(ind)=1}. Default is 1.
}
  \item{Mean2}{
logical, if TRUE (default), the computed \code{shape2} parameters are each replaced by their average. See \code{\link{initpar.SGB}}.
}
  \item{control.optim}{
  list of control parameters for optim, see \code{\link{optim}}. Default is from \code{\link[alabama]{auglag}}, except \code{list(fnscale = -1)}. Always specify \code{fnscale = -1}.
}
  \item{control.outer}{
 list of control parameters to be used by the outer loop in \code{constrOptim.nl}, see \code{\link[alabama]{auglag}}. 
Default is from \code{\link[alabama]{auglag}}, except \cr
\code{list(itmax = 1000, ilack.max = 200}.
}
   \item{object}{
an object of class "regSGB".
}
  \item{digits}{
number of decimal places for print, default 3.
}
   \item{x}{
an object of class "regSGB".
}
\item{\dots}{ not used. }
}

\details{
It is advisable to use the formula to specify the model for easy comparison between models. 
Without formula, the \code{d} matrix of explanatory variables must contain exactly the variables used in the model, 
whereas with formula other variables can be included as well. 
Variable transformations can be utilized within the formula, see Example 4 below with the indicator \code{I} and the log. \cr 
Constraints on parameters can be introduced, see example 5 and  \code{\link{EqualityConstr}} for more details. \cr
Use \code{ weight} for pseudo-likelihood estimation. \code{ weight} is scaled to \eqn{n}, the sample size. \cr
A design based covariance matrix of the parameters can be obtained by linearization as the covariance matrix of the \code{scores}.
}
\value{
A list of class 'regSGB' with the following components:\cr
The first 13 form the output from \code{\link[alabama]{auglag}}.
 \item{par }{Vector of length \eqn{npar}. Parameters that optimize the nonlinear objective function, satisfying constraints, if convergence is successful.}
 \item{value }{The value of the objective function at termination.}
 \item{counts }{A vector of length 2 denoting the number of times the objective and its gradient were evaluated, respectively.}
 \item{convergence }{An integer code indicating the type of convergence. 0 indicates successful convergence. 
 Positive integer codes indicate failure to converge.}
\item{message }{A character string giving any additional information on convergence returned by \code{\link[stats]{optim}}, or NULL.}
 \item{outer.iteration }{Number of outer iterations.}
 \item{lambda }{Values of the Lagrangian parameter. This is a vector of the same length as the total number of inequalities and equalities. It must be zero for inactive inequalities; non-negative for active inequalities; and can have any sign for equalities.}
 \item{sigma }{Value of augmented penalty parameter for the quadratic term.}
\item{gradient }{Gradient of the augmented Lagrangian function at convergence. It should be small.}
\item{hessian }{Hessian of the augmented Lagrangian function at convergence. It should be negative definite for maximization.}
\item{ineq }{Values of inequality constraints at convergence. All of them must be non-negative.}
\item{equal }{Values of equality constraints at convergence. All of them must be close to zero.}
\item{kkt1 }{A logical variable indicating whether or not the first-order KKT conditions were satisfied (printed 1 if conditions satisfied and 0 otherwise).}
\item{kkt2 }{A logical variable indicating whether or not the second-order KKT conditions were satisfied (printed 1 if conditions satisfied and 0 otherwise).}
\item{scale }{\eqn{n 	\times D} matrix, the estimated scale compositions, see \code{\link{bval}}.}
\item{meanA}{Aitchison expectation at estimated parameters.}
\item{fitted.values }{\eqn{(n 	\times (D-1))} matrix, estimated log-ratio transforms.}
\item{residuals }{Observed minus estimated log-ratio transforms.}
\item{scores }{matrix \eqn{n \times npar}. Each row contains the (unweighted) derivatives of the log-density at a data point w.r.t the parameters.}
\item{Rsquare }{ratio of total variation of \code{meanA} and total variation of compositions \code{u}.}
\item{vcov}{The robust covariance matrix of parameters estimates, see \code{\link{covest.SGB}}.}
\item{StdErr1 }{Ordinary asymptotic standard errors of parameters.}
\item{StdErr }{Robust asymptotic standard errors of parameters.}
\item{fixed.par }{Indices of the fixed parameters.}
\item{summary }{The summary from \code{\link{covest.SGB}}.}
\item{AIC }{AIC criterion.}
\item{V}{log-ratio transformation matrix (same as corresponding input parameter \code{V})}
\item{call }{ Arguments for calling \code{regSGB}.}
\item{Formula }{Expression for formula.}
}
\references{
Graf, M. (2017). A distribution on the simplex of the Generalized Beta type. \emph{In J. A. Martin-Fernandez (Ed.), Proceedings CoDaWork 2017}, University of Girona (Spain), 71-90.

Hijazi, R. H. and R. W. Jernigan (2009). Modelling compositional data using Dirichlet regression models.
\emph{Journal of Applied Probability and Statistics}, \strong{4} (1), 77-91.

Kotz, S., N. Balakrishnan, and N. L. Johnson (2000). \emph{Continuous Multivariate Distributions}, Volume 1, Models and Applications. John Wiley & Sons.

Madsen, K., H. Nielsen, and O. Tingleff (2004). Optimization With Constraints. \emph{Informatics and Mathematical Modelling}, Technical University of Denmark.

Monti, G. S., G. Mateu-Figueras, and V. Pawlowsky-Glahn (2011). Notes on the scaled Dirichlet distribution. In \emph{V. Pawlowsky-Glahn and A. Buccianti (Eds.), Compositional data analysis. Theory and applications.} Wiley.

Varadhan, R. (2015). alabama: Constrained Nonlinear Optimization. R package version 2015.3-1.

Wicker, N., J. Muller, R. K. R. Kalathur, and O. Poch (2008). A maximum likelihood approximation
method for Dirichlet parameter estimation. Computational Statistics & Data Analysis 52 (3), 1315-1322.

Zeileis, A. and Y. Croissant (2010). Extended model formulas in R: Multiple parts and multiple responses. \emph{Journal of Statistical Software} \strong{34} (1), 1-13.
}


\seealso{
\code{\link{stepSGB}}, for an experimental stepwise descending regression, \code{\link{initpar.SGB}}, for the computation of initial parameters.
This function uses \code{\link[Formula]{Formula}}, \code{\link[alabama]{auglag}}.
}
\examples{
## Regression for car segment shares
## ---------------------------------
data(carseg)
## Extract the compositions
uc <- as.matrix(carseg[,(1:5)])

## Extract the explanatory variables
attach(carseg)

## Example 1: without formula
## --------------------------
## Change some variables
dc <- data.frame(l.exp1=log(expend)*PAC,l.exp0=log(expend)*(1-PAC), l.sent=log(sent),
l.FBCF=log(FBCF), l.price=log(price), rates)

## Define the log-ratio transformation matrix
Vc <- matrix(c( 1,0,0,0,
               -1,1,0,0,
               0,-1,1,0,
               0,0,-1,1,
               0,0,0,-1),ncol=4,byrow=TRUE)
colnames(Vc) <- c("AB","BC","CD","DE")
rownames(Vc) <- colnames(uc)
Vc

# 2 next rows  only necessary when calling regSGB without a formula.
dc1 <- cbind("(Intercept)"= 1 , dc)
dc1 <- as.matrix(dc1)   

object10 <- regSGB(dc1,uc, Vc,shape10=4.4)
summary(object10)

## Example 2: same with formula
## ----------------------------
## Define the formula
Form <- Formula(AB | BC | CD | DE ~  l.exp1 + l.exp0 + l.sent + l.FBCF + l.price +  rates)

## Regression with formula
object1 <- regSGB(Form, data= list(dc, uc, Vc),shape10=4.4)

summary(object1)

## Example 3: Usage of I()
## -----------------------
Form2 <- Formula(AB | BC | CD | DE ~  I(l.exp1 + l.exp0) + l.exp1 +l.sent + 
                 l.FBCF + l.price + rates )
object2 <- regSGB(Form2,data= list(dc, uc, Vc),shape10=4.4)
object2

## Example 4: Usage of variable transformations on the original file
## -----------------------------------------------------------------
Form3 <- Formula(AB | BC | CD | DE ~  log(expend) + I(PAC*log(expend)) + log(sent) + log(FBCF) + 
                 log(price) + rates)
object3 <- regSGB(Form3, data=list(carseg, uc, Vc),shape10=4.4)
object3
object2[["par"]]-object3[["par"]]    # same results

## Example 5: Fixing parameter values
## ----------------------------------
## 1. In the following regression we condition on shape1 = 2.36.
object4 <- regSGB(Form3,data=list(carseg, uc, Vc), 
                  shape10 = 4.4,  bound = 2.0, ind = 1, shape1 = 2.36)
summary(object4)

## 2. In the following regression we condition on shape1 = 2.36 and the  coefficient of 
## log(FBCF).BC = 0.  Notice that it is the 19th parameter.
object5 <- regSGB(Form3,data=list(carseg, uc, Vc),
                  shape10 = 4.4, bound = 2.0, ind = c(1,19) , shape1 = 2.36)
summary(object5)

object3[["AIC"]]
object4[["AIC"]]  # largest AIC
object5[["AIC"]]
}

\keyword{Regression}
\keyword{Multivariate Techniques}
