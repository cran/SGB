\name{Tabulation}
\alias{Tabulation}
\alias{table.regSGB}
\alias{coefmat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Tabulation of overall SGB regression results with AIC and matrix view of regression coefficients
}
\description{
table.regSGB: Value of the log-likelihood, number of parameters, AIC criterion, optimality tests and iterations counts. \cr
coefmat: regression coefficients in matrix form with significance level.
}
\usage{
table.regSGB(object)
coefmat(object,digits=3)
}

\arguments{
  \item{object}{
an object of class regSGB
}
   \item{digits}{
 number of decimal places for the coefficients
}
}

\value{ 
\code{table.regSGB}: Data frame with one column, with the overall statistics results. \cr
\item{\code{value}}{the maximum log-likelihood}
\item{\code{n.par}}{the number of parameters}
\item{\code{n.par.fixed}}{the number of fixed parameters}
\item{\code{AIC}}{the AIC criterion}
\item{\code{Rsquare}}{total variance of estimated over total variance of observed compositions}
\item{\code{convergence}}{the convergence code (0: converged, others, see \code{\link[alabama]{auglag}}).}
\item{\code{kkt1}}{the first Karush-Kuhn-Tucker conditions (1=TRUE, 0=FALSE), see \code{\link[alabama]{auglag}}.}
\item{\code{kkt2}}{the second Karush-Kuhn-Tucker conditions (1=TRUE, 0=FALSE), see \code{\link[alabama]{auglag}}.}
\item{\code{counts.function}}{number of times the log-likelihood was evaluated.}
\item{\code{counts.gradient}}{number of times the gradient was evaluated.} \cr

\code{coefmat}: character matrix with the regression coefficients arranged in columns, one for each log-ratio transform. Each ceofficient is followed by the significance level. \cr
}

\seealso{
\code{\link{regSGB}}, \code{\link{oilr}}, \code{\link[alabama]{auglag}}.
}
\examples{
## Overall model statistics
table.regSGB(oilr)
## 
print(coefmat(oilr),quote=FALSE)
## it is a subset of
summary(oilr)

}

\keyword{Regression}
\keyword{Multivariate Techniques}
\keyword{Utilities}

