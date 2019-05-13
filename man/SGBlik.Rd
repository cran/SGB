\name{SGBLik}
\alias{SGBLik}
\alias{fn.SGB}
\alias{gr.SGB}

\title{
SGB log-likelihood and gradient
}
\description{
\code{fn.SGB} gives the log-likelihood and \code{gr.SGB} the gradient vector of the log-likelihood.
}
\usage{
fn.SGB(x, d, u, V,  weight, ...)
gr.SGB(x, d, u, V, weight, ...)
}

\arguments{
  \item{x}{
vector of parameters (\code{shape1}, \code{coefi}, \code{shape2}) where
\code{shape1} is the overall shape, \code{coefi} is the vector of regression coefficients (see \code{\link{initpar.SGB}}) and \code{shape2} the vector of \eqn{D} Dirichlet shape parameters.
}
  \item{d}{
data matrix of explanatory variables (without constant vector) \eqn{(n x m)}; \eqn{n}: sample size, \eqn{m}: number of auxiliary variables
}
  \item{u}{
data matrix of compositions (independent variables) \eqn{(n x D)}; \eqn{D}: number of parts
}
  \item{V}{
full rank transformation of log(parts) into log-ratios, matrix \eqn{(D x (D-1))}
}
  \item{weight}{
vector of length \eqn{n};  positive observation weights, default rep(1,n). Should be scaled to sum to \eqn{n}.
}
  \item{\dots}{
others parameters that might be introduced.
}
}
\details{
The analytical expression for \code{fn.SGB} is found in the vignette "SGB regression", Section 3.2. More details in Graf(2017).
}
\value{
 fn.SGB: value of the log-likelihood at parameter \code{x} \cr
 gr.SGB: gradient vector at parameter \code{x}.
}

\references{
Graf, M. (2017). A distribution on the simplex of the Generalized Beta type. \emph{In J. A. Martin-Fernandez (Ed.), Proceedings CoDaWork 2017}, University of Girona (Spain), 71-90.
}
\seealso{
\code{\link{regSGB}}
}


\examples{
## Explanatory variable
da <- data.frame(l.depth=log(arc[["depth"]]))  
damat <- as.matrix(da)

## Compositions
ua <- as.matrix(arc[,1:3])

## alr transforms
Va <- matrix(c(1,0,-1,0,1,-1),nrow=3)   
colnames(Va) <- c("alr1","alr2")
Va

## Initial values
x <- initpar.SGB(damat,ua,Va)
fn.SGB(x, damat, ua, Va,weight=rep(1,dim(da)[1]))
gr.SGB(x, damat, ua, Va,weight=rep(1,dim(da)[1]))

}

\keyword{Utilities}

