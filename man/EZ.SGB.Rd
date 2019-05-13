\name{EZ.SGB}
\alias{EZ.SGB}

\title{
Expectations of Z under the SGB distribution
}
\description{
Expectations under Lebesgue and Aitchison measures for the transformed composition \eqn{Z= C((U/scale)^{shape1})} and \eqn{C(Z^{1/shape1})}, where \eqn{C(.)} is the closure operation.
}
\usage{
EZ.SGB(D, x)
}

\arguments{
  \item{x}{
vector of parameters (\code{shape1},\code{coefi},\code{shape2}) where
\code{shape1} is the overall shape, \code{coefi} is the vector of regression coefficients (see \code{\link{initpar.SGB}}) and \code{shape2} the vector of \eqn{D} Dirichlet shape parameters
}
  \item{D}{
    number of parts
}
}

\value{
 A matrix with 4 rows and D columns giving on each row the expectation of parts
   \item{EZ}{
   \eqn{E(Z)}{E(Z)}, expectation under the (ordinary) Lebesgue measure,
}
   \item{EAZ}{
   \eqn{E_A(Z)}{E_A(Z)}, expectation under the Aitchison measure,
}
   \item{EZa}{
   \eqn{E(Z^{1/shape1})}, expectation under the (ordinary) Lebesgue measure,
}
   \item{EAZa}{
   \eqn{E_A(Z^{1/shape1})}, expectation under the Aitchison measure.
}
}


\seealso{
\code{\link{zval}}
}
\examples{
set.seed(1234)
x <- c(2,rnorm(4,0,1),1.8,3.1,4.0) 
D <- 3
EZ.SGB(D,x)

}

\keyword{Utilities}

