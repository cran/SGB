\name{GenGammaDistrib}
\alias{GenGammaDistrib}
\alias{dggamma}
\alias{rggamma}
\title{Generalized Gamma distribution
}
\description{
Density and random generation of the generalized gamma distribution.
}
\usage{
dggamma(x, shape1, scale, shape2)
rggamma(n, shape1, scale, shape2)
}
\arguments{
  \item{x}{
vector of positive values
}
  \item{n}{
number of simulated vectors
}
  \item{shape1}{
overall shape parameter
}
  \item{scale}{
vector of scales. Should be of the same length as \code{x}
}
  \item{shape2}{
vector of Dirichlet parameters. Should be of the same length as \code{x}.
}
}
\details{
log density at \eqn{x >0}: \cr
\eqn{log(shape1/scale) - lgamma(shape2) + (shape1*shape2-1)*log(x/scale) - (x/scale)^{shape1}}
}
\value{
dggamma: Generalized gamma density evaluated at  x 

rggamma: Generalized gamma random deviates
}
\references{
Stacy, E.W. (1962). "A Generalization of the Gamma Distribution." \emph{Annals of Mathematical Statistics} \strong{33}(3): 1187-1192.\cr \cr
Johnson, N.L.; Kotz, S; Balakrishnan, N. (1994) \emph{Continuous Univariate Distributions}, Volume 1, 2nd Edition. Wiley. ISBN 0-471-58495-9 (Section 17.8.7)
}

\examples{
set.seed(12345)
u1 <- rggamma(10,2,1,1.4) # 10 random deviates with scale 1
set.seed(12345)
u <- rggamma(10,2,1:10,1.4) # 10 random deviates with scale 1:10, repectively
u
u/u1
dggamma(u,2,1:10,1.4)
}

\keyword{Probability Distributions and Random Numbers}
