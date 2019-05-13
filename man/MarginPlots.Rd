\name{MarginPlots}
\alias{MarginPlots}
\alias{hzbeta}
\alias{qzbeta}
\alias{pzbeta}

\title{ 
Histograms, quantile and probability plots for the z(u)-transforms of parts
}
\description{
These functions draw a plot for each part in the dataset.
}
\usage{
hzbeta(u, obj, weight = rep(1,dim(u)[1]) )
qzbeta(u, obj, weight = rep(1,dim(u)[1]) )
pzbeta(u, obj, weight = rep(1,dim(u)[1]) )
}

\arguments{
  \item{u}{
data matrix of compositions (independent variables) \eqn{(N \times D)}; \eqn{D}: number of parts
  }
  \item{obj}{
list, result of regSGB. See \code{\link{regSGB}}.
  }
  \item{weight}{
vector of length \eqn{n};  positive observation weights, default rep(1,n). 
  }
}

\details{
Let \eqn{U} follow a \eqn{SGB(shape1,scale,shape2)} distribution. Then the composition 
\deqn{Z=C[(U/scale)^{shape1}]} 
is called the \eqn{z(u)}-transform of \eqn{U}.\cr
 \eqn{Z} follows a \eqn{Dirichlet(shape2)} distribution and each part \eqn{Z_i, i=1,...,D} is Beta-distributed with parameters \code{(shape2[i],sum(shape2)-shape2[i])}.
\cr
Goodness of fit plots are produced for the parts of the \eqn{z(u)}-transforms against the Beta distribution. Each function creates \eqn{D} plots, where \eqn{D} is the number of parts. \cr
\code{hzbeta}: histograms and the corresponding Beta-densities,\cr 
\code{qzbeta}: marginal quantile plots, \cr 
\code{pzbeta}: marginal probability plots.\cr
If \code{weight}  is specified, weighted histgrams, quantile and probability plots are drawn. 
}
\value{
\eqn{D} plots are produced comparing the marginal distribution of the parts of the \eqn{z(u)} compositions with the theoretical Beta distribution. 
}

\examples{
## Arctic lake data
data(arc)
# Compositions
ua <- arc[,1:3]

# SGB regression
data(oilr)

# plot
par(mfrow=c(3,3))
hzbeta(ua,oilr)
qzbeta(ua,oilr)
pzbeta(ua,oilr)
}

\keyword{High-Level Plots }

