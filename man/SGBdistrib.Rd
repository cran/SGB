\name{SGBdistrib}
\alias{SGBdistrib}
\alias{dSGB}
\alias{rSGB}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Density and random generator for the SGB distribution
}
\description{
\code{dSGB} computes the density for a given argument \code{u} (a composition) and given parameters. \cr
\code{rSGB} generates \eqn{n} compositions for given parameters.
}

\usage{
dSGB(u, shape1, scale, shape2) 
rSGB(n, shape1, scale, shape2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{u}{
vector of length \eqn{D}{D} containing the composition
}
  \item{shape1}{
overall shape parameter. 
shape1 = 1 for a Dirichlet composition.
}
  \item{scale}{
 vector of the same length as u containing the scales of parts. 
 If missing, scales are set to 1.
}
  \item{shape2}{
  vector of length \eqn{D}{D} containing the (Dirichlet) shapes for each part.
}
  \item{n}{
  number of observations.
}
}

\details{
The number of columns in \code{u} and the number of components in \code{shape2} must match. 
}

\value{

 dSGB gives the density, rSGB generates a (\eqn{n \times D} - matrix with random compositions on each row. 
}


\seealso{

 \code{\link{bval}, \link{zval}}
}

\examples{
   u1 <- c(0.2,0.3,0.5)
   scale1 <- c(0.25,0.33,0.32)
   shape1 <- 1
   shape2 <- c(0.8,3,0.9)
   dSGB(u1,shape1,scale1,shape2)
   rSGB(10,shape1,scale1,shape2)
   

## with equal scales
   dSGB(u1,shape1,shape2=shape2)
   rSGB(10,shape1,shape2=shape2)
 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Probability Distributions and Random Numbers} 

