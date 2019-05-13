\name{GoodnessFit}
\alias{GoodnessFit}
\alias{ks.SGB}
\alias{cvm.SGB}
\alias{print.testSGB}

\title{
Goodness of fit tests on the marginal distributions of each part in a SGB model
}
\description{
Kolmogorov-Smirnov goodness of fit tests \cr
Cramer-von-Mises goodness of fit tests. 
}
\usage{
ks.SGB(u,shape1,shape2,scale,alpha=0.05)
cvm.SGB(u,shape1,shape2,scale,alpha=0.05)

\method{print}{testSGB}(x,\dots)
}

\arguments{
  \item{u}{
data matrix of compositions (independent variables) \eqn{(N \times D)}; \eqn{D}: number of parts
}
  \item{shape1}{
positive number, overall shape parameter of the SGB distribution. See \code{\link{SGBdistrib}}.
}
\item{shape2}{
vector of length \eqn{ D}, Dirichlet shape parameters of the SGB distribution. See \code{\link{SGBdistrib}}.
}
\item{scale}{
matrix of the same dimensions as \code{u}, containing the shape compositions, or positive number if the scales of all parts are identical. See \code{\link{SGBdistrib}}.
}
  \item{alpha}{
overall level of the test, default 0.05.
}
  \item{x}{
an object of class "testSGB".
}
  \item{\dots}{
further arguments passed to or from other methods.
}
}

\details{
\code{ks.SGB} calls \code{\link[stats]{ks.test}} and \code{cvm.SGB} calls \code{\link[goftest]{cvm.test}}. \cr  
Consider \eqn{z=C[(u/scale)^{shape1}]}, where \eqn{C[.]} is the closure operation. The scale compositions \eqn{scale} may be modelled with auxiliary variables. Under the SGB hypothesis, the components of  \eqn{ z} should be marginally beta-distributed. The functions provide \eqn{ D} tests, one for each part. \cr
Theoretically, the parameters should be known and not estimated on the data. Thus the test using estimated parameters is conservative. \cr
The cutoff value is based on the false discovery rate for multiple comparisons  (Benjamini and Hochberg, 1995), which is simply \code{alpha*i/D} for the i-th ordered p-value, i=1,...,D (number of tests). Reject the null hypothesis if at least one p-value is smaller than the cutoff. The overall level is then \code{alpha}. The proof of the result does not use an independence assumption between the tests.
}
\value{
A list of class 'testSGB' with the following components:\cr
  \item{method}{either "One-sample Kolmogorov-Smirnov test" or "Cramer-von Mises test of goodness-of-fit"
  }
  \item{Compositions}{name of the dataset \code{u}
  }
  \item{tests}{data frame with \eqn{D} rows and 3 columns: test statistics for each part against the beta distribution and corresponding p-values and cutoff. Any  p-value smaller than the cutoff means that the assumption of the beta distribution for all the margins is rejected.
  }


 A print method exists for the class "testSGB".
}
\references{
Benjamini, Y. and Y. Hochberg (1995). Controlling the false discovery rate: A practical and powerful approach to multiple testing. \emph{Journal of the Royal Statistical Society.} Series B (Methodological) 57 (1), 289-300. \cr \cr
Birnbaum, Z. W.  and Fred H. Tingey (1951), One-sided confidence contours for probability distribution functions. \emph{The Annals of Mathematical Statistics}, \strong{22}/4, 592-596. \cr \cr
Conover, William J.  (1971), \emph{Practical Nonparametric Statistics.} New York: John Wiley & Sons. Pages 295-301 (one-sample Kolmogorov test), 309-314 (two-sample Smirnov test). \cr \cr
Csorgo, S. and Faraway, J.J. (1996) The exact and asymptotic distributions of Cramer-von Mises statistics. \emph{Journal of the Royal Statistical Society}, Series B \strong{58}, 221-234. \cr \cr
Durbin, J. (1973), \emph{Distribution theory for tests based on the sample distribution function.} SIAM. \cr \cr
Marsaglia, G., Wai Wan Tsang and Jingbo Wang (2003), Evaluating Kolmogorov's distribution. \emph{Journal of Statistical Software}, \strong{8}/18.  
}


\seealso{
\code{\link{SGBdistrib}} for the theoretical distribution,  \code{\link{oilr}} for the regression results.
}
\examples{
## Generate 1000 random variates according to SGB(shape1,rep(1/3,3),shape2)
shape1 <- 0.6
shape2 <- c(10,20, 30)
rnum <- rSGB(1000,shape1,rep(1,3)/3,shape2)
ks.SGB(rnum,shape1=shape1, shape2=shape2,scale=1)
## same result as
ks.SGB(rnum,shape1=shape1,scale= matrix(rep(1/3,3000),ncol=3), shape2=shape2)
library(goftest)
cvm.SGB(rnum,shape1=shape1,scale= matrix(rep(1/3,3000),ncol=3), shape2=shape2)

## Arctic lake data

# oilr is a SGB regression object, see \code{\link{oilr}}.
data(oilr)       # regSGB object
data(arc)
ua <- arc[1:3]   # compositions

## Kolmogorov-Smirnov goodness of fit test
ks.SGB(ua,shape1=oilr[["par"]][1],shape2=oilr[["par"]][4:6],scale=oilr[["scale"]])
## Rounding shape1 affects the results less than rounding shape2.
ks.SGB(ua,shape1=round(oilr[["par"]][1],3),shape2=round(oilr[["par"]][4:6],1),
	 scale=oilr[["scale"]])
ks.SGB(ua,shape1=round(oilr[["par"]][1],1),shape2=round(oilr[["par"]][4:6],3),
	 scale=oilr[["scale"]])

## Cramer-von-Mises goodness of fit test
library(goftest)
cvm.SGB(ua,shape1=oilr[["par"]][1],shape2=oilr[["par"]][4:6],scale=oilr[["scale"]])

}
\keyword{Statistical Inference}
