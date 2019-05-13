\name{oilr}
\alias{oilr}
\docType{data}
\title{
oilr data set
}
\description{
Arctic lake SGB regression based on isometric log-ratio transforms
}
\usage{data("oilr")}
\format{
List of 25 items, see \code{\link{regSGB}}.
}

\examples{
data(oilr)
oilr
summary(oilr)   # regSGB summary
oilr[["kkt1"]]  # first KKT condition
oilr[["V"]]     # matrix of log-ratio transformation

## oilr has been created by the following commands:
## Arctic lake data
 data(arc)
# Compositions
 ua <- arc[,1:3]

## ilr transforms
 c1 <- 1/sqrt(2)
 c2 <- 1/sqrt(6)
 Vilr <- matrix(c(-c1,c1,0,-c2,-c2,2*c2),nrow=3)
 colnames(Vilr) <- c("ilr1","ilr2")
 Vilr

## Formula
 F1 <- Formula(ilr1 | ilr2 ~ -1 + log(depth) )
# SGB regression object
 oilr <- regSGB(F1, data= list(arc, ua, Vilr), shape10=0.5, bound=2.1)
##########################################################

}
\keyword{datasets}
