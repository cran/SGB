\name{ocar}
\alias{ocar}
\docType{data}
\title{
ocar data set
}
\description{
Car segment shares SGB regression with formula
}
\usage{data("ocar")}
\format{
List of 25 items, see \code{\link{regSGB}}.
}

\details{
\code{ocar} is the same regression as \code{object3} in \code{\link{regSGB}}, Example 3.
}
\examples{
data(ocar)
ocar
summary(ocar)   # regSGB summary
ocar[["kkt1"]]  # first KKT condition
ocar[["V"]]     # matrix of log-ratio transformation

#######################################################
## ocar has been created by the following commands:
## Car segment shares
 data(carseg)
 
## Extract the compositions
uc <- as.matrix(carseg[,(1:5)])

## Define the log-ratio transformation matrix
Vc <- matrix(c( 1, 0, 0, 0,
               -1, 1, 0, 0,
                0,-1, 1, 0,
                0, 0,-1, 1,
                0, 0, 0,-1),ncol=4,byrow=TRUE)
colnames(Vc) <- c("AB","BC","CD","DE")
rownames(Vc) <- colnames(uc)
Vc

## Formula
Form <- Formula(AB | BC | CD | DE ~  log(expend) + I(PAC*log(expend)) + log(sent) + log(FBCF) + 
                 log(price) + rates)
ocar <- regSGB(Form, data = list(carseg, uc, Vc),shape10=4.4)
##########################################################

}
\keyword{datasets}
