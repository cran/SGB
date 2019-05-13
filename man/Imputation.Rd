\name{Imputation}
\alias{Imputation}
\alias{impute.regSGB}

\title{
Imputation of missing parts in compositions from a SGB model
}
\description{
Applied to a completely missing composition, the function returns the Aitchison expectation. \cr
Applied to a partially missing composition, it returns the conditional Aitchison expectation, given the observed sub-composition. \cr
Applied to a complete case, it returns the complete case. \cr
}
\usage{
impute.regSGB(obj, dsup, usup)
}

\arguments{
  \item{obj}{
list, output of regSGB.
}
  \item{dsup}{
data frame with explanatory variables for the incomplete compositions. Missing values not allowed.
}
  \item{usup}{
compositions corresponding to \code{dsup}. On each row, the non-missing parts sum to 1.
}
}

\value{
data frame with imputed compositions instead of missing or partially missing compositions. Complete cases are also returned.
}


\examples{
## Arctic lake
data(arc)
arcmis <- arc
arc[11:13,]

## Introduce NA alues

arcmis[11,2] <- NA      # "core" observation
arcmis[12,3] <- NA      # outlying clay value
arcmis[13,1:3] <- NA    # totally missing observation
umis <- arcmis[,1:3]
umis <- umis/rowSums(umis,na.rm=TRUE)
umis[11:13,]

d <- data.frame(depth=arc[["depth"]])

## original compositions
arc[11:13,1:3]

## unconditional predicted value                            
MeanA.SGB(oilr[["par"]][1],oilr[["scale"]],oilr[["par"]][4:6] )[11:13,]

## predicted value given the sub-composition (sand,clay) for 11, (sand,silt) for 12        
impute.regSGB(oilr,arcmis,umis)[11:13, ]

impute.regSGB(oilr,arcmis[11:13, ],umis[11:13, ])  # same result. 
}

\keyword{Regression}
\keyword{Multivariate Techniques}
\keyword{Utilities}

