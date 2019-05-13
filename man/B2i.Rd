\name{B2i}
\alias{B2i}

\title{Balances to isometric log-ratio
}
\description{
 Coefficients of log of parts in a balance matrix,  
 (+1) for numerator  and (-1) for denominator, are transformed
 into the corresponding isometric log-ratio (ilr) coefficients
}
\usage{
B2i(bal, balnames=FALSE)
}

\arguments{
  \item{bal}{ a (\eqn{D-1} x \eqn{D}) balance matrix with cells +1, 0 or -1.  

}
  \item{balnames}{ logical, if TRUE, balance names are attributed to ilr transforms; if FALSE (default) ilr transforms are numbered \eqn{ilr1} to \eqn{ilrD1}, where \eqn{D1=D-1} and \eqn{D} is the number of parts.

}
}

\details{
Two scalars multiplying positive and negative cells respectively are defined for each row of the matrix \eqn{bal} in such a way that the resulting matrix defines the ilr transformation to apply to the log of a compositional vector. The output transformation matrix is transposed for application to a compositional dataset where the compositions are the rows.
}

\value{ a  \eqn{D \times (D-1)} matrix giving the coefficients of the ilr transforms 
}

\references{
Pawlowsky-Glahn, V., J. J. Egozcue, and R. Tolosana-Delgado (2007). Lecture Notes on Compositional Data Analysis.
}

\examples{
bal <- matrix(c(1,-1,0,1,1,-1),nrow=2, byrow=TRUE)
colnames(bal) <- paste("l.P",1:3,sep="") 
bal
B2i(bal)

rownames(bal) <- paste("B",1:2,sep="") 
bal
B2i(bal,balnames=TRUE)
B2i(bal)

}

\keyword{Utilities}

