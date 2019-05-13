EZ.SGB <-
function(D,x){
  shape1 <- x[1]
  lx <- length(x)
  lp1 <- lx-D+1
  shape2 <- x[lp1:lx]
  P <- sum(shape2)
  EZ <- shape2/P
  EZa <- exp(lgamma(P)-lgamma(P+1/shape1)+lgamma(shape2+1/shape1)-lgamma(shape2))
  EZa <- EZa/sum(EZa)
  EAZ <- exp(digamma(shape2))
  EAZ <- EAZ/sum(EAZ)
  EAZa <- exp(digamma(shape2)/shape1)
  EAZa <- EAZa/sum(EAZa)
  EE <- t(data.frame(EZ=EZ,EAZ=EAZ,EZa=EZa,EAZa=EAZa))
  return(EE)
}
