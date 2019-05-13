# compushape2 <-
# function(shape1,coefi,d,u,V){
#    inV <- ginv(V)
#    N <- dim(u)[1]
#    D <- dim(u)[2] 
#    D1 <- D-1
#    b <- exp((d%*%coefi)%*%inV)
#    b <- b/rowSums(b)
#    z <- (u/b)^shape1
#    z <- z/rowSums(z)             # zi follows a Dirichlet distr.
#    q <- colMeans(z)              # (qj is xbar_j in text, j=1,...,D)
#    logG <- colMeans(log(z))
#    delta0 <- sum(q*logG)         # (2.72)
#    aplus <- D1*(-digamma(1))/(sum(q*log(q))-delta0)  # (2.73) p. 75
#    shape2 <- aplus*q            # initial values for shape2
#    return(shape2)
#  }
# It is supposed that the model is with intercept.

compushape2 <-
function(shape1,coefi,d,u,V){
   inV <- ginv(V)
   N <- dim(u)[1]
   D <- dim(u)[2] 
   D1 <- D-1
   beta <- matrix(coefi, ncol=D1,byrow=TRUE) 
   b <- exp((as.matrix(d)%*%beta)%*%inV)
   b <- b/rowSums(b)
   z <- (u/b)^shape1
   z <- z/rowSums(z)             # zi follows a Dirichlet distr.
   q <- colMeans(z)              # (qj is xbar_j in text, j=1,...,D)
   logG <- colMeans(log(z))
   delta0 <- sum(q*logG)         # (2.72)
   aplus <- D1*(-digamma(1))/(sum(q*log(q))-delta0)  # (2.73) p. 75
   shape2 <- aplus*q            # initial values for shape2
   return(shape2)
 }
