fn.SGB <-
function(x,d,u,V,weight,...){
   D <- NCOL(u)
   n <- NROW(u)
   npar <- length(x)
   r <- NCOL(d)-1
   shape1 <- x[1]
   names(shape1) <- ""
   R <- r*(D-1)

#   beta0 <- t(x[2:D])
#   beta1 <- matrix(x[(D+1):(D+R)], ncol=D-1)      
beta <- matrix(x[2:(D+R)],ncol=D-1,byrow=TRUE)
#   beta <- matrix(x[2:(D+R)], ncol=D1)
   shape2 <- as.vector(x[-(1:(D+R))])
   P <- sum(shape2)
   inV <- ginv(V)
#   b <- exp((rep(1,N)%*%beta0 +  d%*%beta1)%*%inV)
   b <- exp((d%*%beta)%*%inV)
#   b <- b/rowSums(b)           # 28.12.18

   z <- (u/b)^shape1
   z <- z/rowSums(z)

   lik <- n*((D-1)*log(abs(shape1)) + lgamma(P)- sum(lgamma(shape2)))+
          sum(weight*log(z)%*%shape2)
   return(lik)
}
