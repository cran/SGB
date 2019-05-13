gr.SGB <-
function(x,d,u,V,weight,...){
   D <- NCOL(u)
   n <- NROW(u)
   D1 <- D-1
   npar <- length(x)
   r <- NCOL(d)-1
   shape1 <- x[1]
   R <- r*D1
#print(c(D,N,r,R))
   beta <- matrix(x[2:(D+R)], ncol=D1,byrow=TRUE)
#print(beta)
   shape2 <- as.vector(x[-(1:(D+R))])
   P <- sum(shape2)

   inV <- ginv(V)
   b <- exp((d%*%beta)%*%inV)
   b <- b/rowSums(b)
   z <- (u/b)^shape1
   z <- z/rowSums(z)
   logub <- as.matrix(log(u/b))
   Kb <- sum(weight*z*logub) 

   dl_dbeta <- shape1*inV %*% t(P*z-rep(1,n)%*%t(shape2)) %*% (weight*d )
   dl_dbeta <- as.vector(dl_dbeta)
   names(dl_dbeta) <- paste(sort(rep(paste("dl_dbeta",0:r,sep=""),D1)),rep(1:D1,r),sep="")

    
   dl_dshape1 <- n*D1/shape1 + sum(weight * logub %*% shape2)- P*Kb
   names(dl_dshape1) <- "dl_dshape1"
   dl_dshape2 <- n*(digamma(P)-digamma(shape2)) + colSums(weight*log(z)) 
   names(dl_dshape2) <- paste("dl_dshape2",1:D,sep="")
   return(c(dl_dshape1, dl_dbeta, dl_dshape2))
   
}
