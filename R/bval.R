bval <-
function(D,x,d,V){
   inV <- ginv(V)
   N <- NROW(d)
   r <- NCOL(d)
   D1 <- D-1
   a <- x[1]
   R <- r*D1
   
   beta <- matrix(x[2:(1+R)], ncol=D1,byrow=TRUE) 
   scale <- exp((d%*%beta)%*%inV)
   scale <- scale/rowSums(scale)
   return(scale)
   }


