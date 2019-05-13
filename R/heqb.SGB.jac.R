heqb.SGB.jac <-
function(x,d,u,bound,shape1,index,...){
   li <- length(index)
   J <- matrix(0, li, length(x))
   J[1:li,index] <- diag(rep(1,li))
   return(J)
  }
