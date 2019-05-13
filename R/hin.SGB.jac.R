hin.SGB.jac <-
function(x,d,u,...){
   npar <- length(x)
   D <- dim(u)[2]
   n1 <- npar-D+1
   J <- matrix(0, (2*D+1), npar)
   J[1,] <- c(1,rep(0,npar-1))
   J[2:(D+1),] <- cbind(x[n1:npar],matrix(0,D,npar-D-1),diag(rep(x[1],D)))
   J[(D+2):(2*D+1),n1:npar] <- diag(rep(1,D))
   return(J)
  }
