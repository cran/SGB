hin.SGB <-
function(x,d,u,bound,...){
   npar <- length(x)
   D <- NCOL(u)
   n1 <- npar-D+1
   h <- vector()
   h[1] <- x[1]-0.1
   h[2:(D+1)] <- x[1]*x[n1:npar]-bound
   h[(D+2):(2*D+1)] <- x[n1:npar]
   return(h)
}
