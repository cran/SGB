zval <-
function(u,x,d,V){
   shape1 <- x[1]
   D <- NCOL(u)
   scale <- bval(D,x,d,V)
   z <- (u/scale)^shape1
   z <- z/rowSums(z)             # zi follows a Dirichlet distr.
   return(z)
   }
