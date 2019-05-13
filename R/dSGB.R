dSGB <-
function(u,shape1,scale=1,shape2){
   P <- sum(shape2)
   D <- length(shape2)
   if (length(u) != D) stop("Length of shape2 should be equal to the number of parts")
   if (!missing(scale)) {
      if (length(u) != length(scale)) stop("u and scale should be of equal length")
      }
   z <- (u/scale)^shape1
   z <- z/sum(z)

   ldens <- ((D-1)*log(abs(shape1)) + lgamma(P)-sum(lgamma(shape2)))+
          sum(log(z)%*%shape2)-sum(log(u))
  return(exp(ldens))
}
