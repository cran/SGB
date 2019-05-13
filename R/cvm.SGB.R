cvm.SGB <- function(u,shape1,shape2,scale,alpha=0.05){
   if (is.vector(scale) & length(scale)!=1) stop(" scale should be a scalar or of the same dimension as u")
   else if (is.matrix(scale)) {
      if (max(abs(dim(u) - dim(scale))) > 0)
   stop(" scale should be 1 or of the same dimension as u")}
   DNAME <- deparse(substitute(u))
   z <- (u/scale)^shape1
   z <- z/rowSums(z) 
  names(z) <- names(u)

  D <- dim(u)[2]
  N <- dim(u)[1]
  Ptot <- sum(shape2)
  ccvm <- list()
  resu <- data.frame(omega2=rep(NA,D),p.value=rep(NA,D))
  
  for (i in 1:D){
   yy <- z[,i]
   ccvm[[i]] <-goftest::cvm.test(yy, null = "pbeta",shape2[i],(Ptot-shape2[i]),nullname="beta distr.")
   resu[i,] <- as.numeric(unlist(ccvm[[i]][1:2]))
   row.names(resu) <- names(u)
  }
  resu <- resu[order(resu[["p.value"]]),]
  resu[["cutoff"]] <- alpha*(1:D)/D
  RVAL <- list(method="Cramer-von Mises test of goodness-of-fit",Compositions=DNAME, tests=resu)
  class(RVAL) <- "testSGB"
 return(RVAL)
}
