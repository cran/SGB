pzbeta <-
function(u,obj,weight=rep(1,dim(u)[1])){
   x <- obj[["par"]]
   shape1 <- x[1]
   scale <- obj[["scale"]]
   z <- (u/scale)^shape1
   z <- z/rowSums(z) 
  names(z) <- names(u)

  D <- dim(u)[2]
  N <- dim(u)[1]
  npar <- length(x)
  n1 <- npar-D+1
  shape2 <- x[n1:npar] 
  Ptot <- sum(shape2)

  quant <-  c("st","nd")
  if (D>2) quant <- c("st","nd","rd", rep("th",(D-3)))
  for (i in 1:D){
   zi <- z[,i]
   sw <- weight[order(zi)]
   cw <- cumsum(sw)/(sum(sw)+1)
   zi <- sort(zi)
   pi <- pbeta(zi,shape2[i],Ptot-shape2[i])
   plot(pi, cw, xlab=paste(i,quant[i],"component of z"),
      ylab="Beta probability",main=names(z)[i])
   abline(0,1,lwd=2,col="blue")
  }
}


