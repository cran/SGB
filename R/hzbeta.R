hzbeta <-
function(u,obj,weight=rep(1,dim(u)[1])){
   x <- obj[["par"]]
   xname <- deparse(substitute(x))
   shape1 <- x[1]
   scale <- obj[["scale"]]
   z <- (u/scale)^shape1
   z <- z/rowSums(z) 
  names(z) <- names(u)

  D <- dim(u)[2]
  N <- dim(u)[1]
  npar <- length(x)
  n1 <- npar-D+1
  pvec <- x[n1:npar] 
  Ptot <- sum(pvec)

  for (i in 1:D){
  zi <- z[,i]
  sw <- weight[order(zi)]
  cw <- cumsum(sw)
  xmin <- min(zi)
  xmax <- max(zi)
  quant <-  c("st","nd")
  if (D>2) quant <- c("st","nd","rd", rep("th",(D-3)))
  xx <- xmin + (1:99)*(xmax-xmin)/100
  yy <- dbeta(xx,pvec[i],Ptot-pvec[i])
  hi <- hist(zi,plot=FALSE)
  counts <- c(cw[hi[["counts"]][1]],diff(cw[cumsum(hi[["counts"]])]))
  cumsum(hi[["counts"]])
   density <- counts/(sum(counts)*diff(hi[["breaks"]]))
   hi[["counts"]] <- counts
   hi[["density"]] <- density
   hi[["xname"]] <- xname
  plot(hi,freq=FALSE,xlim=range(xx), xlab=paste(i,quant[i],"component of z"),main=names(z)[i],
        ylim=c(0,max(c(hi$density,yy))),ylab="Beta density")
  lines(xx,  yy,lwd=2,col="blue")
  }
  invisible(hi)
}


