covest.SGB <-
function(x,d,u,V,weight=rep(1,dim(d)[1]),x0=NULL,hessian=NULL,ind=NULL,shape1=NULL){
  if (is.null(hessian)) JJ <- jacobian(x=x,func=gr.SGB,method="Richardson",d=d, u=u, V=V, weight=weight)
  else JJ <- hessian
  vcov1 <- ginv(-JJ)
  rownames(vcov1) <- colnames(vcov1) <- names(x)
  StdErr1 <- sqrt(diag(vcov1))

  n <- NROW(d)
  D <- NCOL(u)
  r <- NCOL(d)
  npar <- length(x)
  varest3 <- matrix(0,ncol=npar,nrow=npar)
  scores <- matrix(NA,ncol=npar,nrow=n)
  
  for (k in 1:n){
    if (r==1)           grk <- gr.SGB(x,d[k],t(u[k,]),V,weight=1)
    if ((r>1) & (D==2)) grk <- gr.SGB(x,t(d[k,]),t(u[k,]),V,weight=1)  
    if ((r>1) & (D>2))  grk <- gr.SGB(x,t(d[k,]),t(u[k,]),V,weight=1) 
    scores[k,] <- t(grk)
  }
  colnames(scores) <- names(x)
  varest3 <- t(scores)%*%((weight^2)*scores)
  vcov <- (vcov1%*%varest3)%*%t(vcov1)
  rownames(vcov) <- colnames(vcov) <- names(x)
  StdErr <- sqrt(diag(vcov))
  x1 <- x
  x1[1] <- ifelse(is.null(shape1), x[1]-1, x[1]-shape1)
  q90 <- qnorm(0.95,0,1)
  sig90 <- (x1 - q90*StdErr > 0)| (x1 + q90*StdErr < 0)
  q95 <- qnorm(0.975,0,1)
  sig95 <- (x1 - q95*StdErr > 0)| (x1 + q95*StdErr < 0)
  signif <- rep("",npar)
  signif[sig90==TRUE] <- "."
  signif[sig95==TRUE] <- "*"
  q99 <- qnorm(0.995,0,1)
  sig99 <- (x1 - q99*StdErr > 0)| (x1 + q99*StdErr < 0)
  signif[sig99==TRUE] <- "**"
  q999 <- qnorm(0.9995,0,1)
  sig999 <- (x1 - q999*StdErr > 0)| (x1 + q999*StdErr < 0)
  signif[sig999==TRUE] <- "***"
  p.value <- rep(NA,npar)
  if (!is.null(ind)) {
     signif[ind] <- "fixed" 
     p.value[-ind] <- 2*pnorm(-abs(x1[-ind]/StdErr[-ind]))
  }
  else p.value <- 2*pnorm(-abs(x1/StdErr))
  summary <- data.frame(Estimate=x,StdErr1,StdErr,p.value,signif)
  
  if (!is.null(x0))  summary <- cbind(Parameters=names(x0),Initial=as.vector(x0),summary)
  return(list(summary=summary,scores=scores,
  vcov1=vcov1, StdErr1=StdErr1, vcov=vcov, StdErr=StdErr))
}
