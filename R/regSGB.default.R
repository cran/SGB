regSGB.default <-
function(d, u, V,  weight=rep(1,dim(d)[1]), shape10 = 1, bound = 2.1, 
       ind = NULL, shape1 = NULL, Mean2 = TRUE ,
       control.optim = list(trace=0,fnscale=-1),
       control.outer = list(itmax=1000, ilack.max=200, trace=TRUE, kkt2.check = TRUE, method = "BFGS"),...){
       
  if (any(is.na(u)) | any(is.na(d))) stop('Missing values u or d are not allowed')
  if (NROW(u) != NROW(d)) stop('Different number of rows in u and d')
  
  d <- as.matrix(d)               
  weight <- weight/mean(weight)

  x0 <- initpar.SGB(d,u,V,weight,shape10, Mean2) 
  if(is.null(ind))  object <-   
  auglag(par = as.vector(x0), fn = fn.SGB, gr = gr.SGB, 
         hin = hin.SGB, hin.jac = hin.SGB.jac,
         control.optim = control.optim, control.outer = control.outer,
         d = d, u = u, V = V, weight = weight, shape10 = shape10, 
         bound = bound, shape1 = shape1, ind = ind)
  else {
  lind <- length(ind)
  minind <- min(ind)
  if (lind ==1 & minind==1)   object <- 
  auglag(par = as.vector(x0), fn = fn.SGB, gr = gr.SGB, hin = hin.SGB, 
          hin.jac = hin.SGB.jac, heq = heqa.SGB, hec.jac = heqa.SGB.jac,
          control.optim = control.optim, control.outer = control.outer,
          d = d, u = u, V = V, weight = weight, shape10 = shape10, 
          bound = bound, shape1 = shape1, ind = ind)
  if (lind >1 & minind==1)   object <-
  auglag(par = as.vector(x0), fn = fn.SGB, gr = gr.SGB, hin = hin.SGB, 
          hin.jac = hin.SGB.jac, heq = heqab.SGB, hec.jac = heqab.SGB.jac,
          control.optim = control.optim, control.outer = control.outer,
          d = d, u = u, V = V, weight = weight, shape10 = shape10, 
          bound = bound, shape1 = shape1, ind = ind)
  if (minind >1)   object <- 
  auglag(par = as.vector(x0), fn = fn.SGB, gr = gr.SGB, hin = hin.SGB, 
         hin.jac = hin.SGB.jac, heq = heqb.SGB, hec.jac = heqb.SGB.jac,
         control.optim = control.optim, control.outer = control.outer,
         d = d, u = u, V = V, weight = weight, shape10 = shape10,
         bound = bound, shape1 = shape1, ind = ind)
  }
  D <- dim(u)[2]

  object$scale <- bval(D,object[["par"]],d,V)
 

  colnames(object$scale) <- colnames(u)
  meanA <- MeanAobj.SGB(object)
  colnames(meanA) <- colnames(u)
  object$meanA <- meanA
   
  object$fitted.values <- log(meanA)%*%V
  object$residuals <- as.matrix((log(u) - log(meanA)))%*%V
  
  Gu <- var(log(u)-rowSums(log(u))%*%t(rep(1,D))/D )
  GEA <- var(log(object[["meanA"]]) -
           rowSums(log(object[["meanA"]])) %*%t(rep(1,D))/D )
  totvaru <- sum(diag(Gu))
  totvarEA <- sum(diag(GEA))
  object$Rsquare <- totvarEA/totvaru

  covlist <- covest.SGB(object$par,d,u,V,weight=weight,x0=x0,hessian=object$hessian,ind=ind,shape1=shape1)
  scores <- covlist[["scores"]]
  vcov <- covlist[["vcov"]]
  rownames(vcov) <- colnames(vcov) <- colnames(scores) <- rownames(covlist[[1]])
  object$scores <- scores
  object$vcov <- vcov
  object$StdErr1 <- covlist$StdErr1
  object$StdErr <- covlist$StdErr
  object$fixed.par <- ind
  object$summary <- covlist[["summary"]] 
  n.par <- length(object[["par"]])
  n.par.fixed <- length(object$fixed.par)
  AIC <- -2 * object[["value"]] + 2 * (n.par - n.par.fixed)
  object$AIC <- AIC
  object$call <- match.call()
  object$V <- V
  class(object) <- "regSGB"
  object
}

