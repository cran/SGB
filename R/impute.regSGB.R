impute.regSGB <-
function(obj,dsup,usup){
  if (NROW(dsup)!= NROW(usup)) stop('Different number of rows in dsup and usup')
  x <- obj[["par"]]
  d1 <- dsup
  V <- obj[["V"]]
  if (!is.null(obj[["Formula"]])){
 
   F1 <- formula(obj[["Formula"]],lhs=0)
   mt <- terms(F1, data=dsup)
   mf <- model.frame(formula=F1, data=dsup)
   ll <- length(obj[["Formula"]])[1]+1
   d1 <- model.matrix(attr(mf, "terms"), data=mf)

 #  d1 <- as.matrix(d1)[,-(1:ll)]
   r <- NCOL(d1)

#   for (i in 1:r) {
#      k <- 1
#      if (substr(colnames(d1)[i],1,2) == "I(") colnames(d1)[i] <- paste("I",k,sep="")
#      k <- k+1
#    }  
  
  }
  
  D <- NCOL(usup)
  NI <- NROW(usup)
  npar <- length(x)
  shape1 <- x[1]
  shape2 <- x[(npar-D+1):npar]
  scalesup <- bval(D,x,d1,V)
  usup_imp <- usup
  
  for (k in 1:NI){
    indk <- (1:D)[ is.na( usup_imp[k,] ) ]
    lk <- length(indk)
    if((lk > 0) & (lk < D)) {
      presk <- (1:D)[-indk]
      sck <- matrix(scalesup[indk],nrow=1)
      wk_hat <- MeanA.SGB(shape1,sck,shape2[indk])
      vk <- usup_imp[k,presk]
      vk <- vk/sum(vk)
      P2k <- sum(shape2[indk])
      P1k <- sum(shape2)-P2k
      scx1 <- (sum((   vk/scalesup[k,presk])^shape1))^(-1/shape1)
      scx2 <- (sum((wk_hat/scalesup[k,indk])^shape1))^(-1/shape1)
      x_hat <- MeanA.SGB(shape1,c(scx1,scx2),c(P1k,P2k))
      upresk <- x_hat[1]*vk
      uimpk <- x_hat[2]*wk_hat
      usup_imp[k,presk] <- upresk
      usup_imp[k,indk] <- uimpk
    }
    
    if (lk== D) usup_imp[k,] <- MeanA.SGB(shape1,scalesup[k,],shape2)
    
  }
  return(usup_imp)
}


