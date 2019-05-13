MeanA.SGB <-
function(shape1,scale,shape2){
    pdig <- digamma(shape2)/shape1
    np <- length(shape2)
    EAu <- scale%*%diag(exp(pdig),nrow=np,ncol=np)
    EAu <- EAu/rowSums(EAu) 
    colnames(EAu) <- colnames(scale)
    return(EAu)
  }
