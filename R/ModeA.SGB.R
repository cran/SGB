ModeA.SGB <-
function(shape1,scale,shape2){
    modeA <- scale%*%diag(shape2^(1/shape1))
    colnames(modeA) <- colnames(scale)
    return(modeA/rowSums(modeA))
  }
