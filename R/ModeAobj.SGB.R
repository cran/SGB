ModeAobj.SGB <-
function(obj) {
    x <- obj[["par"]]
    shape1 <- x[1]
    scale <- obj[["scale"]]
    D <- NCOL(scale)
    npar <- length(x)
    n1 <- npar-D+1
    shape2 <- x[n1:npar]
    return(ModeA.SGB(shape1,scale,shape2))
  }
