rSGB <-
function(n, shape1, scale=rep(1,length(shape2)), shape2){
  D <- length(shape2)
  if (length(scale) != D) stop("Scales should be provided for each part")
  samp <- rggamma(n,shape1,scale=scale[1],shape2=shape2[1])
  for (i in 2:D){
  samp <- cbind(samp,rggamma(n,shape1,scale=scale[i],shape2=shape2[i]))
  }
  samp <- samp/rowSums(samp)
  dimnames(samp)[[2]] <- NULL
  return(samp)
}
