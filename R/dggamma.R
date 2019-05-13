dggamma <-
function(x,shape1,scale,shape2){
 y <- (x/scale)^shape1
 dy_dx <- (shape1/scale) * (x/scale)^(shape1 - 1)
 dens <- dgamma(y,shape2)*dy_dx
return(dens)
}
