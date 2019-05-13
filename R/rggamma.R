rggamma <-
function(n,shape1,scale,shape2){
 y <- rgamma(n,shape2)
 return(scale*y^(1/shape1))
}
