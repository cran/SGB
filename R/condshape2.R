# condshape2 <-
# function(x,d,u,V){
# shape1.est <- x[1]
# r <- NCOL(d)
# D <- dim(u)[2]
# D1 <- D-1
# R <- (r+1)*D1+1
# coefi.est <- matrix(x[2:R],ncol=(r+1))
# cond.value <- compushape2(shape1.est,coefi.est,d,u,V)
# compap <- list(shape2=data.frame(estimate=x[-(1:R)],cond.value))
# return(compap)
# }

 condshape2 <-
function (x, d, u, V) 
{
    shape1.est <- x[1]
    r <- NCOL(d)
    D <- dim(u)[2]
    D1 <- D - 1
    R <- length(x)-D
    coefi.est <- x[2:R]
    cond.value <- compushape2(shape1.est, coefi.est, d, u, V)
    compap <- list(shape2 = data.frame(estimate = x[-(1:R)], 
        cond.value))
    return(compap)
}