heqab.SGB <-
function(x,d,u,bound,shape1,index,...){
   li <- length(index)
   h <- vector()
   h[1] <- x[1]-shape1
   h[2:li] <- x[index[-1]]
   return(h)
}
