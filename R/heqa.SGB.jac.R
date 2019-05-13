heqa.SGB.jac <-
function(x,...){
   
   J <- matrix(0, 1, length(x))
   J[1,1] <- 1
   return(J)
  }
