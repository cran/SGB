table.regSGB <-
function(object){
  val <- data.frame(statistics=object[["value"]])
  n.par <- length(object[["par"]])
  val <- rbind(val,n.par)
  n.par.fixed <- length(object$fixed.par)
  val <- rbind(val,n.par.fixed)
  val <- rbind(val,object[["AIC"]])
  val <- rbind(val,object[["Rsquare"]])
  val <- rbind(val,object[["convergence"]])
  val <- rbind(val,object[["kkt1"]])
  val <- rbind(val,object[["kkt2"]])
  val <- rbind(val,object[["counts"]][1])
  val <- rbind(val,object[["counts"]][2])
#  val <- rbind(val,systest[3])
  row.names(val) <- c("value","n.par","n.par.fixed","AIC","Rsquare","convergence","kkt1","kkt2","counts.function",
                     "counts.gradient")
return(val)
}
