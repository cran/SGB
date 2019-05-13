#' @method summary regSGB
summary.regSGB <-
function(object,digits=3,...){
  res1 <- object[["summary"]]
  ncol <- dim(res1)[2]
  n1 <- ncol-1
  parameters <- cbind(res1[,1],round(res1[,2:n1],digits),res1[,ncol])
  names(parameters) <- names(res1)
  res <- list(call=object$call, Formula=object$Formula,
              parameters=parameters,
              signif.codes = " 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1")
  class(res) <- "summary.regSGB"
  res
}
