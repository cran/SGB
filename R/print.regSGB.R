print.regSGB <-
function(x, ...)
{
  cat("Call:\n")
  print(x$call)
  cat("\nParameters:\n")
  print(x$par)
}
