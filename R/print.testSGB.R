print.testSGB <-
function(x, ...)
{
  cat("Method:")
  cat(x$method)
  cat("\nAlternative: two sided")
  cat("\nNull hypothesis: beta distr.")
  cat("\nCompositions:")
  cat(x$Compositions)
  cat("\n")
  print(x$tests)
}
