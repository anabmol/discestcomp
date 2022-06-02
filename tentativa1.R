#Probability   P(X > x)  P(X < x)

p <- function(x,dist = "poisson", lower.tail = TRUE, roundig = 4, porcentage = FALSE, gui = plot...) {
argnames <- names(formals(p))
  if (dist == "poisson") {
    #if (!any(argnames == "lambda")) stop("Insira o argumento 'lambda'!", call. = FALSE)
    if (lower.tail) {
      prob <- ppois(q = x, lambda = lambda)
    }
  }
  prob <- round(prob, rounding)
  if (porcentage == TRUE) prob <- prob * 100
  return(prob)
}
