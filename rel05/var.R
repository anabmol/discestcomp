# Cálculo da variância utilizando o leem.

variance <- function(x, rounding = 2, na.rm = FALSE, details = FALSE) {
  if (class(x) != "leem") stop("Use the 'new_leem()' function to create an object of class leem!", call. = FALSE)
  if (class(x) == "leem" & is.null(attr(x, "table"))) x <- tabfreq(x)
  if (attr(x, "variable") == "discrete") {
    numchar <- is.numeric(x$estat$raw_data)
    if (numchar) {
      vari <- round(var(x = x$estat$raw_data,
                        na.rm = na.rm), digits = rounding)
      resume <- list(variance = variance, table = x$tabela, rawdata = x$estat$raw_data)
      if (details) {
        return(resume)
      } else {
        return(variance)
      }
      
    } else {
      stop("Measure not used for this data type!", call. = FALSE,
           domain = "R-leem")
    }
  }
  if (attr(x, "variable") == "continuous") {
    vari <- sum((x$tabela$Fi - mean(x))^2/(x$estat$raw_data - 1))
    return(vari)
  }
    
}

set.seed(10)
x <- rnorm(36, 100, 50)
x <- new_leem(x, variable = 2)
variance(x)







