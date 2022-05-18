# Cálculo da variância utilizando o leem.

# Tabela em distribuição de frequências 
# @export
tabfreq <- function(dados, k = NULL, ordered = NULL){
  if (class(dados) != "leem") stop("Use the 'new_leem()' function to create an object of class leem")
  if (attr(dados, "variable") = "continuous") {
    # numeric ou character
    numchar <- is.numeric(dados)
    # tamanho da amostra 
    n <- length(dados)
    # Distribuição de frequência
    aux <- table(dados)
    if (numchar){
      groups <- as.numeric(names(aux))
    } else {
      if (is.null(ordered)) {
        groups <- as.numeric(names(aux))}
    } else {
      pos <- match (as.character(names(aux)), ordered)
      }
    }
  }

# Encontrando a variância 
library(leem)
set.seed(10)
x <- rnorm(36, 100, 50)
x <- new_leem(variable ="continuous") 
tab <- tabfreq(x)
tab$tabela$PM
tab$tabela$Fi
sum(tab$tabela$Fi*tab$tabela$PM)
var(x)  


