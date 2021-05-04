# Title     : Método da Bisseção
# Objective : Exercício de cálculo numérico
# Created by: André Felipe C. Morais
# Created on: 27/04/2021

F <- function(x) {
      out <- x^2 - 25
  return(out)
}

sinal <- function(a) {

    out <- ifelse(F(a) > 0, 1, -1)

    return(out)
}

sinal(3)