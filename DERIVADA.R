# Title     : DERIVADA NO PONTO
# Objective : Exercício de cálculo numérico
# Created by: André Felipe C. Morais
# Created on: 06/05/2021

F <- function(x) {
  out <- x ^ 3 - 10*x
  return(out)
}


S <- function(x,h) {
  out <- ( - F(x+2*h) + 8*F(x+h) - 8*F(x-h) + F(x-2*h)) / (12*h)
  return(out)
}

L <- function(x,h) {
  out <- (16*S(x,h)-S(x,2*h))/15
}


print("+++++++++++++")
print(L(2,10^-3))
print("+++++++++++++")