# Title     : Método de Newton-Raphson
# Objective : Exercício de cálculo numérico
# Created by: André Felipe C. Morais
# Created on: 04/05/2021

F <- function(x) {
  out <- 2*cos(x) - exp(x)/2 #defina a equação
  return(out)
}

tol <- as.numeric(readline("Defina o valor de tolerância: "))
S <- function(x,h) {
  out <- ( - F(x+2*h) + 8*F(x+h) - 8*F(x-h) + F(x-2*h)) / (12*h)
  return(out)
}

L <- function(x,h) {
  out <- (16*S(x,h)-S(x,2*h))/15
}

x0 <- as.integer(readline(prompt = "Insira o valor inicial do intervalo: "))
x1 <- x0 - F(x0)/L(x0,tol)
erro <- abs(x1 - x0)
count <- 0
while (erro > tol) {
  x1 <- x0 - F(x0)/L(x0,tol)
  erro <- abs(x1 - x0)
  x0 <- x1
  count <- count + 1
}
print(count)
print(x1)




