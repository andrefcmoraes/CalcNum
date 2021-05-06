# Title     : Método de Newton-Raphson
# Objective : Exercício de cálculo numérico
# Created by: André Felipe C. Morais
# Created on: 04/05/2021

F <- function(x) {
  out <- x ^ 3 - 10*x
  return(out)
}

DF <- function(x) {
  out <- 3*x^2-10
  return(out)
}

x0 <- as.integer(readline(prompt = "Insira o valor inicial do intervalo: "))
x1 <- x0 - F(x0)/DF(x0)
erro <- abs(x1 - x0)
count <- 0
while (erro > 0.00000001) {
  x1 <- x0 - F(x0)/DF(x0)
  erro <- abs(x1 - x0)
  x0 <- x1
  count <- count + 1
}
print(count)
print(x1)




