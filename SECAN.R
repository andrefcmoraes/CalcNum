# Title     : Método das Secantes
# Objective : Exercício de cálculo numérico
# Created by: André Felipe C. Morais
# Created on: 05/05/2021

F <- function(x) {
  out <- x ^ 3 - 10*x
  return(out)
}

x0 <- as.integer(readline(prompt="Insira um valor inicial pertencente ao intervalo: "))
x1 <- as.integer(readline(prompt="Insira um valor secundário pertencente ao intervalo: "))
x2 <- (x0*F(x1)-x1*F(x0))/(F(x1)-F(x0)) 

erro <- abs(x2 - x1)
it = 0

while (erro > 0.0001) {
  x2 <- (x0*F(x1)-x1*F(x0))/(F(x1)-F(x0))
  erro <- abs(x2 - x1)
  x1 <- x2
  it = it + 1
}
print("----------Solução----------")
print(x2)
print("-----------erro------------")
print(erro)
print("--------iterações----------")
print(it)
print("---------------------------")