# Title     : Método da Bissecção
# Objective : Exercício de cálculo numérico
# Created by: André Felipe C. Morais
# Created on: 03/05/2021

F <- function(x) {
      out <- x-1 #AQUI VOCÊ ESCREVE A FUNÇÃO
  return(out)
}
tolerancia <- function(a0,b0,eps) {
  out <- (log(b0 - a0)-log(eps)) %/% log(2)
  return(out)
}

sinal <- function(n) {
    out <- ifelse(F(n) > 0, 1, -1)
    return(out)
}

a <- as.integer(readline(prompt="Insira o valor inicial do intervalo: "))
temp.a <- a
b <- as.integer(readline(prompt="Insira o valor final do intervalo: "))
temp.b <- b
erro <- as.numeric(readline("Defina o valor aproximado para o erro? "))
it = tolerancia(a,b,erro)+1
m0 <- (a + b)/2

for (i in 1:it) {
  m0 <- (a + b)/2
  if (sinal(m0) * sinal(a) < 0) {
    m1 <- (a + m0)/2
    b <- m0
  }
  else {
    m1 <- (b + m0)/2
    a <- m0
  }
}
print(paste0("o numero estimado necessário de iterações foi de: ",
             it," e o valor aproximado da raiz no intervalo ",
             temp.a, " e ", temp.b, " é: ",m0))
