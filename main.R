# Title     : Métodos Numéricos para Encontrar Raízes
# Objective : Exercício de cálculo numérico
# Created by: André Felipe C. Morais
# Created on: 06/05/2021

F <- function(x) {
  out <- 2*cos(x)-exp(x)/2 #Aqui você escreve a equação
  return(out)
}

selection <- readline(prompt = "Insira newton, bissec ou secante: ")
raiz <- readline(prompt = "Deseja verificar os intervalos? y ou n: ")

if(raiz == "y") {

  i <- 1
  aws <- vector()
  for (x in 0:10) {
    a <- 0
    if(F(x-1)*F(x+1) < 0){a <- x}
    if (a != 0){
      
      aws[i] <- c(a)
      
      i <- i + 1
    }
  }
  i <- 1
  vws <- vector()
  for (x in 0:10) {
    a <- 0
    if(F(-x-1)*F(-x+1) < 0){a <- -x}
    if (a != 0){
      
      vws[i] <- c(a)
      
      i <- i + 1
    }
  }
  print("--------->--------")
  print(aws)
  print("--------->--------")
  print("##################")
  print("---------<--------")
  print(vws)
  print("---------<--------")
  print("PODE SER QUE ZERO SEJA SOLUÇÃO")
}





if(selection == "newton") {
err <- as.numeric(readline("Defina o valor aproximado para o erro? "))
S <- function(x,h) {
  out <- ( - F(x+2*h) + 8*F(x+h) - 8*F(x-h) + F(x-2*h)) / (12*h)
  return(out)
}

L <- function(x,h) {
  out <- (16*S(x,h)-S(x,2*h))/15
}

x0 <- as.integer(readline(prompt = "Insira o valor inicial do intervalo: "))
x1 <- x0 - F(x0)/L(x0,err)
erro <- abs(x1 - x0)
count <- 0
while (erro > 0.00000001) {
  x1 <- x0 - F(x0)/L(x0,err)
  erro <- abs(x1 - x0)
  x0 <- x1
  count <- count + 1
}
print(count)
print(x1)

}

if(selection == "bissec") {
  

tolerancia <- function(a0,b0,eps) {
  out <- (log(abs(b0 - a0))-log(eps)) %/% log(2)
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
it = abs(tolerancia(a,b,erro)+1)
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
print(paste0("o numero estimado necessário de iterações foi de ",
             it," e o valor aproximado da raiz no intervalo ",
             temp.a, " e ", temp.b, " é: ",m0))
}

if(selection == "secante") {

x0 <- as.integer(readline(prompt="Insira um valor inicial pertencente ao intervalo: "))
x1 <- as.integer(readline(prompt="Insira um valor secundário pertencente ao intervalo: "))
err <- as.numeric(readline("Defina o valor aproximado para o erro? "))
x2 <- (x0*F(x1)-x1*F(x0))/(F(x1)-F(x0)) 

erro <- abs(x2 - x1)
it <- 0

while (erro > err) {
  x2 <- (x0*F(x1)-x1*F(x0))/(F(x1)-F(x0))
  erro <- abs(x2 - x1)
  x1 <- x2
  it <- it + 1
}
print("----------Solução----------")
print(x2)
print("-----------erro------------")
print(erro)
print("--------iterações----------")
print(it)
print("---------------------------")
}
