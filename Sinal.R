# Title     : Função para definir intervalo com raiz
# Objective : Exercício de cálculo numérico
# Created by: André Felipe C. Morais
# Created on: 05/05/2021


F <- function(x) {
  out <- (x+1.2)*(x-2.8)*(x-7.98) #AQUI VOCÊ ESCREVE A EQUAÇÃO
  return(out)
}
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
