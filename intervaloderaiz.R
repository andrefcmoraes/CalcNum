# Title     : Métodos Numéricos para Encontrar Raízes
# Objective : Exercício de cálculo numérico
# Created by: André Felipe C. Morais
# Created on: 10/05/2021
F <- function(x) {
  out <- 2*cos(x) - exp(x)/2
  return(out)
}
i <- 1
aws <- vector()
for (x in seq(-10,10,1)) {
  a <- x
  if(F(x-1)*F(x+1) < 0){
    aws[i] <- c(a)
    i <- i + 1
  }
}
print(aws)