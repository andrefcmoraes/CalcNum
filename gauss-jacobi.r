vector1 <- c(10,1,2,2,5,3,1,1,10)
vector2 <- c(7, -8, 6)
a <- matrix(c(vector1), nrow = 3)
b <- matrix(c(vector2), ncol= 1)
print(a)
print(b)
GJ <- function(x1,x2,x3) {
    with(as.list(c(x1,x2,x3)), {
        y1 <- (1/a[1,1])*(b[1] - a[1,2]*x2 - a[1,3]*x3)
        y2 <- (1/a[2,2])*(b[2] - a[2,1]*x1 - a[2,3]*x3)
        y3 <- (1/a[3,3])*(b[3] - a[3,1]*x1 - a[3,2]*x2)
        return(list(c(y1,y2,y3)))
    })
}

init <- GJ(0.7,-1.6,0.6)

h1 <- init[[1]][1]
h2 <- init[[1]][2]
h3 <- init[[1]][3]
n <- 100
if (n >= 2) {
it <- n-2
for (i in seq(0,it,1)){

    fin <- GJ(h1,h2,h3)

    g1 <- fin[[1]][1]
    g2 <- fin[[1]][2]
    g3 <- fin[[1]][3]

    h1 <- g1
    h2 <- g2
    h3 <- g3
}

print(fin)
}
if(n < 2) {print(init)}
