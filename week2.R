rm(list=ls())

sumdice <- function(n) {
  k <- sample(1:6, size=n, replace=TRUE)
  return(sum(k))
}

mediandice <- function(matrix) {
  med <- apply(matrix, 2, median)
  return(median(med))
}

mat <- matrix(rnorm(100), nrow=20, ncol=5)
mediandice(mat)

mediandice <- function(matrix) {
  matrix <- apply(matrix, )
  return(matrix)
}
iris14mat = iris[,1:4]
colMeans(iris14mat)
apply(iris14mat, 2, sd)


x <- cbind(x1 = 3, x2 = c(4:1, 2:5))
dimnames(x)[[1]] <- letters[1:8]
apply(x, 2, mean, trim=.2)
rbeta(10, 4, 4)

mat <- matrix(c(1, 7, 3, 9, 4, 8, 6, 2, 5), nrow=3, byrow=TRUE)
apply(mat, 1, function(x) any(x > 5))

#### Inverse Transform

n = 1000
u = runif(n)
x = u^(1/3)
hist(x, prob=T)
y = seq(0, 1, .01)
lines(y, 3*y^2)

#### Inverse Transform-- Exponential Dist
rexp <- function(n, theta) {
  u = runif(n) 
  x = -theta*log(u)
  return(x)
}

n = 1000
theta = 2
u = runif(n)
x = rexp(n, theta)
hist(x, prob=T)

#### Inverse Transform with the Binomial Dist
n = 20
p = 0.4
u = runif(n)
x = as.integer(u > 0.6) 
table(x)/length(x)
mean(x)
var(x)
sum(x)


