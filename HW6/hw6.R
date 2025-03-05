
# 1
ttest1 <- function(x, y) {
  t.test(x, y, var.equal=TRUE)$p.value
}
ttest2 <- function(x, y) {
  t.test(x, y, var.equal=FALSE)$p.value
}
ttest3 <- function(x, y, perm=1000) {
  m.x <- length(x)
  m.y <- length(y)
  abs.tstat <- abs(t.test(x,y)$statistic)
  xy = c(x,y)
  
  # Perform B permutation resamples.
  tstats.permutation <- replicate(perm, expr = {
    xy.b <- sample(xy, (m.x + m.y), replace=FALSE)
    x.b <- xy.b[1:m.x]
    y.b <- xy.b[(m.x + 1):(m.x + m.y)]
    abs(t.test(x.b, y.b, var.equal=TRUE)$statistic)
  })
  # Sum of the Indicator variables
  p.value <- mean(tstats.permutation >= abs.tstat)
  return(p.value)
}
ttest4 <- function(x, y, perm=1000) {
  m.x <- length(x)
  m.y <- length(y)
  abs.tstat <- abs(t.test(x,y)$statistic)
  xy = c(x,y)
  
  # Perform B permutation resamples.
  tstats.permutation <- replicate(perm, expr = {
    xy.b <- sample(xy, (m.x + m.y), replace=FALSE)
    x.b <- xy.b[1:m.x]
    y.b <- xy.b[(m.x + 1):(m.x + m.y)]
    abs(t.test(x.b, y.b, var.equal=FALSE)$statistic)
  })
  # Sum of the Indicator variables
  p.value <- mean(tstats.permutation >= abs.tstat)
  return(p.value)
}

# 1a data
m.x = 7
m.y = 8
B = 10000
alpha = 0.05
x = rt(m.x*B, m.x-1)
x.mat = matrix(x, nrow = B)
y = rt(m.y*B, m.y-1)
y.mat = matrix(y, nrow = B)

p.values = c()

for (i in 1:B) {
  x = x.mat[i,]
  y = y.mat[i,]
  
  p.values = c(p.values, ttest1(x, y))
  p.values = c(p.values, ttest2(x, y))
  p.values = c(p.values, ttest3(x, y))
  p.values = c(p.values, ttest4(x, y))
}
type.1.error.1a <- matrix(p.values, nrow=4)
type.1.error.1a
rowMeans(type.1.error.1a > alpha)
write.csv(type.1.error.1a, "1a.csv", row.names=FALSE)

# 1b data
m.x = 7
m.y = 8
x = rt(m.x*B, 4)
x.mat = matrix(x, nrow = B)
y = rt(m.y*B, 20)
y.mat = matrix(y, nrow = B)

p.values = c()

for (i in 1:B) {
  x = x.mat[i,]
  y = y.mat[i,]
  
  p.values = c(p.values, ttest1(x, y))
  p.values = c(p.values, ttest2(x, y))
  p.values = c(p.values, ttest3(x, y))
  p.values = c(p.values, ttest4(x, y))
}
type.1.error.1b <- matrix(p.values, nrow=4)
type.1.error.1b
rowMeans(type.1.error.1b > alpha)
write.csv(type.1.error.1b, "1b.csv", row.names=FALSE)



# 1c data
library(rmutil)
m.x = 7
m.y = 8
x = rlaplace(m.x*B, 0, 0.5)
x.mat = matrix(x, nrow = B)
y = rlaplace(m.y*B, 0, 2)
y.mat = matrix(y, nrow = B)

p.values = c()

for (i in 1:B) {
  x = x.mat[i,]
  y = y.mat[i,]
  
  p.values = c(p.values, ttest1(x, y))
  p.values = c(p.values, ttest2(x, y))
  p.values = c(p.values, ttest3(x, y))
  p.values = c(p.values, ttest4(x, y))
}
type.1.error.1c <- matrix(p.values, nrow=4)
type.1.error.1c
rowMeans(type.1.error.1c > alpha)
write.csv(type.1.error.1c, "1c.csv", row.names=FALSE)



# 1d data
library(rmutil)
m.x = 7
m.y = 8
x = rt(m.x*B, 6)
x.mat = matrix(x, nrow = B)
y = rlaplace(m.y*B, 0, 0.5)
y.mat = matrix(y, nrow = B)

p.values = c()

for (i in 1:B) {
  x = x.mat[i,]
  y = y.mat[i,]
  
  p.values = c(p.values, ttest1(x, y))
  p.values = c(p.values, ttest2(x, y))
  p.values = c(p.values, ttest3(x, y))
  p.values = c(p.values, ttest4(x, y))
}
type.1.error.1d <- matrix(p.values, nrow=4)
type.1.error.1d
rowMeans(type.1.error.1d > alpha)
write.csv(type.1.error.1d, "1d.csv", row.names=FALSE)



