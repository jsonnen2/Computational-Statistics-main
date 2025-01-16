
#### Problem 1c. ####

meanrow <- function(matrix) {
  matrix <- apply(matrix, 2, mean)
  return(matrix)
}

n = 1e4
population = c(1:400)
data = replicate(n, sample(population, 100, replace=TRUE))
data = meanrow(data)
hist(data)
sd(data)
mean(data)
#################################
# Problem 2
#################################
A = c(-.1, 1.5, -.6, -1.4,
      -1.2, -0.5, 1.0, -1.5,
      0.1, 0.5, -0.5, -2.0)
A = matrix(A, nrow=3, byrow=TRUE)
t(A)
B = A %*% t(A)
C = t(A) %*% A

install.packages("MASS")
library(MASS)
solve(B)
ginv(B)
solve(C)
ginv(C)

# The matrix C is singular. Check that the determinant is nearly zero.
det(C)

row.median = apply(A, 1, median)
column.std = apply(A, 2, sd)

#########
# Problem 3

data <- read.table("sleep.txt", header=T)
y <- data$NonD
length(y)
sum(!is.na(y))

# 3b
w = na.omit(y)
w = y[!is.na(y)]

# 3c
sleep17 = data[,1:7]
apply(na.omit(sleep17), 2, mean) # ignoring nan
apply(sleep17, 2, mean) # including nan

# 3d
sleep35 = data[,3:5]
boxplot(sleep35, main = "Box Plot of the Sleep Data")

# 3e
tapply(data$Sleep, data$Danger, mean, na.rm = T)


sumdice <- function(n) {
  dice = sample(1:6, n, replace=TRUE)
  return(sum(dice))
}

# 4a
v <- sample(1:6, 10000*100, replace=TRUE)

# 4b
vmat <- matrix(v, nrow=10000, ncol=100)

# 4c
row.sum <- rowSums(vmat)

# 4d
mean(row.sum)
var(row.sum)

# 4e
hist(row.sum, freq=F, breaks="Scott", main="Histogram of X", xlab="sum")


