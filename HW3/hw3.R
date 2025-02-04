# 1a
n = 100000
x = rexp(n, rate=2)
cdf = 1 - exp(-2 * x)
hist(cdf)


# 1c
B = 1000
n = 5
alpha = 2
beta = 3
x = rexp(B*n)
x = matrix(x, nrow = B)

gamma.alpha = rowSums(x[, 1:alpha])
gamma.beta = rowSums(x[, (alpha+1):(n)])
beta.dist = gamma.alpha / (gamma.alpha + gamma.beta)

hist(beta.dist, main="Beta distribution from exponential")
hist(rbeta(B, 2, 3), main="Using rbeta()")

# 3a
m = 100000
x = runif(m)
y = x^x
theta = mean(y)
theta

# 3b 
m = 100000
a = -1
b = 1
x = runif(m, min=a, max=b)
y = 1 / sqrt(1 - x^2)
y.bar = mean(y)
theta = (b - a) * y.bar
theta

# 3c
m = 100000
x = runif(m)
y = 1 - log(1-x)
z = (-1/y + 1/floor(y)) * 1/(1-x)
theta = mean(z)
theta

# 4
m = 1000
x = rbeta(m, 3, 3)
c = seq(0, 1, by=0.1)
F.c = double(length(c))
for (i in 1:length(c)) {
  F.c[i] = mean(x < c[i])
}
alpha = 0.05
upper.bound = F.c + qnorm(alpha/2, lower.tail = FALSE) * sqrt(F.c*(1-F.c)/m)
lower.bound = F.c - qnorm(alpha/2, lower.tail = FALSE) * sqrt(F.c*(1-F.c)/m)
plot(x=c, y=F.c, xlim=c(0,1), ylim=c(-0.1, 1.1), lwd=2, 
  main="Problem 4", 
  xlab = "c",
  ylab = "Cummulative Density",
  pch=3, type="n")
lines(x=c, y=F.c, lty=1, lwd=2)
lines(x=c, y=upper.bound, lty=2)
lines(x=c, y=lower.bound, lty=2)
lines(x=c, y=pbeta(c, 3, 3), lty=3)


# 5a
gamma = seq(0.2, 0.9, by=0.1)
r.sequence = 4 / gamma^2

# 5b
library(EnvStats)
B = 10000
m = 30
errors = c()
for (r in r.sequence) {
  data = rgamma(B*m, shape=r, scale=1)
  data = matrix(data, nrow = B)
  tb <- apply(data, 1, function(x){varTest(x, alternative = "greater",
                                            sigma.squared = r)$statistic})
  empirical.error <- mean(tb > qchisq(alpha, df=m-1, lower.tail=FALSE))
  errors = c(errors, empirical.error)
}
print(errors)

# 5c
plot(gamma, errors, 
     main="Problem 5c",
     xlab="Gamma",
     ylab="Empirical Type 1 Error")
lines(gamma, errors)
abline(h=0.075)


