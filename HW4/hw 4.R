
# 1a
library(EnvStats)
B = 10000
alpha = 0.05
m.seq = seq(20, 120, by=20)
powers = c()

for (m in m.seq) {
  x = rgamma(B*m, 3, 1)
  x = matrix(x, nrow=B)
  tb <- apply(data, 1, function(x){varTest(x, alternative = "greater",
                                           sigma.squared = 2)$statistic})
  empirical.error <- mean(tb > qchisq(alpha, df=m-1, lower.tail=FALSE))
  powers = c(powers, empirical.error)
}
print(powers)

# 1b
plot(m.seq, powers, 
     main="Problem 1b",
     xlab="Sample size (m)",
     ylab="Empirical Power of the Test")
lines(m.seq, powers)
abline(h=0.8)

# 2b
# Monte Carlo integration with antithetic variables

m = 100000
u = runif(m)
v = 1 - u
f <- function(x){
  return(2 / sqrt(1 - x^2))
}
theta.u = mean(f(u))
theta.v = mean(f(v))
theta = (theta.u + theta.v) / 2
theta

# 3c
laplace <- function(n, mu, alpha) {
  X = rexp(n, rate = 1/alpha)
  Y = rexp(n, rate = 1/alpha)
  return(X - Y + mu)
}
m = 10000
mu = 10
alpha = 2
x = laplace(m, mu, alpha)
estimate = (2*alpha / sqrt(2*pi)) * exp(-(1-1/alpha)*abs(x - mu))
mean(estimate)


# 5.1
B = 10000
m = 8
mu = 3
alpha = 0.05
xb <- rexp(m*B, 1/mu)
xbmat <- matrix(xb, nrow=B, ncol=m)

CIs <- apply(xbmat, 1, function(x){t.test(x, mu=mu, conf.level=(1-alpha))$conf.int})
CI.lower <- CIs[1,]
CI.upper <- CIs[2,]
alpha.sim <- mean((CI.lower <= mu)*(CI.upper >= mu))
alpha.sim

# 5.2
B = 10000
m = 50
mu = 3
alpha = 0.05
xb <- rexp(m*B, 1/mu)
xbmat <- matrix(xb, nrow=B, ncol=m)

CIs <- apply(xbmat, 1, function(x){t.test(x, mu=mu, conf.level=(1-alpha))$conf.int})
CI.lower <- CIs[1,]
CI.upper <- CIs[2,]
alpha.sim <- mean((CI.lower <= mu)*(CI.upper >= mu))
alpha.sim

# 5.3
B = 10000
m = 20
df = 6
alpha = 0.05
xb <- rchisq(m*B, df)
xbmat <- matrix(xb, nrow=B, ncol=m)

CIs <- apply(xbmat, 1, function(x){t.test(x, mu=df, conf.level=(1-alpha))$conf.int})
CI.lower <- CIs[1,]
CI.upper <- CIs[2,]
alpha.sim <- mean((CI.lower <= df)*(CI.upper >= df))
alpha.sim

# 5.4
B = 10000
m = 10
mu = 0
df = 20
alpha = 0.05
xb <- rt(m*B, df)
xbmat <- matrix(xb, nrow=B, ncol=m)

CIs <- apply(xbmat, 1, function(x){t.test(x, mu=mu, conf.level=(1-alpha))$conf.int})
CI.lower <- CIs[1,]
CI.upper <- CIs[2,]
alpha.sim <- mean((CI.lower <= mu)*(CI.upper >= mu))
alpha.sim


