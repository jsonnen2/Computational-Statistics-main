
# 1a
m = 10
B = 1000
sigma = seq(1, 6, by=0.5)
alpha = 0.05
powers = c()

library(EnvStats)

for (s in sigma) {
  data = rgamma(B*m, shape=s, scale=1)
  data = matrix(data, nrow = B)
  tb <- apply(data, 1, function(x){varTest(x, alternative = "greater",
                                           sigma.squared = 1)$statistic})
  empirical.power <- mean(tb > qchisq(alpha, df=m-1, lower.tail=F))
  powers = c(powers, empirical.power)
}
print(powers)

# 1b
plot(sigma, powers,
     xlab = "Sigma",
     ylab = "Power of the Test",
     main = "Problem 1a")
lines(sigma, powers)
abline(h=0.8)


# 2d
m = 10
B = 10000
p = 0.25
theta = 3

x = rexp(B*m, 1/theta)
x = matrix(x, nrow=B)

Q1 = -theta * log(1 - p)
mc.estimates = apply(x, 1, quantile, probs=0.25)
mc.theta = -mc.estimates / log(1 - p)
mc.mean = mean(mc.theta)
mc.mean

mc.variance = mean((mc.theta - mc.mean)^2)
mc.variance
theoretical.variance = (3/m) / log(1 - p)^2
theoretical.variance


# 3
library(bootstrap)
data = read.csv("earthquakes.csv", header = T)
distance = data[,"distanceKM"]

B = 10000
n = length(distance)
alpha = 0.05

# No Resampling
mean = mean(distance)
variance = mean((distance - mean)^2)
t.stat = qt(1-alpha/2, df=n-1)
CI.lower = mean - t.stat * sqrt(variance / n)
CI.upper = mean + t.stat * sqrt(variance / n)
CI.lower
CI.upper

# Non-Parametric Percentile Bootstrap
resample = sample(distance, size = B*n, replace = TRUE)
resample = matrix(resample, nrow = B)
mc.mean = rowMeans(resample)
grand.mean = mean(mc.mean)
mc.variance = mean((mc.mean - grand.mean)^2)
percentiles = quantile(mc.mean, probs = c(alpha/2, 1 - alpha/2))
percentiles

# Semi-Parametric Percentile Bootstrap
# The inverse of the sample mean is an estimate for theta. Generate Exp(1/x.bar)
x.bar = mean(distance)
semi.bootstrap = rexp(B*n, rate = 1/x.bar)
semi.bootstrap = matrix(semi.bootstrap, nrow=B)
mc.mean = rowMeans(semi.bootstrap)
grand.mean = mean(mc.mean)
mc.variance = mean((mc.mean - grand.mean)^2)
percentiles = quantile(mc.mean, probs = c(alpha/2, 1 - alpha/2))
percentiles

# Studentized Bootstrap
B = 1000
C = 200
theta = mean(distance)
resample = sample(distance, size = B*n, replace=TRUE)
resample = matrix(resample, nrow=B)
theta.b = rowMeans(resample)
var.theta = mean((theta.b - theta)^2)

# Use R's parallel package to multi-thread my code. 
# Reduced time from 16 sec to 3.8 sec
library(parallel)
cl <- makeCluster(detectCores() - 1)  # Use available cores
clusterExport(cl, c("C", "n"))  # Export necessary variables

# Compute estimates for variance(theta.b)
variance.estimates <- parApply(cl, resample, 1, function(row) {
  inner.resample = sample(row, size = C*n, replace=TRUE)
  inner.resample = matrix(inner.resample, nrow=C)
  theta.bc = rowMeans(inner.resample)
  theta.b.bar = mean(theta.bc)
  mean((theta.bc - theta.b.bar)^2) # Variance for each b block
})
stopCluster(cl)

# Find lower & upper percentile estimates for t*
tb = (theta.b - theta) / sqrt(variance.estimates)
t.star = quantile(tb, probs=c(1-alpha/2, alpha/2))

percentiles = theta - t.star * sqrt(var.theta)
percentiles



