
# 1a
m = 10
B = 1000
sigma = seq(0.1, 1, by=0.1)
alpha = 0.05
powers = c()

library(EnvStats)

for (s in sigma) {
  data = rgamma(B*m, shape=s, scale=1)
  data = matrix(data, nrow = B)
  tb <- apply(data, 1, function(x){varTest(x, alternative = "greater",
                                           sigma.squared = s)$statistic})
  empirical.power <- mean(tb > qt(1-alpha, df=m))
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
