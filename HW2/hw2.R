
# 1d
u = runif(1000)
f_inv <- function(x) {
  return(exp(x-1))
}
data = f_inv(u)
hist(data, prob=T, breaks=50)


# 1e
mean(data)
var(data)

# 2b
f_v <- function(v, theta) {
  return(-theta * logb(1-v))
}
u = runif(1000)
data = f_v(u, 1/5)
ground_truth = rexp(1000, 1/5)

# 2c 
hist(data, prob=T)

pdf <- function(x, theta) {
  return(1/theta * exp(-x / theta))
}
curve(pdf(x, 1/5), add=T)

# 2e
n = 1000
g = 5
poisson_samples = numeric(n)
cummulative_sum = numeric(n)
bitmask = rep(TRUE, n)
while (any(bitmask)) {
  e = rexp(n, g)
  cummulative_sum = cummulative_sum + e
  bitmask[cummulative_sum > 1] = FALSE
  poisson_samples[cummulative_sum < 1] = poisson_samples[cummulative_sum < 1] + 1
}

# 2f
hist(poisson_samples, main="My implementation")
hist(rpois(n, g), main="Using rpois()")

# 3c
laplace <- function(n, mu, alpha) {
  u = runif(1000)
  f1 = mu + 1/alpha * logb(2 * u[u<0.5])
  f2 = mu - 1/alpha * logb(2 - 2 * u[u>=0.5])
  return(c(f1, f2))
}

# 3d
hist(laplace(1000, 0, 2), main="Using Inverse Transform Method")
ground_truth = rexp(1000, rate=2) - rexp(1000, rate=2)
hist(ground_truth, main="Using Formula")

# 4c
f <- function(x) {
  # pdf of normal distribution ~ N(0, 1)
  return(1/sqrt(2*pi) * exp(-x^2/2))
}
g <- function(x) {
  # pdf of laplace distribution ~ Laplace(0, 1)
  return(1/2 * exp(-abs(x)))
}

c = sqrt(2*exp(1)/pi)
n = 1000
normal = c()
count = 0

while(length(normal) < n) {
  count = count + 1
  u = runif(n)
  laplace = rexp(n, rate=1) - rexp(n, rate=1)
  result = f(laplace) / (g(laplace) * c)
  normal = c(normal, laplace[result >= u])
}

# 4d
experimental_p_of_acceptance = length(normal) / (count * n)
theoretical_p_of_acceptance = 1/c
print(experimental_p_of_acceptance)
print(theoretical_p_of_acceptance)

# Trim to desired length
normal = normal[1:n]

# Histograms
hist(normal, main="Normal distribution using accept/reject method")
hist(rnorm(n), main="Normal distribution using rnorm()")

# EXTRA CREDIT
theta = 2.5
n = 1000
u = runif(n)
my_exponential = theta * log(1 / (1-u))
hist(my_exponential, main="Using strange formula")
hist(rexp(n, rate=theta), main="Using rexp()")


