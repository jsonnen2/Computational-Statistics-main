B = 100000
m = 8
mu0 = 3
alpha = 0.05

x = rexp(B*m, rate=1/mu0)
x = matrix(x, nrow=B)
x.bar = rowMeans(x)
x.stdev = rowMeans((x - x.bar)^2)
T.test = (x.bar - mu0) / (x.stdev / sqrt(m))
T.threshold = qnorm(alpha/2, lower.tail = FALSE)
alpha.sim = length(T.test[abs(T.test) > T.threshold]) / B
alpha.sim


# generate B random samples of size m from the Exp(5) distribution.
xb <- rexp(m*B, rate=1/mu0)
# store the b-th random sample in the b-th row of the matrix.
xbmat <- matrix(xb, nrow=B, ncol=m)
# apply() function calculates the test statistic value for each row.
tb <- apply(xbmat, 1, function(x){t.test(x, mu=mu0)$statistic})
# Monte Carlo simulator for the empirical Type I error rate.
alpha.sim <- mean(abs(tb) > qt((alpha/2), df=(m-1), lower.tail=FALSE))
alpha.sim
