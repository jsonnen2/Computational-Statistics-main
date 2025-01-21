

normal <- function(n, mean=0, var=1) {
  # Use the box-muller method to generate an observation from the standard normal distribution
  # This method rounds down n to the next even number
  u <- runif(n/2)
  v <- runif(n/2)
  
  z1 = sqrt(-2 * log(u)) * cos(2*pi*v)
  z2 = sqrt(-2 * log(u)) * sin(2*pi*v)
  
  z = c(z1, z2)
  z = mean + sqrt(var) * z
  return(z)
}

chi2 <- function(n, df) {
  # mean = df
  # var = 2*df
  z = normal(n*df)
  z = matrix(z, nrow=n, ncol=df)
  chi = rowSums(z^2)
  return(chi)
}

student_t <- function(n, df) {
  # This seems inefficient when df is large
  z = normal(n)
  v = chi2(n, df)
  t = z / sqrt(v / df)
  return(t)
}

F_dist <- function(n, df1, df2) {
  u = chi2(n, df1)
  v = chi2(n, df2)
  f = (u / df1) / (v / df2)
  return(f)
}


