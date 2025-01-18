library(glue)

accept_reject <- function(f, c, n) {
  # f: function F(x) which is the pdf of our target distribution
  # c: acceptance threshold. P(accept) = 1/c
  # n: number of samples to compute
  
  step_size <- n*c
  start <- 1
  u <- runif(step_size)
  y <- runif(step_size)
  x <- double(n)
  
  while (TRUE) {
    result <- f(y, c)
    accept <- result >= u
    result <- result[accept]
    
    end <- start + length(result)

    if (end >= n) { # Trim result when it overshoots n.
      end <- n - start + 1
      result <- result[1:end]
      x[start:n] = result
      return(x)
    }
    x[start: (end - 1)] <- result
    start = end
    step_size = (n - start)*c
  }
}


f <- function(y, c) { # Beta(2,2)
  6 * y * (1-y) / c
}

my_beta <- accept_reject(f, 1.5, 1000)
print(mean(my_beta))
print(var(my_beta))

explicit <- rbeta(1000, 2, 2)
print(mean(explicit))
print(var(explicit))








