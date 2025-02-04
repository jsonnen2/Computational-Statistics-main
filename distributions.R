library(glue)

accept_reject <- function(f, c, n) {
  # f: function F(x) which is the pdf of our target distribution
  # c: acceptance threshold. P(accept) = 1/c
  # n: number of samples to compute
  
  step_size <- n*c
  start <- 1
  x <- double(n)
  
  while (TRUE) {
    u <- runif(step_size)
    y <- runif(step_size)
    result <- f(y)
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


beta <- function(p, q) { # Beta distribution
  const <- factorial(p+q-1) / (factorial(p-1) * factorial(q-1))
  # TODO: this underestimates the max, which is catastrophic
  max_over_interval <- (p / (p+q))^(p-1) * (q / (p+q))^(q-1)
  c <- const * max_over_interval
  
  # create the function f(t) for the accept-reject method
  f <- function(y) {
    const * y^(p-1) * (1-y)^(q-1)/ c
  }
  # package as a list
  return(list(
    funct = f,
    c = c
  ))
}

result <- beta(2, 2)
print(result$c)
my_beta <- accept_reject(result$funct, result$c, 1000)
print(mean(my_beta))
print(var(my_beta))

explicit <- rbeta(1000, 2, 2)
print(mean(explicit))
print(var(explicit))








