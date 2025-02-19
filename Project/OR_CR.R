rm(list=ls())
#See https://stats.stackexchange.com/questions/588428/confidence-intervals-for-odds-ratio

#The OR_CI() function computes the odds ratio confidence interval.

#Input argument:
#n11: A numeric specifying the cell count for (1,1).
#n1d: A numeric specifying the total cell count for the first row.
#n21: A numeric specifying the cell count for (2,1).
#n2d: A numeric specifying the total cell count for the second row.
#dist: A character string specifying the distribution to be used. 
#"z" for standard normal, and "t" for t with Welch's adjustment
#adjust: A character string specifying the cell count adjustment. 
#"Agresti" for the independent-smoothed adjustment of Agresti.
#"Gart" for the Gart adjustment.
#"Woolf" for no adjustment, corresponding to the original Woolf logit interval
#alpha: A numeric related to confidence level. Note that the confidence level
#is given by 1-alpha. It is set to 0.05 by default.

OR_CI <- function(n11, n1d, n21, n2d, dist=c("z","t"), 
                  adjust=c("Agresti","Gart","Woolf"), alpha=0.05)
{
  dist <- match.arg(dist)
  adjust <- match.arg(adjust)
  #The second column counts
  n12 <- n1d - n11
  n22 <- n2d - n21
  #Total column counts
  nd1 <- n11 + n21
  nd2 <- n12 + n22
  #Grand total count
  N <- n11 + n12 + n21 + n22
  
  #Cell count adjustments
  if(adjust == "Gart")
  {
    n11a <- n11 + 0.5
    n12a <- n12 + 0.5
    n21a <- n21 + 0.5
    n22a <- n22 + 0.5
  }
  else if(adjust == "Agresti")
  {
    c11 <- 2*n1d*nd1/(N^2)
    c12 <- 2*n1d*nd2/(N^2)
    c21 <- 2*n2d*nd1/(N^2)
    c22 <- 2*n2d*nd2/(N^2)
    
    n11a <- n11 + c11
    n12a <- n12 + c12
    n21a <- n21 + c21
    n22a <- n22 + c22
  }
  else #adjust == "Woolf"
  {
    n11a <- n11
    n12a <- n12
    n21a <- n21
    n22a <- n22
  }
  
  #theta is the odds ratio (OR) estimate
  theta  <- (n11a*n22a)/(n12a*n21a)
  
  #Standard error
  se <- sqrt(1/n11a + 1/n12a + 1/n21a + 1/n22a)
  
  #Critical value calculation
  if(dist=="z")
  {
    crit <- qnorm(alpha/2, lower.tail=FALSE)
  }
  if(dist=="t")
  {
    Na <- n11a + n12a + n21a + n22a
    p11a <- n11a/Na
    p12a <- n12a/Na
    p21a <- n21a/Na
    p22a <- n22a/Na
    
    f1 <- 1/p11a + 1/p12a + 1/p21a + 1/p22a
    f3 <- 1/(p11a^3) + 1/(p12a^3) + 1/(p21a^3) + 1/(p22a^3)
    
    #Welch's degrees of freedom
    nu <- (2*Na*f1^2)/(f3 - f1^2)
    
    crit <- qt(alpha/2, df=nu, lower.tail=FALSE)
  }
  
  #Lower and upper bound of the confidence interval on the log scale
  loglower <- log(theta) - crit*se
  logupper <- log(theta) + crit*se
  
  #Lower and upper bound of the confidence inerval on the original scale
  lower <- exp(loglower)
  upper <- exp(logupper)
  
  #Handling NAs by assigning 0 and infinity for the lower and upper bound.
  lowerNAs <- which(is.na(lower))
  upperNAs <- which(is.na(upper))
  
  if(length(lowerNAs) > 0) lower[lowerNAs] <- 0
  if(length(upperNAs) > 0) upper[upperNAs] <- Inf
  
  results <- rbind(lower, upper)
  rownames(results) <- c("lower", "upper")
  
  return(results)
}

#The simulations matrix below gives all the different simulation scenarios to consider.
#n1d and n2d gives the total number of trials for the first and second population.
#p1 is the probability of success for the first population.
#OR is the odds ratio.
#alpha is the value so that 100(1-alpha)% represents the confidence level.

#Be sure to change these vectors below to match your research plan.
simulations <- expand.grid(n1d=seq(10,30,10), n2d=seq(10,30,10), 
                           p1=seq(0.1,0.9,0.4), OR=seq(1,40,3), alpha=0.05)
simulations

#numsimcases gives the number of simulation cases.
numsimcases <- dim(simulations)[1]
numsimcases

#Number of Monte Carlo simulations 
#(10000 or more is recommended, but use 1000 for preliminary study)
MC <- 10000

#Initializing some vectors
empirical_CR_za <- c()
empirical_CR_ta <- c()
empirical_CR_zg <- c()
empirical_CR_tg <- c()
empirical_CR_zw <- c()
empirical_CR_tw <- c()
p2 <- c()
for(i in 1:numsimcases)
{
  print(paste("Running simulation ", i, sep=""))
  #Retrieving information about the i-th simulation scenario.
  curr_sim <- simulations[i,]
  curr_n1d <- curr_sim$n1d
  curr_n2d <- curr_sim$n2d
  curr_p1 <- curr_sim$p1
  curr_OR <- curr_sim$OR
  curr_p2 <- curr_p1/(curr_OR*(1-curr_p1)+curr_p1)
  p2[i] <- curr_p2
  curr_alpha <- curr_sim$alpha
  
  #Generating random n11's and n21's.
  n11s <- rbinom(n=MC, size=curr_n1d, prob=curr_p1)
  n21s <- rbinom(n=MC, size=curr_n2d, prob=curr_p2)
  
  #dist = "z" and adjust = "Agresti"
  case_za <- OR_CI(n11=n11s, n1d=curr_n1d, n21=n21s, n2d=curr_n2d, 
                 dist="z", adjust="Agresti", alpha=curr_alpha)

  #dist = "t" and adjust = "Agresti"
  case_ta <- OR_CI(n11=n11s, n1d=curr_n1d, n21=n21s, n2d=curr_n2d, 
                   dist="t", adjust="Agresti", alpha=curr_alpha)
  
  #dist = "z" and adjust = "Gart"
  case_zg <- OR_CI(n11=n11s, n1d=curr_n1d, n21=n21s, n2d=curr_n2d, 
                   dist="z", adjust="Gart", alpha=curr_alpha)
  
  #dist = "t" and adjust = "Gart"
  case_tg <- OR_CI(n11=n11s, n1d=curr_n1d, n21=n21s, n2d=curr_n2d, 
                   dist="t", adjust="Gart", alpha=curr_alpha)  
  
  #dist = "z" and adjust = "Woolf"
  case_zw <- OR_CI(n11=n11s, n1d=curr_n1d, n21=n21s, n2d=curr_n2d, 
                   dist="z", adjust="Woolf", alpha=curr_alpha)
  
  #dist = "t" and adjust = "Woolf"
  case_tw <- OR_CI(n11=n11s, n1d=curr_n1d, n21=n21s, n2d=curr_n2d, 
                   dist="t", adjust="Woolf", alpha=curr_alpha)    
  
  #Empirical coverage rate calculations
  empirical_CR_za[i] <- mean((case_za[1,] <= curr_OR)*(curr_OR <= case_za[2,])==1)
  empirical_CR_ta[i] <- mean((case_ta[1,] <= curr_OR)*(curr_OR <= case_ta[2,])==1)  
  empirical_CR_zg[i] <- mean((case_zg[1,] <= curr_OR)*(curr_OR <= case_zg[2,])==1)
  empirical_CR_tg[i] <- mean((case_tg[1,] <= curr_OR)*(curr_OR <= case_tg[2,])==1)   
  empirical_CR_zw[i] <- mean((case_zw[1,] <= curr_OR)*(curr_OR <= case_zw[2,])==1)
  empirical_CR_tw[i] <- mean((case_tw[1,] <= curr_OR)*(curr_OR <= case_tw[2,])==1)   
}

#Empirical coverage rates
empirical_CR_za
empirical_CR_ta
empirical_CR_zg
empirical_CR_tg
empirical_CR_zw
empirical_CR_tw

#All the results can be found here.
all_results <- cbind(simulations, 
                 p2,
                 empirical_CR_za,
                 empirical_CR_ta,
                 empirical_CR_zg,
                 empirical_CR_tg,
                 empirical_CR_zw,
                 empirical_CR_tw                 
                 )
all_results

#Save simulation results as a CSV file.
write.csv(all_results, file="all_results.csv", quote=FALSE, row.names=FALSE)


