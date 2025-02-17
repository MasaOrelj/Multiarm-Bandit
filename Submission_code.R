
#########################################################################################################################################################
# DATA DRIVEN DECISION MAKING ASSIGNMENT 1
#########################################################################################################################################################

library(ggplot2)
library(dplyr) 
library(tidyr)
library(knitr)
library(kableExtra)
library(openxlsx)

set.seed(123)  



########################################################################################
# ALGORITHEM
########################################################################################

# Define the policy
adaptive_bandit <- function(n = 200, N, l1, u1, l2, u2, m) {
  
  # Generate rewards from uniform distributions
  arm1_rewards <- runif(n, min = l1, max = u1)
  arm2_rewards <- runif(n, min = l2, max = u2)
  
  # Storage
  chosen_arms <- numeric(n)
  received_rewards <- numeric(n)
  
  # Initial exploration: Play each arm N/2 times
  chosen_arms[1:(N/2)] <- 1
  chosen_arms[(N/2 + 1):N] <- 2
  received_rewards[1:(N/2)] <- arm1_rewards[1:(N/2)]
  received_rewards[(N/2 + 1):N] <- arm2_rewards[(N/2 +1):N]
  
  # Compute initial means
  mean1 <- mean(received_rewards[chosen_arms == 1])
  mean2 <- mean(received_rewards[chosen_arms == 2])
  
  
  # Main loop
  for (t in (N+1):n) {
    # Compute probabilities
    prob1 <- mean1 / (mean1 + mean2)
    prob2 <- 1 - prob1
    epsilon <- max(1/(t),m)#0.025 for presentation
    
    if (runif(1) < epsilon) {
      chosen_arm <- ifelse(rbinom(1, 1, prob1) == 1, 1, 2)# Explore: pick a random arm
    } else {
      chosen_arm <- ifelse(mean1 >= mean2, 1, 2)  # Exploit: pick the best arm so far
    }
    
    # Select arm based on probability
    chosen_arms[t] <- chosen_arm
    
    # Receive reward
    reward <- ifelse(chosen_arm == 1, arm1_rewards[t], arm2_rewards[t])
    received_rewards[t] <- reward
    
    # Update means
    mean1 <- mean(received_rewards[chosen_arms == 1])
    mean2 <- mean(received_rewards[chosen_arms == 2])
  }
  
  # Compute total reward
  total_reward <- sum(received_rewards)
  
  # Compute best possible reward
  exp_val1 <- (l1+u1)/2
  exp_val2 <- (l2+u2)/2
  best_reward <- n*max(exp_val1,exp_val2)
  
  # Compute regret
  regret <- max(best_reward - total_reward,0)
  
  return(regret)
}




#######################################################################################################
# Choosing the parameters
#######################################################################################################

# Define parameter grids
epsilon_values <- seq(0.005, 0.02, by = 0.001) 
N_values <- seq(2, 10, by = 2)

# Create all combinations
param_grid_parameters <- expand.grid(epsilon = epsilon_values, N = N_values)

# Distribution boundaries
values <- c(0.1, 0.3, 0.5, 0.7, 0.9)

# Define parameter grid
param_grid <- expand.grid(l1 = values, u1 = values, l2 = values, u2 = values)
param_grid <- subset(param_grid, l1 < u1 & l2 < u2)  # Ensure valid ranges

#Num sim
n_sims <- 100

# Placeholder for results
results_parameter_selection <- data.frame(k = numeric(), N = numeric(), MeanRegret = numeric(), StandDev=numeric())

# Loop through each parameter combination
start = Sys.time()
for (j in 1:nrow(param_grid_parameters)) {
  epsilon <- param_grid_parameters$epsilon[j]
  N <- param_grid_parameters$N[j]
  
  results_parameters <- data.frame(l1 = numeric(), u1 = numeric(), l2 = numeric(), u2 = numeric(), 
                                   MeanRegret = numeric())
  for (i in 1:nrow(param_grid)) {
    l1 <- param_grid$l1[i]
    u1 <- param_grid$u1[i]
    l2 <- param_grid$l2[i]
    u2 <- param_grid$u2[i]
    
    # Run multiple simulations
    regrets <- replicate(n_sims, adaptive_bandit(n = 200, N = N, l1 = l1, u1 = u1, l2 = l2, u2 = u2,m=epsilon))
    
    # Compute statistics
    mean_regret <- mean(regrets)
    
    # Store results
    results_parameters <- rbind(results_parameters, data.frame(l1 = l1, u1 = u1, l2 = l2, u2 = u2, MeanRegret = mean_regret))
  }
  
  results_parameter_selection <- rbind(results_parameter_selection, data.frame(k=epsilon, N=N, MeanRegret=mean(results_parameters$MeanRegret),
                                                                               StandDev = sd(results_parameters$MeanRegret)))
}



#######################################################################################################
# COMPUTING RESULTS WITH SELECTED VALUE
#######################################################################################################

# Define parameter grid
values <- c(0.1, 0.3, 0.5, 0.7, 0.9)
param_grid <- expand.grid(l1 = values, u1 = values, l2 = values, u2 = values)
param_grid <- subset(param_grid, l1 < u1 & l2 < u2)  # Ensure valid ranges


# Run simulations
n_sims <- 5000  # Number of simulations per setting
main_results <- data.frame(l1 = numeric(), u1 = numeric(), l2 = numeric(), u2 = numeric(), 
                           MeanRegret = numeric(), StdErr = numeric(), LowerCI = numeric(), UpperCI = numeric())

for (i in 1:nrow(param_grid)) {
  l1 <- param_grid$l1[i]
  u1 <- param_grid$u1[i]
  l2 <- param_grid$l2[i]
  u2 <- param_grid$u2[i]
  
  # Run multiple simulations
  regrets <- replicate(n_sims, adaptive_bandit(n = 200, N = 4, l1 = l1, u1 = u1, l2 = l2, u2 = u2,m=0.006))
  
  # Compute statistics
  mean_regret <- mean(regrets)
  std_error <- sd(regrets) / sqrt(n_sims)
  ci_lower <- mean_regret - qnorm(0.975) * std_error #Compute 95% Confidence Interval
  ci_upper <- mean_regret + qnorm(0.975) * std_error
  
  ### I just changed the 1.96 to qnorm for more accurate results
  
  # Store results
  main_results <- rbind(main_results, data.frame(l1 = l1, u1 = u1, l2 = l2, u2 = u2, 
                                                 MeanRegret = mean_regret, StdErr = std_error, LowerCI = ci_lower, UpperCI = ci_upper))
}
