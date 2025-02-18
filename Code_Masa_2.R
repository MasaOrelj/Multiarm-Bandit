
#-------------------------------------------------------------Attempt 2-------------------------------------------------------------------
library(ggplot2)
library(dplyr) 
library(tidyr)
library(knitr)
library(kableExtra)
set.seed(123)  # For reproducibility

#b)

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
  regret <- max(0, best_reward - total_reward)
  
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
  
  

# Find the best parameters
best_params <- results_parameter_selection[order(results_parameter_selection$MeanRegret),]


# Table
kable(best_params[1:10, ], digits = 4, format = "html", row.names = FALSE, 
      align = rep("c", ncol(best_params))) %>%
  kable_styling(position = "center", 
                font_size = 16, # Adjust the font size if needed
                full_width = F,
                bootstrap_options = c("striped", "hover")) %>% # Makes the table more compact
  column_spec(1:ncol(best_params), width = "5em") %>% # Adjusts column width
  row_spec(0, bold = TRUE, background = "lightblue") #


#################################################################NULL#######################################################################################################
# Analysis of different values of epsilon
#######################################################################################################

# Filter data
k_analysis <- best_params %>% filter(N==4) %>% arrange(k)

# Plot the lines
plot_epsilon1 <- ggplot(k_analysis, aes(x = k)) +
  geom_bar(aes(y = MeanRegret, fill = "Mean Regret"), stat = "identity") +
  geom_line(aes(y = StandDev, color = "Standard Deviation"), linewidth = 1) +
  geom_point(aes(y = StandDev, color = "Standard Deviation"), size = 2) +
  labs(title = "Analysis for N=4 and for different k values",
       x = "k values", 
       y = "Regret") +
  scale_color_manual(values = c("Standard Deviation" = "red")) +
  scale_fill_manual(values = c("Mean Regret" = "light blue")) +
  coord_cartesian(ylim = c(0.05, NA)) +
  guides(fill = guide_legend(title = NULL), color = guide_legend(title = NULL)) +
  theme_minimal()+
  ylim(0, 2)+
  scale_x_continuous(breaks = k_analysis$k) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  


#######################################################################################################
# STARTING SAMPLE
#######################################################################################################

# Run simulations
starting_sample_size <- seq(2,30,by=2)
results_for_sample_size <- data.frame("Sample_size" = starting_sample_size, "Regret" = rep(0,length(starting_sample_size)), "StandDev" = rep(0,length(starting_sample_size)))
for (k in 1:length(starting_sample_size)){
  
  n_sims <- 100  
  results_sample <- data.frame(l1 = numeric(), u1 = numeric(), l2 = numeric(), u2 = numeric(), 
                        MeanRegret = numeric(), StandDev = numeric())
  
  for (i in 1:nrow(param_grid)) {
    l1 <- param_grid$l1[i]
    u1 <- param_grid$u1[i]
    l2 <- param_grid$l2[i]
    u2 <- param_grid$u2[i]
    
    # Run multiple simulations
    regrets <- replicate(n_sims, adaptive_bandit(n = 200, N = starting_sample_size[k], l1 = l1, u1 = u1, l2 = l2, u2 = u2, m=0.006))
    
    # Compute statistics
    mean_regret <- mean(regrets)
    std_error <- sd(regrets) / sqrt(n_sims)
    std_dev <- sd(regrets)
    
    # Store results
    results_sample <- rbind(results_sample, data.frame(l1 = l1, u1 = u1, l2 = l2, u2 = u2, MeanRegret = mean_regret, StandDev = std_dev))
    
  }
  
  results_for_sample_size[k,c("Regret", "StandDev")] <- c(mean(results_sample$MeanRegret), mean(results_sample$StandDev))
}

sample_size_analysis <- results_for_sample_size


#---------------------------------------------------------------Plot---------------------------------------------------------------------
plot_sample_size <- ggplot(sample_size_analysis, aes(x = Sample_size)) +
  geom_bar(aes(y = Regret, fill = "Mean Regret"), stat = "identity") +
  geom_line(aes(y = StandDev, color = "Standard Deviation"), linewidth = 1) +
  geom_point(aes(y = StandDev, color = "Standard Deviation"), size = 2) +
  labs(title = "Mean Regret and Standard Deviation for Different Starting Sample N",
       x = "Sample Size", 
       y = "Value") +
  scale_color_manual(values = c("Standard Deviation" = "red")) +
  scale_fill_manual(values = c("Mean Regret" = "light blue")) +
  guides(fill = guide_legend(title = NULL), color = guide_legend(title = NULL)) +
  scale_x_continuous(breaks = sample_size_analysis$Sample_size) + 
  coord_cartesian(ylim = c(0.05, NA)) +
  theme_minimal()





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
  
  # Computing the "convincing" confidence intervals (with level 1-delta) using Hoeffding's Inequality
  delta = 0.05 #For 95% Confidence Interval
  b = max(u1,u2)
  a = min(l1,l2)
  n=200
  epsilon_H <- sqrt(((b - a)^2 * log(2 / delta)) / (2 * n))
  ci_lower_H <- mean_regret - epsilon_H
  ci_upper_H <- mean_regret + epsilon_H
  
  # Store results
  main_results <- rbind(main_results, data.frame(l1 = l1, u1 = u1, l2 = l2, u2 = u2, 
                                                 MeanRegret = mean_regret, StdErr = std_error, LowerCI = ci_lower_H, UpperCI = ci_upper_H))
}


#######################################################################################################
# COMPARISON WITH FIXED ARM AFTER INITIAL SAMPLING
#######################################################################################################

# Define the policy
nonadaptive_bandit <- function(n = 200, N, l1, u1, l2, u2,m) {
  
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
  dominating_arm <- ifelse(mean1>=mean2, 1, 2)
  
  # Finishing with a fixed hand
  chosen_arms[(N+1):n] <- dominating_arm
  if (dominating_arm == 1) {
    received_rewards[(N+1):n] <- arm1_rewards[(N+1):n]
  } else {
    received_rewards[(N+1):n] <- arm2_rewards[(N+1):n]
  }
  
  # Compute total reward
  total_reward <- sum(received_rewards)
  
  # Compute best possible reward
  exp_val1 <- (l1+u1)/2
  exp_val2 <- (l2+u2)/2
  best_reward <- n*max(exp_val1,exp_val2)
  
  # Compute regret
  regret <- best_reward - total_reward
  
  return(regret)
}


# Run simulations
starting_sample_size <- seq(2,30,by=2)
results_for_sample_size_nonad <- data.frame("Sample_size" = starting_sample_size, "Regret" = rep(0,length(starting_sample_size)), "StandDev" = rep(0,length(starting_sample_size)))
for (k in 1:length(starting_sample_size)){
  
  n_sims <- 100
  results_non_adaptive <- data.frame(l1 = numeric(), u1 = numeric(), l2 = numeric(), u2 = numeric(), 
                                     MeanRegret = numeric(), StandDev = numeric())
  
  for (i in 1:nrow(param_grid)) {
    l1 <- param_grid$l1[i]
    u1 <- param_grid$u1[i]
    l2 <- param_grid$l2[i]
    u2 <- param_grid$u2[i]
    
    # Run multiple simulations
    regrets <- replicate(n_sims, nonadaptive_bandit(n = 200, N = starting_sample_size[k], l1 = l1, u1 = u1, l2 = l2, u2 = u2, m=optimal_epsilon))
    
    # Compute statistics
    mean_regret <- mean(regrets)
    std_dev <- sd(regrets)
    
    # Store results
    results_non_adaptive <- rbind(results_non_adaptive, data.frame(l1 = l1, u1 = u1, l2 = l2, u2 = u2, 
                                                                   MeanRegret = mean_regret, StandDev = std_dev))
  }
  
  results_for_sample_size_nonad[k,c("Regret", "StandDev")] <- c(mean(results_non_adaptive$MeanRegret), mean(results_non_adaptive$StandDev))
}

comparison_adaptive_nonadaptive <- merge(results_for_sample_size[,-3], results_for_sample_size_nonad[,-3], by="Sample_size")
colnames(comparison_adaptive_nonadaptive) <- c("SampleSize","Adaptive", "NonAdaptive")


long_comparison_data <- data.frame(pivot_longer(comparison_adaptive_nonadaptive, cols = c(Adaptive, NonAdaptive), 
                          names_to = "Regret_Type", 
                          values_to = "Regret_Value"))

plot_comparison <- ggplot(long_comparison_data, aes(x = SampleSize, y = Regret_Value, fill = Regret_Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +  
  labs(title = "Comparison with Non-Adaptive Approach", 
       x = "Sample Size",
       y = "Regret") +
  theme_minimal() +  # Minimal theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_continuous(breaks=long_comparison_data$SampleSize)+
  coord_cartesian(ylim = c(0.05, NA)) 



#######################################################################################################
# PRESENTING THE RESULTS
#######################################################################################################

plot_results <- main_results %>% filter(ifelse(l1 == l2, u1 <= u2, l1 <= l2))
min_regret_row_plot <- plot_results[which.min(plot_results$MeanRegret), ]
max_regret_row_plot <- plot_results[which.max(plot_results$MeanRegret), ]


#-----------------------------------------------------Plot-------------------------------------------------------------
plot_adaptive <- ggplot(plot_results, aes(x = MeanRegret, y = factor(paste(l1, u1, l2, u2, sep = "-")), color = MeanRegret)) +
  geom_point(size = 2) + 
  scale_color_gradient(low = "blue", high = "red") +  # Color gradient for mean regret
  ggtitle("Distribution-Dependent Regret") +
  xlab("Mean Regret") + 
  ylab("Uniform Distributions Combinations") +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "none") +
  scale_x_continuous(expand = expansion(mult = 0.05)) +
  # Highlight minimum regret
  geom_point(data = min_regret_row_plot, aes(x = MeanRegret, y = factor(paste(l1, u1, l2, u2, sep = "-"))), 
             color = "green", size = 5, shape = 20) +
  geom_errorbarh(data = min_regret_row_plot, aes(xmin = LowerCI, xmax = UpperCI, 
                                                 y = factor(paste(l1, u1, l2, u2, sep = "-"))),
                 height = 0.3, color = "green", linewidth = 0.7) +
  geom_text(data = min_regret_row_plot, aes(x = MeanRegret, y = factor(paste(l1, u1, l2, u2, sep = "-"))),
            label = paste("Min Regret \n", "[",min_regret_row_plot$l1,", ", min_regret_row_plot$u1,"]",
                          "[",min_regret_row_plot$l2,", ", min_regret_row_plot$u2,"]"),
            vjust = -1, color = "green", size = 4) +
  geom_point(data = max_regret_row_plot, aes(x = MeanRegret, y = factor(paste(l1, u1, l2, u2, sep = "-"))), 
             color = "orange", size = 5, shape = 20) + 
  geom_errorbarh(data = max_regret_row_plot, aes(xmin = LowerCI, xmax = UpperCI, 
                                                 y = factor(paste(l1, u1, l2, u2, sep = "-"))),
                 height = 0.3, color = "orange", linewidth = 0.7) +
  geom_text(data = max_regret_row_plot, aes(x = MeanRegret, y = factor(paste(l1, u1, l2, u2, sep = "-"))),
            label = paste("Max Regret \n", "[",max_regret_row_plot$l1,", ", max_regret_row_plot$u1,"]",
                          "[",max_regret_row_plot$l2,", ", max_regret_row_plot$u2,"]"),
            vjust = -1, color = "orange", size = 4)




### b2)

############################################################################################################################################
# FIXED ARM APPROACH
############################################################################################################################################

# Number of time periods and number of simulations
n <- 200
n_sims <- 200  # Number of simulations per setting

# Define possible reward parameter configurations
reward_configs <- param_grid

# Function to calculate the regret for fixed-action policies
calculate_regret <- function(l1, u1, l2, u2, n) {
  # Simulate rewards for both arms
  arm1_rewards <- runif(n, min = l1, max = u1)
  arm2_rewards <- runif(n, min = l2, max = u2)
  
  # Best possible total reward (always choose the better arm)
  exp_val1 <- (l1+u1)/2
  exp_val2 <- (l2+u2)/2
  best_reward <- n*max(exp_val1,exp_val2)
  
  # Fixed-action policy 1: Always choose arm 1
  total_reward_policy1 <- sum(arm1_rewards)
  regret_policy1 <- best_reward - total_reward_policy1
  
  # Fixed-action policy 2: Always choose arm 2
  total_reward_policy2 <- sum(arm2_rewards)
  regret_policy2 <- best_reward - total_reward_policy2
  
  return(c(regret_policy1, regret_policy2))
}

# Run simulations for each configuration and store regrets

results_fixed_arms <- data.frame(l1 = numeric(), u1 = numeric(), l2 = numeric(), u2 = numeric(), 
                                 MeanRegretArm1 = numeric(), StdErr1 = numeric(), MeanRegretArm2 = numeric(), Std_Err2 = numeric())

for (i in 1:nrow(reward_configs)) {
  # Extract current reward configuration
  config <- reward_configs[i, ]
  
  regrets_policy1 <- numeric(n_sims)
  regrets_policy2 <- numeric(n_sims)
    
  # Run the simulation n_sims times for the current configuration
  for (j in 1:n_sims) {
    
    regret_values <- calculate_regret(config$l1, config$u1, config$l2, config$u2, n)
    regrets_policy1[j] <- regret_values[1]
    regrets_policy2[j] <- regret_values[2]
  }
  
  # Calculate average regret and confidence bounds
  avg_regret_policy1 <- round(mean(regrets_policy1), digits=9)
  avg_regret_policy2 <- round(mean(regrets_policy2), digits=9)
  
  delta = 0.05 #For 95% Confidence Interval
  n=200
  # Confidence bounds for policy 1
  epsilon_H_policy1 <- sqrt(((u1 - l1)^2 * log(2 / delta)) / (2 * n))
  ci_lower_H_policy1 <- avg_regret_policy1 - epsilon_H_policy1
  ci_upper_H_policy1 <- avg_regret_policy1 + epsilon_H_policy1
 
  # Confidence bounds for policy 2
  epsilon_H_policy2 <- sqrt(((u2 - l2)^2 * log(2 / delta)) / (2 * n))
  ci_lower_H_policy2 <- avg_regret_policy2 - epsilon_H_policy2
  ci_upper_H_policy2 <- avg_regret_policy2 + epsilon_H_policy2

  
  # Store results
  results_fixed_arms <- rbind(results_fixed_arms, data.frame(l1 = config$l1, u1 = config$u1, l2 = config$l2, u2 = config$u2, 
                                                                 MeanRegretArm1 = avg_regret_policy1, StdErr1 = std_error1, MeanRegretArm2 = avg_regret_policy2, StdErr2 = std_error2))
  
}



#-------------------------------------------------------------------Plot-----------------------------------------------------------------------
plot_comparison <- merge(main_results, results_fixed_arms, by = c("l1", "u1", "l2", "u2"))

comparison <- data.frame("Mean Regret"=c(mean(plot_comparison$MeanRegret), mean(plot_comparison$MeanRegretArm1)),
           "Min Regret"= c(min(plot_comparison$MeanRegret), min(plot_comparison$MeanRegretArm1)),
           "Max Regret"=c(max(plot_comparison$MeanRegret), max(plot_comparison$MeanRegretArm1)),
           "Standard Dev"=c(sd(plot_comparison$MeanRegret), sd(plot_comparison$MeanRegretArm1)))



data_long <- pivot_longer(plot_comparison[,-c(6,7,8,10)], cols = c(MeanRegret, MeanRegretArm1), 
                          names_to = "Approach", values_to = "Regret")


ggplot(data_long, aes(x = Approach, y = Regret, fill = Approach)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +  
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 3)), 
               vjust = -0.5, size = 4, color = "black") +  
  stat_summary(fun.data = "mean_sdl", geom = "text", 
               aes(label = paste("Mean:", round(..y.., 3))), 
               vjust = -1, size = 3, color = "blue") +  
  scale_fill_manual(values = c("Mean_regret1" = "skyblue", "Mean_regret2" = "salmon")) +  
  labs(title = "Comparison of Adaptive vs Fixed Arm Regret",
       x = "Approach",
       y = "Mean Regret") +
  theme_minimal() +
  theme(legend.position = "none")
