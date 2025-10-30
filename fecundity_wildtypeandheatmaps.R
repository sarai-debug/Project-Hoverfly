num_rounds <- 400
inita_pop <- 1000 + 50
max_gen <- 200

# Range of wildtype fecundity to explore
f_w_values <- seq(1, 6, by = 0.5)   # can adjust based on realistic values

# Create result containers
results <- data.frame(f_w = numeric(), prob_rescue = numeric(), prob_extinct = numeric())
freq_df <- data.frame(array(NA, dim = c(0, 3), dimnames = list(c(), c("f_w", "wildtype", "mutant"))))

# Sweep over wildtype fecundity values
for (f_w in f_w_values) {
  
  extinct_count <- 0
  rescue_count <- 0
  
  for (i in 1:num_rounds) {
    
    sim <- simulate_pop(Z_init_w = 100, Z_init_m = 50, 
                        f_w = f_w, f_m = 2,        # << varying wildtype fecundity
                        hZ_w = 0.5, hZ_m = 0.5, 
                        pZ_w = 0.5, pZ_m = 0.5, 
                        mut_rate = 0.001, t_max = max_gen)
    
    total_pop <- sum(tail(sim, 1))
    
    if (total_pop == 0) { 
      extinct_count <- extinct_count + 1
    } else {
      rescue_count <- rescue_count + 1
      freq_df <- rbind(freq_df, c(f_w, sim[nrow(sim),1]/total_pop, sim[nrow(sim),2]/total_pop))
    }
  }
  
  # Store results
  results <- rbind(results, data.frame(f_w = f_w, 
                                       prob_rescue = rescue_count/num_rounds * 100, 
                                       prob_extinct = extinct_count/num_rounds * 100))
}

# Label the frequency data frame
colnames(freq_df) <- c("f_w", "wildtype", "mutant")

# --- Plot 1: Rescue probability vs wildtype fecundity ---
plot(results$f_w, results$prob_rescue, type = 'b',
     col = 'darkgreen', pch = 21,
     xlab = 'Wildtype adult fecundity (f_w)',
     ylab = 'Rescue probability (%)',
     main = 'Evolutionary Rescue vs Wildtype Fecundity')

# --- Plot 2: Frequency of mutants vs wildtype fecundity ---
plot(freq_df$f_w, freq_df$mutant, type = 'p',
     col = 'darkgreen', pch = 21,
     xlab = 'Wildtype adult fecundity (f_w)',
     ylab = 'Frequency of Mutant at End of Simulation',
     main = 'Frequency at Rescue vs Wildtype Fecundity')

# --- Plot 3: Boxplot of mutant frequencies ---
boxplot(mutant ~ f_w,
        data = freq_df,
        col = "darkgreen",
        border = "black",
        xlab = "Wildtype adult fecundity (f_w)",
        ylab = "Frequency of Mutant at End of Simulation",
        main = "Distribution of wildtype Frequency at Rescue")




###heat maps for mutant hatching rate vs mutant fecundity###

library(ggplot2)

# ranges for the parameters
hZ_m_values <- seq(0, 0.9, by = 0.1)
f_m_values <- seq(0, 10, by = 0.5)

num_rounds <- 400
max_gen <- 200

results_matrix <- expand.grid(hZ_m = hZ_m_values, f_m = f_m_values)
results_matrix$prob_rescue <- NA

for (i in 1:nrow(results_matrix)) {
  hZ_m <- results_matrix$hZ_m[i]
  f_m <- results_matrix$f_m[i]
  
  rescue_count <- 0
  
  for (rep in 1:num_rounds) {
    sim <- simulate_pop(Z_init_w = 100, Z_init_m = 50,
                        f_w = 2, f_m = f_m,
                        hZ_w = 0.5, hZ_m = hZ_m,
                        pZ_w = 0.4, pZ_m = 0.55,
                        mut_rate = 0.001, t_max = max_gen)
    total_pop <- sum(tail(sim, 1))
    if (total_pop > 0) rescue_count <- rescue_count + 1
  }
  
  results_matrix$prob_rescue[i] <- rescue_count / num_rounds * 100
}

# plot heatmap
ggplot(results_matrix, aes(x = f_m, y = hZ_m, fill = prob_rescue)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(name = "Rescue Probability (%)") +
  labs(x = "Mutant fecundity (f_m)", y = "Mutant hatching rate (hZ_m)",
       title = "Heatmap: Evolutionary Rescue Probability n_400") +
  theme_minimal()
 
####mutant fecundity vs wildtype fecundity###

# Load required library
library(ggplot2)

# Define parameter ranges
f_w_values <- seq(0, 5, by = 0.1)   
f_m_values <- seq(0, 5, by = 0.1)   

num_rounds <- 200   
max_gen <- 200       

# Data storage
results_matrix <- expand.grid(f_w = f_w_values, f_m = f_m_values)
results_matrix$prob_rescue <- NA

# Loop through each parameter combination
for (i in 1:nrow(results_matrix)) {
  f_w <- results_matrix$f_w[i]
  f_m <- results_matrix$f_m[i]
  
  rescue_count <- 0
  
  for (rep in 1:num_rounds) {
    sim <- simulate_pop(Z_init_w = 100, Z_init_m = 50,
                        f_w = f_w, f_m = f_m,
                        hZ_w = 0.5, hZ_m = 0.55,
                        pZ_w = 0.4, pZ_m = 0.55,
                        mut_rate = 0.001, t_max = max_gen)
    
    total_pop <- sum(tail(sim, 1))
    if (total_pop > 0) rescue_count <- rescue_count + 1
  }
  
  # Calculate rescue probability
  results_matrix$prob_rescue[i] <- rescue_count / num_rounds * 100
}

# --- PLOT: Heatmap of Rescue Probability ---
ggplot(results_matrix, aes(x = f_w, y = f_m, fill = prob_rescue)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(name = "Rescue Probability (%)") +
  labs(
    x = "Wildtype fecundity (f_w)",
    y = "Mutant fecundity (f_m)",
    title = "Heatmap: Evolutionary Rescue vs Fecundity of Wildtype and Mutant n_200"
  ) +
  theme_minimal(base_size = 13)

####mutation rate vs mutation hatching rate##

library(ggplot2)
library(viridis)
library(gridExtra)

set.seed(2025)

#parameters 
mut_rates <- 10^seq(-6, -2, length.out = 6)   
hZ_m_values <- seq(0, 1, by = 0.1)    

num_rounds <- 200 
max_gen <- 200      

# Fixed parameters but we can adjust it if we want
Z_init_w <- 100
Z_init_m <- 50
f_w <- 3
f_m <- 3
hZ_w <- 0.5
pZ_w <- 0.5
pZ_m <- 0.5

#result
grid <- expand.grid(mut_rate = mut_rates, hZ_m = hZ_m_values)
grid$prob_rescue <- NA_real_

#simulation loop
for (row in seq_len(nrow(grid))) {
  mr <- grid$mut_rate[row]
  hZm <- grid$hZ_m[row]
  
  rescue_count <- 0
  
  for (rep in 1:num_rounds) {
    sim <- simulate_pop(
      Z_init_w = Z_init_w, Z_init_m = Z_init_m,
      f_w = f_w, f_m = f_m,
      hZ_w = hZ_w, hZ_m = hZm,
      pZ_w = pZ_w, pZ_m = pZ_m,
      mut_rate = mr, t_max = max_gen
    )
    
    final_total <- sum(tail(sim, 1))
    
    if (final_total > 0) {
      rescue_count <- rescue_count + 1
    }
  }
  
  grid$prob_rescue[row] <- rescue_count / num_rounds * 100
  
  ##print
  if (row %% 5 == 0) cat("Completed", row, "of", nrow(grid), "\n")
}

#plot of the heatmap
grid$log10_mut <- log10(grid$mut_rate)

ggplot(grid, aes(x = log10_mut, y = hZ_m, fill = prob_rescue)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "C", name = "Rescue probability (%)") +
  scale_x_continuous(breaks = log10(mut_rates), labels = formatC(mut_rates, format = "e", digits = 0)) +
  labs(
    x = "Mutation rate (mut_rate)",
    y = "Mutant hatching rate (hZ_m)",
    title = "Evolutionary Rescue Probability"
  ) +
  theme_minimal(base_size = 13)

