num_rounds <- 400
inita_pop <- 1000 + 50
max_gen <- 200

# Range of mutant fecundity to explore
f_m_values <- seq(1, 6, by = 0.5)  # can adjust depending on biological realism

# Create result containers
results <- data.frame(f_m = numeric(), prob_rescue = numeric(), prob_extinct = numeric())
freq_df <- data.frame(array(NA, dim = c(0, 3), dimnames = list(c(), c("f_m", "wildtype", "mutant"))))

# Sweep over mutant fecundity values
for (f_m in f_m_values) {
  
  extinct_count <- 0
  rescue_count <- 0
  
  for (i in 1:num_rounds) {
    
    sim <- simulate_pop(Z_init_w = 100, Z_init_m = 50, 
                        f_w = 3, f_m = f_m, 
                        hZ_w = 0.5, hZ_m = 0.5, 
                        pZ_w = 0.5, pZ_m = 0.5, 
                        mut_rate = 0.001, t_max = max_gen)
    
    total_pop <- sum(tail(sim, 1))
    
    if (total_pop == 0) { 
      extinct_count <- extinct_count + 1
    } else {
      rescue_count <- rescue_count + 1
      freq_df <- rbind(freq_df, c(f_m, sim[nrow(sim), 1]/total_pop, sim[nrow(sim), 2]/total_pop))
    }
  }
  
  # Store results
  results <- rbind(results, data.frame(f_m = f_m, 
                                       prob_rescue = rescue_count/num_rounds * 100, 
                                       prob_extinct = extinct_count/num_rounds * 100))
}

# Label the frequency data frame
colnames(freq_df) <- c("f_m", "wildtype", "mutant")

# Plot 1 — Rescue probability vs mutant fecundity
plot(results$f_m, results$prob_rescue, type = 'b',
     col = 'darkgreen', pch = 21,
     xlab = 'Mutant adult fecundity (f_m)',
     ylab = 'Rescue probability (%)',
     main = 'Evolutionary Rescue vs Mutant Fecundity')

# Plot 2 — Frequency of mutants vs fecundity
plot(freq_df$f_m, freq_df$mutant, type = 'p',
     col = 'darkgreen', pch = 21,
     xlab = 'Mutant adult fecundity (f_m)',
     ylab = 'Frequency of Mutant at End of Simulation',
     main = 'Frequency at Rescue vs Mutant Fecundity')

# Plot 3 — Boxplot showing variation
boxplot(mutant ~ f_m,
        data = freq_df,
        col = "darkgreen",
        border = "black",
        xlab = "Mutant adult fecundity (f_m)",
        ylab = "Frequency of Mutant at End of Simulation",
        main = "Distribution of Mutant Frequency at Rescue")
