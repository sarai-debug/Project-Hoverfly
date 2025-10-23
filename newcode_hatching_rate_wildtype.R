num_rounds <- 400
inita_pop <- 1000+50
max_gen <- 200

hZ_w_values <- seq(0.1, 0.9 , by = 0.1)

# create result
results <- data.frame(hZ_w = numeric(), prob_rescue = numeric(), prob_extinct = numeric())
freq_df <- data.frame(array(NA, dim =c(0,3), dimnames = list(c(), c("hZ_w", "wildtype", "mutant"))))

# sweep over wildtype hatching rate
for (hZ_w in hZ_w_values) {
  
  extinct_count <- 0
  rescue_count <- 0
  
  for (i in 1:num_rounds){
    
    sim <- simulate_pop(Z_init_w = 100, Z_init_m = 50, f_w = 3, f_m = 3, 
                        hZ_w = hZ_w, hZ_m = 0.5, pZ_w = 0.5, pZ_m = 0.5, 
                        mut_rate = 0.001, t_max = max_gen)
    
    total_pop <- sum(tail(sim, 1))
    
    if (total_pop == 0){ 
      extinct_count <- extinct_count + 1
    } else {
      rescue_count <- rescue_count + 1
      freq_df <- rbind(freq_df, c(hZ_w, sim[nrow(sim),1]/total_pop, sim[nrow(sim),2]/total_pop))
    }
  }
  
  # store results
  results <- rbind(results, data.frame(hZ_w = hZ_w, 
                                       prob_rescue = rescue_count/num_rounds * 100, 
                                       prob_extinct = extinct_count/num_rounds * 100))
}

colnames(freq_df) <- c("hZ_w", "wildtype", "mutant")

# plots
plot(results$hZ_w, results$prob_rescue, type='b',  
     col='darkgreen', pch=21, xlab='Wildtype hatching rate', 
     ylab='Rescue probability (%)',
     main='Evolutionary Rescue vs Wildtype hatching Rate')

plot(freq_df$hZ_w, freq_df$mutant, type='p',
     col='darkgreen', pch=21, xlab='Wildtype hatching rate',
     ylab='Frequency of Mutant at End of Simulation',
     main='Frequency at Rescue vs Wildtype hatching Rate')

boxplot(mutant ~ hZ_w,
        data = freq_df,
        col = "darkgreen",
        border = "black",
        xlab = "Wildtype hatching rate",
        ylab = "Frequency of Mutant at End of Simulation",
        main = "Distribution of mutant frequency at rescue")
