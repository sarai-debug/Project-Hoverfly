num_rounds <- 400
hatching_rates <- seq(0, 1, by = 0.05)


# creating a data frame to keep all output

results_hatch_wt <- data.frame(array(NA, dim =c(0,2), dimnames = list(c(), c("hatching_rate", "rescue_prob"))))

for (h in hatching_rates) {
  
  extinct_count <- 0
  rescue_count <- 0
  
  for (i in 1:num_rounds){
    
    
    sim <- simulate_pop(Z_init_w = 100, Z_init_m = 50, f_w = 3, f_m = 3, 
                        hZ_w = h, hZ_m = 0.5, pZ_w = 0.5, pZ_m = 0.5, 
                        mut_rate = 0.05,t_max = max_gen)
    
    total_pop <- sum(tail(sim, 1))
    
    
    if (total_pop == 0){ 
      extinct_count <- extinct_count + 1
      
    }
    else{
      rescue_count <- rescue_count + 1
    }
  }
  
  # result for each mutation rate
  results_hatch_wt <- rbind(results_hatch_wt, data.frame(hatching_rates = h, 
                                       prob_rescue = rescue_count/num_rounds * 100))
}
print(results_hatch_wt)


# plot for rescue probability

plot(results_hatch_wt$hatching_rates, results_hatch_wt$prob_rescue, type='b',  
     col='darkgreen', pch=21, xlab='Wildtype hatching rate', 
     ylab='Rescue probability (%)',
     main='Evolutionary Rescue vs Wildtype hatching rate')


# boxplot for frequency and mutation rate
boxplot(mutant ~ mutation_rate,
        data = freq_df,
        col = "darkgreen",
        border = "black",
        xlab = "Mutation rate",
        ylab = "Frequency of Mutant at End of Simulation",
        main = "Frequency at Rescue vs Mutation Rate")
