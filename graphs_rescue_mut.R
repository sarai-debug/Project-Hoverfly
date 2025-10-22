```{r}
num_rounds <- 400
inita_pop <- 1000+50
mut_rates <- seq(0, 0.02, by = 0.005)


# creating a data frame to keep all output

results <- data.frame(mut_rate=numeric(), rescue_prob=numeric(), extinct_prob=numeric(), rescue_by_w=numeric(), rescue_by_m=numeric())
freq_df <- data.frame(array(NA, dim =c(0,3), dimnames = list(c(), c("mutation_rate", "wildtype", "mutant"))))

# for (i in 1:num_rounds)   
for (mut in mut_rates) {
  
  extinct_count <- 0
  rescue_count <- 0
  
  for (i in 1:num_rounds){
    
    
    sim <- simulate_pop(Z_init_w = 100, Z_init_m = 50, f_w = 3, f_m = 3, 
                        hZ_w = 0.5, hZ_m = 0.5, pZ_w = 0.5, pZ_m = 0.5, 
                        mut_rate = mut_rates,t_max = max_gen)
    
    # total_pop <- round_gen[dim(round_gen)[1],1] + round_gen[dim(round_gen)[1],2]
    total_pop <- sum(tail(sim, 1))
    
    
    if (total_pop == 0){ 
      extinct_count <- extinct_count + 1
      
      # if (total_pop > inita_pop)
    }
    else{
      rescue_count <- rescue_count + 1
      freq_df <- rbind(freq_df, c(mut, sim[nrow(sim),1]/total_pop, sim[nrow(sim),2]/total_pop))
    }
  }
  
  # result for each mutation rate
  results <- rbind(results, data.frame(mut_rates = mut, 
                                       prob_rescue = rescue_count/num_rounds * 100, 
                                       prob_extinct = extinct_count/num_rounds * 100))
}
print(results)

colnames(freq_df) <- c("mutation_rate", "wildtype", "mutant")
print(freq_df)

# plot for rescue probability

plot(results$mut_rates, results$prob_rescue, type='b',  
     col='darkgreen', pch=21, xlab='Mutation rate', 
     ylab='Rescue probability (%)',
     main='Evolutionary Rescue vs Mutation Rate')

# frequency plot against mutation rate

plot(freq_df$mutation_rate, freq_df$mutant, type='p',  
     col='darkgreen', pch=21, xlab='Mutation rate', 
     ylab='Frequency of Mutant at End of Simulation',
     main='Frequency at Rescue vs Mutation Rate')

# boxplot for frequency and mutation rate
boxplot(mutant ~ mutation_rate,
        data = freq_df,
        col = "darkgreen",
        border = "black",
        xlab = "Mutation rate",
        ylab = "Frequency of Mutant at End of Simulation",
        main = "Frequency at Rescue vs Mutation Rate")

```