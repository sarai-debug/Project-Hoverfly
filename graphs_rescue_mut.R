num_rounds <- 400
inita_pop <- 1000+50
max_gen <- 200

hZ_m_values <- seq(0.1, 0.9 , by = 0.1)


# creating a data frame to keep all output

results <- data.frame(hZ_m = numeric(), prob_rescue = numeric(), prob_extinct = numeric())
freq_df <- data.frame(array(NA, dim =c(0,3), dimnames = list(c(), c("hW_m", "wildtype", "mutant"))))


# for (i in 1:num_rounds)   
for (hZ_m in hZ_m_values) {
  
  extinct_count <- 0
  rescue_count <- 0
  
  for (i in 1:num_rounds){
    
    
    sim <- simulate_pop(Z_init_w = 100, Z_init_m = 50, f_w = 3, f_m = 3, 
                        hZ_w = 0.5, hZ_m = hZ_m, pZ_w = 0.5, pZ_m = 0.5, 
                        mut_rate = 0.001, t_max = max_gen)
    
    # total_pop <- round_gen[dim(round_gen)[1],1] + round_gen[dim(round_gen)[1],2]
    total_pop <- sum(tail(sim, 1))
    
    
    if (total_pop == 0){ 
      extinct_count <- extinct_count + 1
      
      # if (total_pop > inita_pop)
    }
    else{
      rescue_count <- rescue_count + 1
      freq_df <- rbind(freq_df, c(hZ_m, sim[nrow(sim),1]/total_pop, sim[nrow(sim),2]/total_pop))
    }
  }
  
  # result for each mutation rate
  results <- rbind(results, data.frame(hZ_m = hZ_m, 
                                       prob_rescue = rescue_count/num_rounds * 100, 
                                       prob_extinct = extinct_count/num_rounds * 100))
}
print(results)

  colnames(freq_df) <- c("hZ_m", "wildtype", "mutant")
  print(head(freq_df))

# plot for rescue probability

plot(results$hZ_m, results$prob_rescue, type='b',  
     col='darkgreen', pch=21, xlab='Mutant hatching rate', 
     ylab='Rescue probability (%)',
     main='Evolutionary Rescue vs Mutant hatching Rate')

# frequency plot against mutant hatching rate

plot(freq_df$hZ_m, freq_df$mutant, type='p',
     col='darkgreen', pch=21, xlab='Mutant hatching rate',
     ylab='Frequency of Mutant at End of Simulation',
     main='Frequency at Rescue vs Mutant hatching Rate')


# boxplot for frequency and mutant hatching rate
boxplot(freq_df ~ hZ_m,
        data = freq_df,
        col = "darkgreen",
        border = "black",
        xlab = "Mutant hatching rate",
        ylab = "Frequency of Mutant at End of Simulation",
        main = "Distribution of mutant frequency at rescue")


