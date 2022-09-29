rm( list = ls() )

## Import libraries / functions ------------------------------------------------

#remotes::install_github('coolbutuseless/wordle')

library( tidyverse )
library( wordle )
source( "functions.R" )

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ MAIN ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

helper <- WordleHelper$new( nchar = 5 )

#---Simulation routine
res_sim <- list()
for( sim in 1:100 ){
  
  #---Sample of target words
  test_words <- sample( helper$words, 1000 )
  
  #---Play game using random method
  method <- "random" 
  sim_rand <- sapply( test_words, function(x) unlist( simulate_wordle( target_word = x, method = method, quiet = T )[c(2,4)] ))
  res_rand <- as_tibble( t(sim_rand) )
  
  out_rand <- res_rand %>% 
              mutate( method = method, avg_guess = mean(n_guesses), win_prob = mean(solved) ) %>%
              nest( data = c(n_guesses, solved) )
  
  #---Play game using Bayesian method
  method <- "log_lik" 
  sim_lh <- sapply( test_words, function(x) unlist( simulate_wordle( target_word = x, method = method, quiet = T )[c(2,4)] ))
  res_lh <- as_tibble( t(sim_lh) )
  
  out_lh <- res_lh %>% 
            mutate( method = method, avg_guess = mean(n_guesses), win_prob = mean(solved) ) %>%
            nest( data = c(n_guesses, solved) )
  
  #---Output Results
  res_sim[[sim]] <- bind_rows( out_rand, out_lh )
  
}





# letter_frequency <- word_vecs %>% 
#   table() %>% 
#   data.frame() %>% 
#   rename( letter = 1 ) %>%
#   mutate( density = Freq / sum(Freq) )
# 
# letter_frequency %>% ggplot() + 
#   geom_bar( aes(x = letter, y = density ), position = "dodge", stat = "identity" ) +
#   labs( x = "Letter", y = "Density", title = "Total Letter Frequency" )
# 
# positional_letter_freq <- bind_rows( list_vec ) %>%
#   mutate( density = Freq / sum(Freq) )
# 
# positional_letter_freq %>% ggplot() + 
#   geom_bar( aes(x = letter, y = density, fill = as.factor(position) ), position = "dodge", stat = "identity" ) +
#   labs( x = "Letter", y = "Density", title = "Positional Letter Frequency" ) +
#   facet_grid( ~position ) +
#   theme( legend.position = "none" )
# 
# positional_letter_freq %>%
#   ggplot( aes(x = letter, y = position, fill = density) ) +
#   geom_tile()


