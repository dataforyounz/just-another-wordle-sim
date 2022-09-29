
## Helper Functions
##
## simulate_wordle()
## evaluate_guess()
## make_guess()
## get_distribution()
## plot_density()

simulate_wordle <- function( target_word = NULL, method = "random", max_guess = 6, n_char = 5, quiet = F ){

  # Function to simulate Wordle   
  #
  # Uses two methods to generate guesses:
  # - log_lik: Uses the joint position and frequency distribution to compute posterior
  # - random:  Randomly selects a word from list
  
  #---Initial dictionary and helper function
  helper    <- WordleHelper$new( nchar = n_char )
  word_list <- helper$words
  
  #---Method Logic
  if( method %in% c("log_lik") ){
    
    # initialize weight matrix from current word list
    letter_dists <- get_distributions( word_list = word_list )
    weights      <- list()
    weights[[1]] <- letter_dists$joint_letter_pos_dist
    
  } else weights <- NULL
  
  #---Target word Logic
  if( is.null(target_word) ){
    
    # If no target word is specified
    # randomly select target word from dictionary
    target_word <- sample( word_list, 1 )
  }

  #---Initialize game
  game_init <- WordleGame$new( word_list, target_word = target_word )
  
  guess_count <- 1
  is_solved   <- F
  
  # while guess_count <= max_guess AND puzzle not solved DO
  while( guess_count <= max_guess & !is_solved ){
    
    # vectorize word list
    word_vecs    <- str_split( word_list, pattern = "" )
    
    # 1. generate guess (random or log lik)
    wts   <- weights[[guess_count]]
    guess <- make_guess( word_list = word_list, 
                         word_vecs = word_vecs, 
                         method    = method, 
                         weights   = wts
    )
    
    # 2. evaluate guess
    feedback <- evaluate_guess( game_init = game_init, target_word = target_word, guess = guess, quiet = quiet )
    
    # 3. update word list 
    helper$update(guess, feedback )
    word_list <- helper$words
    
    if( method %in% c("log_lik") ){
      
      # 4. update weight matrix (only if method is not random)
      #word_index <- which( word_list %in% helper$words )
      wts_update <- get_distributions( word_list = word_list )
      weights[[guess_count+1]] <- wts_update$joint_letter_pos_dist
      
    } else weights <- NULL
    
    # 5. update guess count and puzzle status
    guess_count <- guess_count + 1
    is_solved   <- game_init$is_solved()
    
  }
  
  output <- list()
  output$target_word <- target_word
  output$n_guesses   <- length( game_init$attempts )
  output$guesses     <- game_init$attempts
  output$solved      <- game_init$is_solved() 
  
  return( output )
  
}

evaluate_guess <- function( game_init, target_word, guess, quiet = quiet ){
  
  # Function evaluates the current guess and provides feedback
  #
  
  letter_mat  <- str_split( guess, pattern = "", simplify = T )
  feedback    <- game_init$try( guess, quiet = quiet )
  
  names( feedback ) <- letter_mat
  
  return( feedback )
}

make_guess <- function( word_list, word_vecs, method = method, weights = NULL ){
  
  if( method == "random" ){
    
    n_words <- length( word_list )
    weight  <- runif( n_words ) 
    guess   <- word_list[ which.max(weight) ]
    
  }

  if( method == "log_lik" ){
    
    if( is.null(weights) ) stop( "Weight matrix must be specified!" )
    
    index     <- stack( setNames( word_vecs, seq_along(word_vecs) ) )
    index$ind <- rep( 1:5, length(word_vecs) )
    
    probs <- matrix( weights[cbind(index$values, index$ind)], nrow = length(word_list), ncol = 5, byrow = T)
    score <- apply( log(probs), 1, sum )
    dens  <- exp( score - max(score) ) / sum( exp( score - max(score) ) )
    
    df   <- data.frame( word = word_list, score = score, dens = dens )
    best <- df[order(df$score, decreasing = T),][1,]
    
    #best <- df[sample( x = 1:length(word_list), size = 1, replace = F, prob = df$dens),]
    
    guess <- best$word

  }
  
  return( guess )
}


get_distributions <- function( word_list ){
  
  word_vecs <- str_split( word_list, pattern = "", simplify = T )
  
  # joint letter and position distributions
  list_mat <- matrix(0, 26, 5)
  
  for( position in 1:5 ){ 
    
    letter_freq <- word_vecs[,position] %>% table() 
    letter_idx  <- which( letters %in% names(letter_freq) )
    
    list_mat[letter_idx, position] <- as.numeric( letter_freq )
    
    }
  
  prob_matrix <- list_mat / sum( list_mat )
  rownames( prob_matrix ) <- letters
  colnames( prob_matrix ) <- c("1", "2", "3", "4", "5")
  
  out <- list()
  out$marginal_letter_dist  <- apply( prob_matrix, 1, sum )
  out$joint_letter_pos_dist <- prob_matrix
  out$marginal_pos_dist     <- apply( prob_matrix, 2, sum )
  
  return( out )
  
}

plot_density <- function( weights, palette = 1){
  
  tmp <- cbind( expand_grid( pos = 1:5, letters ),
                density = as.vector( weights) )
  
  print(
    as_tibble( tmp ) %>%
      ggplot( aes(x = pos, y = ordered(letters, levels = rev(unique(letters))), fill = density) ) +
      geom_tile() +
      scale_fill_distiller( type = "div", palette = palette, limits = c(0, .2) ) +
      labs(y = "", x = "Position")
  )
}



