source('goose_functions.R')         # 2 dice version (includes ad-hoc rules for classic game)

test_players <- c(3,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,5,5,5,
                  6,6,6,6,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,7,7,7,7,7,8,8,8,8,8,8,8,8,8,8,8,8)
size <- length(test_players)

goose_table <- data.frame( 
  game     = "",
  players  = numeric(size),
  min      = numeric(size),
  p2.5     = numeric(size),
  mean     = numeric(size),
  p97.5    = numeric(size),
  max      = numeric(size),
  lnorm.m  = numeric(size),
  lnorm.s  = numeric(size),
  persist1 = numeric(size),
  persist2 = numeric(size),
  persist3 = numeric(size),
  persist4 = numeric(size),
  persist5 = numeric(size),
  winner1  = numeric(size),
  winner2  = numeric(size),
  winner3  = numeric(size),
  winner4  = numeric(size),
  winner5  = numeric(size),
  winner6  = numeric(size),
  winner7  = numeric(size),
  winner8  = numeric(size),
  percentil= numeric(size),
  ahead1   = numeric(size),
  ahead2   = numeric(size),
  ahead3   = numeric(size),
  ahead4   = numeric(size),
  ahead5   = numeric(size),
  ahead6   = numeric(size),
  ahead7   = numeric(size),
  ahead8   = numeric(size))

n_games <- 40000   # how many games are to be played?
i = 1

for(n_players in test_players) {

#   game <- list( players = n_players,
#                 size    = 63,                                      # total number of cells
#                 geese   = c(), # where are the geese cells
#                 jump    = c(),                           # cells that require a jump
#                 into   = c(),                             # where to go?
#                 waits   = c(),                          # cells that require a stop
#                 amount = c() )                          # wait for how many turns?
  
# goose  
#   game <- list( players = n_players,
#                 size    = 63,                                      # total number of cells
#                 geese   = c(5,9,14,18,23,27,32,36,41,45,50,54,59), # where are the geese cells
#                 jump    = c( 6, 42, 58),                           # cells that require a jump
#                 into   = c(12, 30,  0),                             # where to go?
#                 waits   = c(19, 31,  52),                          # cells that require a stop
#                 amount = c( 2, Inf, Inf) )                          # wait for how many turns?

# universe  
#   game <- list( players = n_players,
#                 size    = 70,                                      # total number of cells
#                 geese   = c(), # where are the geese cells
#                 jump    = c( 2, 4, 11, 16, 28, 30, 40, 41, 45, 60, 68),
#                  into   = c(61, 5,  0, 20, 15, 12, 51, 32, 37,  0, 65),
#                 waits   = c(  7, 21, 27, 44,  59),      
#                  amount = c(Inf,  2,  2,  3, Inf) )     
  
# navy  
  game <- list( players = n_players,
                size    = 63,                                      # total number of cells
                geese   = c(9,18,27,36,45,54), # where are the geese cells
                jump    = c( 6, 25, 40, 58),
                 into   = c(12,  7, 30,  0),
                waits   = c(19,  32,  52),      
                 amount = c( 2, Inf, Inf) )  
  
  ## Run simulations and keep the number of turns that it takes to end each one
  res     <- replicate(n_games, play(game, navy = TRUE))  # execute simulation
  report  <- present_results(game, res, show_fit=TRUE)
  
  ###############################################
  ## Measuring Drama:
  ###############################################
  
  ## Run simulations and keep the players that were ahead each time
  res <- replicate(n_games, play(game, ahead=TRUE) ) # a list of 'ahead' vectors
  # and just keep those ahead at the end of each turn (ie, all players played once)
  res <- sapply(res, select.nth.position)
  
  ###############################################
  # criterium: persistence of winner, how many turns does she kept her winning position?
  ###############################################
  
  length_persistence_winner <- sapply(res, crit_persistence_winner)
  
  # save % of just 1 to 5 turns
  persistence <- 
    sapply(1:5, function(p) length(length_persistence_winner[length_persistence_winner==p]) / n_games)
  
  ###############################################
  # criterium: who won the game?
  ###############################################
  
  winners <- sapply(res, get_winner)
  table(winners)
  
  # Check if there is some statistical bias:
  source("desvio_funcs.R")
  
  more.wins <- max(table(winners))  # victories of 'best' player
  less.wins <- min(table(winners))  # victories of 'worst' player
  
  throws <- n_games  # throws per simulation
  n      <- 2000     # number of simulations
  
  ################
  # Compute how surprising is this result
  #  low  percentile suggests the result is       caused by uniform probability of winning
  #  high percentile suggests the result is *not* caused by uniform probability of winning
  
  percentil <- run(more.wins/less.wins, throws=throws, n=n, players=n_players)
   
  ###############################################
  # criterium: how many players were ahead during the game?
  ###############################################
  
  n_aheads <- sapply(res, num_players_ahead)
  table(n_aheads)
  
  ## add data to goose table
  
  goose_table[i,] <- list( 
                             game     = "goose",
                             players  = game$players,
                             min      = report$min,
                             p2.5     = report$interval[1],
                             mean     = report$mean,
                             p97.5    = report$interval[2],
                             max      = report$max,
                             lnorm.m  = report$lognorm_mean,
                             lnorm.s  = report$lognorm_sd,
                             persist1 = persistence[1],    # %games with winner 1 turn ahead
                             persist2 = persistence[2],
                             persist3 = persistence[3],
                             persist4 = persistence[4],
                             persist5 = persistence[5],
                             winner1  = table(winners)[1]/n_games, # %games won by player 1
                             winner2  = table(winners)[2]/n_games,
                             winner3  = table(winners)[3]/n_games,
                             winner4  = table(winners)[4]/n_games,
                             winner5  = table(winners)[5]/n_games,
                             winner6  = table(winners)[6]/n_games,
                             winner7  = table(winners)[7]/n_games,
                             winner8  = table(winners)[8]/n_games,
                             percentil = percentil,
                             ahead1  = table(n_aheads)[1]/n_games, # %games with 1 player ahead
                             ahead2  = table(n_aheads)[2]/n_games,
                             ahead3  = table(n_aheads)[3]/n_games,
                             ahead4  = table(n_aheads)[4]/n_games,
                             ahead5  = table(n_aheads)[5]/n_games,
                             ahead6  = table(n_aheads)[6]/n_games,
                             ahead7  = table(n_aheads)[7]/n_games,
                             ahead8  = table(n_aheads)[8]/n_games
                           )
  i=i+1

}

