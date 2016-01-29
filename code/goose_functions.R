library(coda)

# get the next player index
fetch_next <- function(player, n) {
  res <- (player+1)%%n
  if (res==0) n else res
}

###################################

# check if there's someone at a certain position (zero does not count)
# return which player's there, 0 otherwise
on_top <- function(position, players) {
  if (position !=0 && position %in% players)
    which(players == position)[1]
  else
    0
}

###################################

swap <- function(v,i,j) {
  temp = v[i]
  v[i] = v[j]
  v[j] = temp
  v
}

###################################
# if verbose, the function shows a print of the entire game
# if ahead, the function returns a vector with the sequence of players
#  that were ahead until the end of the game
# otherwise, the game returns the total number of turns

play <- function(game, verbose=FALSE, 
                 step=FALSE,    # with verbose, print just the turn's end, not every step
                 ahead=FALSE,   # instead of #turns, it returns who's ahead during the game
                 navy=FALSE,    # special rules for game of navy
                 universe=FALSE # special rules for game of universe
                ) {
  players <- rep(0,game$players)   # positions of each player
  waits   <- rep(0,game$players)   # amounts of turns to wait
  next_player <- 1     
  turns <- 0
  
  if (ahead) {
    whos_ahead <- rep(NA,1e3*game$players) # the player ahead at i-th turn
  }
  
  while (all(players < game$size)) { # no one reach the last cell
    dices <- sample(1:6, 2)
    dice <- sum(dices)
    
    # if it's waiting, decrease it, and pass the move to the next player:
    
    if (waits[next_player] > 0) {
      waits[next_player] <- waits[next_player] - 1
      next_player <- fetch_next(next_player, game$players)
      turns <- turns+1
      
      if (ahead) {
        whos_ahead[turns] <- which.max(players)
        if (verbose) {
          cat(paste("turn:", turns, "---"))
          cat(whos_ahead[1:turns])
          cat("\n")
        }
      }

      next
    }      
    
    # compute new position
    
    if (turns<players && max(dices)==6 && sum(dices)==9) {
        if (navy==TRUE)
          new_position <- 28 # game of navy
        else
          new_position <- 26 # game of goose
    }
    else if (navy==TRUE && turns<players && max(dices)==5 && sum(dices)==9) {
      new_position <- 53
    }
    else if (is.element(players[next_player] + dice, game$geese)) # lands on a goose?
      new_position <- players[next_player] + 2*dice
    else {

      new_position <- players[next_player] + dice

      if (universe==TRUE) { # recompute if special rules for the game of universe apply
        if (players[next_player] == 8) 
          new_position <- players[next_player] + 4*dice
        
        else if (players[next_player] == 35) 
          new_position <- players[next_player] - dice
        
        else if (players[next_player] == 64 && dice==3) 
          new_position <- game$size
      }
    }
    
    if (new_position > game$size)                            # missed the last cell?
      new_position <- game$size - (new_position - game$size) # must go back

    # check if it reach an opponent
    index <- on_top(new_position, players)
    
    if (index>0) {  # reached an opponent!
      players <- swap(players, next_player, index) # swap positions
      waits   <- swap(waits,   next_player, index) # for the case the opponent was waiting
    } 
    
    else if (is.element(new_position, game$waits)) { # reached a stop cell? needs to wait
      waits[next_player]   <- game$amount[which(new_position == game$waits)]
      players[next_player] <- new_position
    }
    
    else if (is.element(new_position, game$jump)) {  # landed on a jump cell?
      sp <- which(new_position == game$jump)
      index <- on_top(game$into[sp], players)
      if (index>0) {  # reached an opponent!
        players <- swap(players, next_player, index) # swap positions
        waits   <- swap(waits,   next_player, index) # for the case the opponent was waiting
      } else
        players[next_player] <- game$into[sp]
    }
    
    else {
      players[next_player] <- new_position
    }
    
    if (verbose) {
      if (!step | (step & (turns+1) %% game$players == 0)) {
        cat(paste("player ", next_player, " with dice ", dices[1], "+", dices[2], "goes to ", players[next_player],
                  " current positions: "))
        cat(players)
        cat("\n")
      }
    }
    
    next_player <- fetch_next(next_player, game$players)
    turns <- turns+1
    
    if (ahead) {
      whos_ahead[turns] <- which.max(players)
      if (verbose) {
        cat(paste("turn:", turns, "---"))
        cat(whos_ahead[1:turns])
        cat("\n")
      }
    }
  
}
  
  if (ahead) whos_ahead[1:turns] else turns
}

###################################
library('fitdistrplus')

present_results <- function(game, res, show_fit=FALSE,
                            xlab="number of dice throws") {
  
  h <- hist(res, breaks=50, ylab=NA, xlab=xlab, prob=T, yaxt='n',
            main="Game of Goose", sub=paste0(game$players, " players"))
  
  if(show_fit) {
    fit <- fitdist(res, "lnorm")
    xs  <- seq(min(res), max(res), len=100)
    ys  <- dlnorm(xs, fit$estimate[1], fit$estimate[2])
    points(xs,ys,type="l", col="red", lwd=2)
    #   denscomp(fit, ylab=NA, xlab="number of dice throws",
    #            main="Game of Goose", sub=paste0(game$players," players"),
    #            lwd=2)
  }

  print(paste("min:",  min(res),           "moves"))
  print(paste("mean:", round(mean(res),0), "moves"))
  print(paste("max:",  max(res),           "moves"))
  
  interval <- HPDinterval(as.mcmc(res), prob=0.95)
  print(paste0("95% HPD: [",round(interval[1],2),",",round(interval[2],2),"]"))
  
  if (show_fit) {
    print("Log-normal fit")
    print(paste("- mean", round(fit$estimate[1],3)))
    print(paste("- sd",   round(fit$estimate[2],3)))
    list(min=min(res),
         mean=round(mean(res),0),
         max=max(res),
         interval=c(round(interval[1],2),round(interval[2],2)),
         lognorm_mean=round(fit$estimate[1],3),
         lognorm_sd  =round(fit$estimate[2],3))
  } else {
    list(min=min(res),
         mean=round(mean(res),0),
         max=max(res),
         interval=c(round(interval[1],2),round(interval[2],2)))
  }

  
#   if(show) {
#     report <- paste("min:",min(res),"mean:",round(mean(res),0),"max:",max(res))
#     text(h$mids[30],mean(h$counts)*3.5,report)
#     interval <- HPDinterval(as.mcmc(res), prob=0.95)
#     text(h$mids[30],mean(h$counts)*2.5,paste("95% HPD: [",round(interval[1],2),",",round(interval[2],2),"]"))
#   }
}


###################################
# criteria for drama
###################################

# criterium: persistence of winner, how many turns does she kept her winning position?

crit_persistence_winner <- function(v) {
  d <- diff(v)
  if (max(d)==0) # always the same winner
    length(v)
  else
    which(rev(d)!=0)[1]
}

###############################################
# criterium: who won the game?
###############################################

get_winner <- function (v) {v[length(v)]}

###################################
# criterium: how many players were ahead during the game
###################################

num_players_ahead <- function(v) {length(unique(v))}

###################################
# this function is used to just select the frontrunner at the end of an entire turn
# (including also the winner, if it is not the last player)
###################################

select.nth.position <- function(game_res) {
  tmp <- game_res[seq(game$players,length(game_res),game$players)]
  if (length(game_res) %% game$players != 0) {
    tmp <- c(tmp,game_res[length(game_res)]) # include the winner
  }
  tmp
}