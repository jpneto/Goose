library(coda)

# get the next player index
fetch_next <- function(player, n) {
  res <- (player+1)%%n
  if (res==0) n else res
}

###################################3

# check if there's someone at a certain position (zero does not count)
# return which player's there, 0 otherwise
on_top <- function(position, players) {
  if (position !=0 && position %in% players)
    which(players == position)[1]
  else
    0
}

###################################3

swap <- function(v,i,j) {
  temp = v[i]
  v[i] = v[j]
  v[j] = temp
  v
}

###################################3

play <- function(game, verbose=FALSE) {
  players <- rep(0,game$players)   # positions of each player
  waits   <- rep(0,game$players)   # amounts of turns to wait
  next_player <- 1     
  turns <- 0
  
  while (all(players < game$size)) { # no one reach the last cell
    dice <- sample(1:6, 1)
    
    # if it's waiting, decrease it, and pass the move to the next player:
    
    if (waits[next_player] > 0) {
      waits[next_player] <- waits[next_player] - 1
      next_player <- fetch_next(next_player, game$players)
      turns <- turns+1
      next
    }      
    
    # compute new position
    
    if (is.element(players[next_player] + dice, game$geese)) # lands on a goose?
      new_position <- players[next_player] + 2*dice
    else
      new_position <- players[next_player] + dice
    
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
      cat(paste("player ", next_player, " with dice ", dice, "goes to ", players[next_player],
                " current positions: "))
      cat(players)
      cat("\n")
    }
    
    next_player <- fetch_next(next_player, game$players)
    turns <- turns+1
  }
  
  turns
}

###################################3
library('fitdistrplus')

present_results <- function(game, res, show_fit=FALSE) {
  h <- hist(res, breaks=50, ylab=NA, xlab="number of dice throws", prob=T, yaxt='n',
           main="Game of Goose", sub=paste0(game$players, " players"))
  
  interval <- HPDinterval(as.mcmc(res), prob=0.95)
  
  if(show_fit) {

    # remove outliers    
    # res2     <- res[res > interval[1] & res < interval[2]]

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

  print(paste0("95% HPD: [",round(interval[1],2),",",round(interval[2],2),"]"))
  
  if (show_fit) {
    print("Log-normal fit")
    print(paste("- mean", round(fit$estimate[1],3)))
    print(paste("- sd", round(fit$estimate[2],3)))
  }
   
  list(interval=c(round(interval[1],2),round(interval[2],2)),
       lognorm_mean=round(fit$estimate[1],3),
       lognorm_sd  =round(fit$estimate[2],3))

#   if(show) {
#     report <- paste("min:",min(res),"mean:",round(mean(res),0),"max:",max(res))
#     text(h$mids[30],mean(h$counts)*3.5,report)
#     interval <- HPDinterval(as.mcmc(res), prob=0.95)
#     text(h$mids[30],mean(h$counts)*2.5,paste("95% HPD: [",round(interval[1],2),",",round(interval[2],2),"]"))
#   }
}

