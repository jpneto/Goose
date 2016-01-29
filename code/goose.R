# source('goose_functions_1dice.R') # 1 dice version
source('goose_functions.R')         # 2 dice version (includes ad-hoc rules for classic game)
###############################################
# game specification
###############################################

abstract <- list( players = 3,
               size    = 63,                                      # total number of cells
               geese   = c(), # where are the geese cells
               jump    = c(),                           # cells that require a jump
               into   = c(),                             # where to go?
               waits   = c(),                          # cells that require a stop
               amount = c() )                          # wait for how many turns?

goose <- list( players = 3,
               size    = 63,                                      # total number of cells
               geese   = c(5,9,14,18,23,27,32,36,41,45,50,54,59), # where are the geese cells
               jump    = c( 6, 42, 58),                           # cells that require a jump
                into   = c(12, 30,  0),                             # where to go?
               waits   = c(19, 31,  52),                          # cells that require a stop
                amount = c( 2, Inf, Inf) )                          # wait for how many turns?

game <- goose

###############################################
# run simulation
###############################################

n_games <- 5000                             # how many games are to be played?

# pick one:

res     <- replicate(n_games, play(game))  # execute simulation
res     <- replicate(n_games, play(game, navy=TRUE))      # execute game of navy
res     <- replicate(n_games, play(game, universe=TRUE))  # execute game of universe

###############################################
# present results
###############################################

report <- present_results(game, res, show_fit=TRUE)

###############################################
# use this to check one game, step by step
# it returns the total number of dice throws
###############################################

play(game, verbose=TRUE) 
play(game, verbose=TRUE, navy=TRUE) 
play(game, verbose=TRUE, universe=TRUE) 

play(game, verbose=TRUE, step=TRUE)  # show board only at the end of an entire turn

###############################################
# use this to return a vector with the players that were ahead until the game ended
###############################################

play(game, ahead=TRUE) 

###############################################
# use vectors to measure drama
###############################################

n_games <- 500
res <- replicate(n_games, play(game, ahead=TRUE) ) # a list of 'ahead' vectors

## Apply next line only to count end of turn stats

res <- sapply(res, select.nth.position)

###############################################
# criterium: persistence of winner, how many turns does she kept her winning position?
###############################################

length_persistence_winner <- sapply(res, crit_persistence_winner)
present_results(game, length_persistence_winner, show_fit=FALSE, xlab="number of last turns ahead")

###############################################
# criterium: who won the game?
###############################################

winners <- sapply(res, get_winner)
present_results(game, winners, show_fit=FALSE, xlab="player who won he game")

###############################################
# criterium: how many players were ahead during the game?
###############################################

n_aheads <- sapply(res, num_players_ahead)
present_results(game, n_aheads, show_fit=FALSE, xlab="number of different leaders")

###############################################
# what % of wins for each player?
###############################################

winners <- sapply(res, function(g) g[[length(g)]])
table(winners)/length(winners)

###############################################
###############################################
###############################################
# statistical tests et al.
###############################################
###############################################
###############################################

###############################################
# does it fit a lognormal?
###############################################

log_res <- log(res)
hist(log_res, breaks=50)  # if the sample comes from a log-normal, its log is a normal

qqnorm(log_res) # the qqplot should be a straight line
qqline(log_res, col="red")

## Use a Shapiro-Wilk test for normality in the log results:

shapiro.test(log_res) # p-value must the low

## Use a Kolmogorov-Smirnov test for log-normality

# jitter is to remove equal numbers (ks.test does not like those)
ks.test(jitter(res, amount=1e-3), 
        "plnorm", meanlog = report$lognorm_mean, sdlog = report$lognorm_sd)

##########################

library(coda)

# cut extremes
interval <- HPDinterval(as.mcmc(log_res), prob=0.999)  

log_res2 <- log_res[log_res > interval[1] & log_res < interval[2]]
shapiro.test(log_res2) # p-value > 0.05, ie, it does not reject log-normality


################

M <- 62
sim <- replicate(1e6,  which( cumsum( sample(1:6,M,replace=T) ) >= M )[1])

mean(sim)
var(sim)

hist(sim)
qqnorm(log(sim))


shapiro.test(log(sim))

################

Ms <- seq(1,63)
report <- data.frame(m = Ms, E_D = rep(NA,length(Ms)), var_D = rep(NA,length(Ms)))

par(mfrow=c(2,2))
for (i in 1:length(Ms)) {
  M <- report[i,1]

  sim <- replicate(5e4,  which( cumsum( sample(1:6, M, replace=T) ) >= M )[1])
  #hist(sim, main=paste("M = ", M), xlab="D", ylab="", yaxt="n")

  report[i,2] <- mean(sim)
  report[i,3] <- var(sim)
}

plot(report[,1], report[,2], xlab="M", ylab="E[D]", pch=20) 
fit_E <- lm(E_D ~ m, data= report)     # approx a line with slope 2/7
abline(fit_E, col="red")

plot(report[,1], report[,3], xlab="M", ylab="var(D)", pch=20) 
fit_Var <- lm(var_D ~ m, data= report) # approx a line with slope 0.06824
abline(fit_Var, col="red")

################

M <- 63
E_D <- rep(0,M+5) # extra slots to simplify next recursive expression

for (m in (M-1):1) {
  E_D[m] <- 1 + sum(1/6*E_D[m+1:6])
}

par(mfrow=c(1,1))
plot(1:M, report[,2], pch=20, type="l")
points(1:(M-1), rev(E_D[1:(M-1)]), type="l", col="red")

cbind(report[,2], rev(E_D[1:M]))
      