#################
# make simulation assuming uniform probability of winning for all players

exec.sim <- function(players, throws) {
  sim    <- sample(1:players, throws, replace=TRUE)
  events <- table(sim)
  max(events) / min(events)  
}

#################
# compute how surprising is this result
# low  percentile suggests the result is       caused by uniform probability of winning
# high percentile suggests the result is *not* caused by uniform probability of winning

run <- function(my.value, throws=5000, n=1000, players=6) {
  sims <- replicate(n, exec.sim(players, throws))
  percentil <- round(sum(sims<my.value)/n,2)
  
  hist(sims, breaks=25, prob=T, col="grey80", yaxt='n', xlab="", ylab="", 
       main=paste("Your result is at percentil", percentil))
  abline(v=my.value, col="blue", lty=2, lwd=2)
  
  percentil
}

