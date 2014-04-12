#' Euler #1
#' 
#' Find the sum of multiples of 3 and 5 below 1000
threes = rep(c(0,0,1),1000/3)
fives = rep(c(0,0,0,0,1), 1000/5)
sum(which(threes==1)) + sum(which(fives==1)) - sum(which())
