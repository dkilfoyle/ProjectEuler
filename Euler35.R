# Euler Project # 35
# How many circular primes are there below one million?

source("UtilsPrime.R")
primeSieve <- prime_sieve(1000000)

# get digits
getDigits = function(mynum) {
  return(as.numeric(strsplit(as.character(mynum),NULL)[[1]]))
}

# get rotations
getRotations = function(xx) {
  y=xx
  x=getDigits(xx)
  # enumerate along possible starting positions
  for (i in 2:length(x)) {
    newx = c(x[i:length(x)], x[1:(i-1)])
    y = c(y, as.numeric(paste(newx,collapse="")))
  }
  y
}

# isPrime = function(x) {
#   if (primeSieve[x]==T)
#     return(T)
#   else
#     return(F)
# }

primeRotTest = function(x) {
  if (all(sapply(getRotations(x), function(xx) primeSieve[xx]))) {
    cat("Found ",x,"\n")
    return(T)
  }
  return(F)
  
}

x=which(sapply(which(primeSieve)[-(1:4)], primeRotTest))

# the number of primes is
length(x)+4