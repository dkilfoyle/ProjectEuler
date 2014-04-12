# Num 37 truncatable primes

# Can only have digits 1,3,7,9

source("UtilsPrime.r")

# find primes that are formed from only 1,3,7,9
primes = getPrimesFromSieve(100000000)[-(1:4)]

prime.sieve = prime_sieve(100000000)
prime.sieve

# get digits
getDigits = function(mynum) {
  return(as.numeric(strsplit(as.character(mynum),NULL)[[1]]))
}

onlyDigits = function(x, allowed.digits) {
  mydig = getDigits(x)
  return(all(mydig %in% allowed.digits))
}

only1379 = primes[which(sapply(primes, onlyDigits, c(1,3,7,9)))]

truncateTest = function(x) {
  mydig = getDigits(x)
  for (i in 1:length(mydig)) {
    if (prime.sieve[as.numeric(paste(mydig[i:length(mydig)],collapse=""))] == F) return(F)
  }
  for (i in length(mydig):1) {
    if (prime.sieve[as.numeric(paste(mydig[1:i],collapse=""))]==F) return(F)
  }
  cat("Found ", x,"\n")
  return(T)
}

ans = only1379[sapply(only1379, truncateTest)]
cat(ans)