# Find all primes up to n
prime_sieve = function(n) {
  primes = rep(T,n)
  primes[1] = F # by convention 1 is not prime
  cur.prime = 2
  fsqrtn = floor(sqrt(n))
  while (cur.prime <= fsqrtn) {
    # cross out all multiples of this prime ie 2*i, 3*i, 4*i
    primes[seq(from=(2*cur.prime), to=n, by=cur.prime)] = F
    # starting at the next number find the next T entry 
    sel = which(primes[(cur.prime+1):(fsqrtn+1)])
    if (any(sel)) {
      cur.prime = cur.prime + min(sel)
    }
    else cur.prime = fsqrtn+1
  }
  return(primes)
}

getPrimesFromSieve = function(n) {
  primes = prime_sieve(n)
  which(primes)
}

trial_division = function(n) {
  if (n==1) return(c(1))
  primes = prime_sieve(floor(sqrt(n))+1)
  #cat("Primes up to sqrt(n) are:", primes)
  pfactors = c()
  
  for (p in primes) {
    if ((p*p) > n) break # there cant be any bigger p factors now
    while ((n %% p) == 0) {
      # p is a prime factor, so find out how many times
      pfactors = append(pfactors, p)
      n = n / p
      #cat("found",p,"new n is",n,"\n")
    }
  }
  if (n > 1) {
    # no more prime factors, whatever is left must be prime
    pfactors = append(pfactors,n)
  }
  return(pfactors)
}

trial.division.n = function(n) {
  if (n==1) return(c(1))
  primes = prime_sieve(floor(sqrt(n))+1)
  pfactors = c()
  pcounts = c()
  
  for (p in primes) {
    if ((p*p) > n) break # there cant be any bigger p factors now
    i = 0;
    while ((n %% p) == 0) {
      # p is a prime factor, so find out how many times
      if (i == 0) pfactors = append(pfactors, p)
      n = n / p
      i = i + 1
    }
    if (i > 0) pcounts = append(pcounts, i)
  }
  if (n > 1) {
    # no more prime factors, whatever is left must be prime
    pfactors = append(pfactors,n)
    pcounts = append(pcounts,1)
  }
  return(list(factors=pfactors,counts=pcounts))
}

count.divisors = function(n) {
  x = trial.division.n(n)$counts
  prod(x+1)
}

isPrime <- function(x){
  div <- 2:floor(sqrt(x))
  !any(x %% div == 0)
}

