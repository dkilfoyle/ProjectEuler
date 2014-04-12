collatz = function(n) {
  i = 0
  orig.n = n
  setWinProgressBar(pb, n)
  while (n > 1) {
    #print(n)
    if (n < 1000000)
      if (collatz.cache[n] != 0)
      {
        #print(paste("cache",n,"\n"))
        collatz.cache[orig.n] <<- i + collatz.cache[n] - 1
        return(i+collatz.cache[n]-1)
      }  
    n=ifelse(n %% 2 == 0, n/2, 3*n+1)
    i=i+1
  }
  i=i+1 # n==1
  collatz.cache[orig.n] <<- i
  return(i)
}

pb <<- winProgressBar(min=0, max=1000000)
collatz.cache=rep(0,1000000)
for (a in 2:1000000) collatz(a)
print(which.max(collatz.cache))
close(pb)
choose