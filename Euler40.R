# We shall say that an n-digit number is pandigital if it makes use of all the digits 
# 1 to n exactly once. For example, 2143 is a 4-digit pandigital and is also prime.
# What is the largest n-digit pandigital prime that exists?

# start at 987654321 - too big for a prime sieve
# candidates must end in an odd number other than 5 (1,3,7,9)
# add digits, is sum divisible by 3

isPrime <- function(x) {
  xdigs = strsplit(as.character(x),"")[[1]]
  if (xdigs[length(xdigs)] %in% c(2,4,5,6,8,0))
    return(F)

  if (sum(as.numeric(xdigs)) %% 3 == 0)
    return(F)
  
  div <- 2:floor(sqrt(x))
  !any(x %% div == 0)
}

getNextSmallest = function(a,x) {
  if (length(x) > 1) {
    # swap first 2 digits
    y = x[1]
    x[1] = x[2]
    x[2] = y
    cat(a,x,"\n")
    getNextSmallest(c(a,x[1]), x[2:length(x)])
  }
}

#getNextSmallest(c(), strsplit(as.character(54321),"")[[1]])


avail = 9:1
for (i in 1:9) {
  ans = c(avail[i], avail[-i])
  cat(ans,"\n")
  
}
