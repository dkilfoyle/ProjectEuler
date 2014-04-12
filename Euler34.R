# Euler project #34
# Find the sum of all numbers which are equal to the sum of the digit factorials

# get digits
getDigits = function(mynum) {
  return(as.numeric(strsplit(as.character(mynum),NULL)[[1]]))
}

# get sum of digits
getDigitFactSum = function(mynum) {
  return(sum(factorial(getDigits(mynum))))
}

# brute force approach - find as many as we can
# this will be very slow
x = sapply(1:1000000, getDigitFactSum)
which(x == 1:1000000)

# subtract 3 as 1! and 2! don't count
sum(which(x==1:1000000))-3
