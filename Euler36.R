# find all numbers below 1 million that are palindromic in decimal and binary

# dec to bin
dec2bin = function(dec) {
  x = dec
  bin = c()
  while (x >= 1) {
    remainder = x %% 2
    x = x %/% 2
    bin = c(remainder, bin)
  }
  bin
}

strReverse <- function(x)  sapply(lapply(strsplit(x, NULL), rev), paste, collapse = "")

isDoublePalindrome = function(x) {
  # get decimal digits
  xdec = strsplit(as.character(x),NULL)[[1]]
  if (!all(xdec == rev(xdec)))
    return(F)
  
  # get binary digits
  xbin = dec2bin(x)
  if (!all(xbin == rev(xbin))) 
    return(F)
  
  cat("Found ", x, "\n")
  
  return(T)
}

mytest = function() sum(which(sapply(1:1000000, isDoublePalindrome)))