gcddk = function(num, denom) {
  # using Euclid's method
  
  quotient = num %/% denom
  remainder  = num %% denom
  
  #cat(paste(num, "\\", denom, "=", quotient, ":", remainder, "\n"))
  
  if (remainder > 0)
    return(gcddk(denom, remainder))
  else
    return(denom)
}

ans.num = c()
ans.denom = c()

for (denom in 11:99) {
  for (num in 10:(denom-1)) {
    
    numdigs = strsplit(as.character(num),NULL)[[1]]
    denomdigs = strsplit(as.character(denom),NULL)[[1]]
    shareddig = numdigs[numdigs %in% denomdigs]
    
    # if no shared digitis go to next numerator
    if (length(shareddig)==0)
      next
    
    if (shareddig=="0")
      next
    
    numdig = numdigs[numdigs != shareddig[1]]
    denomdig = denomdigs[denomdigs != shareddig[1]]
    
    if ((length(numdig) == 0) | (length(denomdig)==0)) 
      next
    
    if ((denomdig=="0") | (numdig=="0"))
      next
    
    # find greatest common divisor
    gcd = gcddk(num, denom)
    
    # simplify fraction
    a = num/gcd
    b = denom/gcd
    
    gcd = gcddk(as.numeric(numdig), as.numeric(denomdig))
    x = as.numeric(numdig) / gcd
    y = as.numeric(denomdig) / gcd
    
    # are the non-shared digits the same as the simplified fraction
    #if (grepl(a, x) & (grepl(b,denomdig)))
    if ((a==x) & (b==y))
    {
      cat("Shared: ",num, "\\", denom, "=", numdig, "\\", denomdig, "\n")
      ans.num = c(ans.num, x)
      ans.denom = c(ans.denom, y)
    }
  }
}

x = prod(ans.num)
y = prod(ans.denom)
gcd = gcddk(x, y)

# simplify fraction
aa = x/gcd
bb = y/gcd
