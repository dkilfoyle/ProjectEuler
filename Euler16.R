dkp = function(indent=0, ...) {
  args = list(...)
  output = ""
  for (i in 1:length(args)) {
    output = paste(output, names(args)[i]," = ",args[[i]], sep="")
    if (i != length(args)) output = paste(output, " |", sep="")
  }
  cat(paste(output,"\n",sep=""))
}

pow2 = function(exp) {
  maxdig = ceiling(exp/3)
  L = rep(0,maxdig)
  L[1] = 1
  
  for (power in 1:exp) {
    carry = 0
    for (index in 1:maxdig) {
      prod = L[index] * 2 + carry
      if (prod > 9) {
        carry = 1
        prod = prod %% 10
      }
      else
      {
        carry = 0
      }
      L[index] = prod
    }
  }
  return(L)
}

x=pow2(1000)
print(sum(x))