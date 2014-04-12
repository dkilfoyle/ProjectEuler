getdigits = function(x) {
  as.numeric(unlist(strsplit(as.character(x),"")))
}

ans=c()

for (i in 2:10000) {
  j = 2
  x = i * j
  a = getdigits(i)
  b = getdigits(j)
  c = getdigits(x)
  
  while (length(a) + length(b) + length(c) < 10) {
    a = getdigits(i)
    b = getdigits(j)
    c = getdigits(x)
    d = sort(c(a,b,c))
    if ((length(d) == 9) & (any(d != 1:9)==F)) {
      ans = append(ans, (x))
      cat("i=",i," j=",j," prod =", x,"\n")
    }
    j=j+1
    x = i*j
  }
}

print(sum(unique(ans)))