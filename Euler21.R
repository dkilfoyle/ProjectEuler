# build a list of proper divisors (don't include n itself)
x=list()
x[1:30000]=c(1)

for (i in 2:15000) {
  j=i+i
  while (j <= 30000) {
    x[[j]]=append(x[[j]],i)
    j = j + i
  }
}

y=unlist(lapply(x,sum))

ans=c()
for (i in 1:10000) {
  if (i == y[y[i]])
    if (y[i] != y[y[i]])
    {
      cat(paste("a=",i," d(a) = ", y[i], " d(b)= ", y[y[i]], "\n"))
      ans = append(ans, y[i])
    }
}
print(ans)
print(sum(ans))