# a^b for a=2:100 b=2:100
ans = c()
for (a in 2:100)
  for (b in 2:100)
    ans = append(ans, a^b)