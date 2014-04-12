x=names(read.csv("names.txt",na.strings="")) # all on 1 line so it makes them columns, not rows
x=sort(x)
x[3627]="NA"

getScore = function(mystr) {
  return (sum(match(unlist(strsplit(mystr,"")),LETTERS)))
}

y=sapply(x, getScore, USE.NAMES=FALSE)
z=y * 1:length(y)
library(gmp)
zz=as.bigz(z)
print(sum(zz))