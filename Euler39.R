# If p is the perimeter of a right angle triangle with integral length sides, {a,b,c}, there are exactly three solutions for p = 120.
# 
# {20,48,52}, {24,45,51}, {30,40,50}
# 
# For which value of p ≤ 1000, is the number of solutions maximised?

# a^2 + b^2 = c^2
# so if a,b,c are integers then only numbers with integer square roots are candidates

aa=c()
bb=c()
cc=c()

# make a lookup table for squares
sqrlook = 1:500*1:500

for (i in 1: 499) {
  for (j in (i+1):500) {
    abc = which(sqrlook == (sqrlook[i] + sqrlook[j]))
    if (length(abc)==1) {
      cat(i,",",j,",",abc,"\n")
      cc = c(cc, abc)
      aa = c(aa,i)
      bb = c(bb,j)
    }
  }
}

perim = aa+bb+cc
which.max(table(perim))

# so perimeter^2 = 2 * c^2

