# Take the number 192 and multiply it by each of 1, 2, and 3:
#   
# 192 × 1 = 192
# 192 × 2 = 384
# 192 × 3 = 576
# By concatenating each product we get the 1 to 9 pandigital, 192384576. 
# We will call 192384576 the concatenated product of 192 and (1,2,3)
# The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4, and 5, 
# giving the pandigital, 918273645, which is the concatenated product of 9 and (1,2,3,4,5).
# What is the largest 1 to 9 pandigital 9-digit number that can be formed as the 
# concatenated product of an integer with (1,2, ... , n) where n > 1?

isValidPandigital = function(curNum, newNum) {
  # first do newnum checks
  
  # check for 0's
  if (any(newNum == "0")) return (F)
  
  # check for repeated digits within newNum
  if (anyDuplicated(newNum)) return (F)
  
  # check for > 9 digits
  if ((length(newNum) + length(curNum)) > 9) return (F)
  
  # check for digit repeats
  if (any(newNum %in% curNum)) return (F)
  
  return(T)
  
}

# from 1 to 9 as multiple
# from 1
# keep going until 9 digit number or any digit is repeated

maxx = 987654321 / 2
x = 9
curPandigital = c()
while (x < maxx) {
  for (i in 1:9) {
    newNum = x * i
    newNumChr = strsplit(as.character(newNum),"")[[1]]
    if (isValidPandigital(curPandigital, newNumChr)) {
      curPandigital = c(curPandigital, newNumChr)
    } else {
      curPandigital = c()
      break
    }
    if (length(curPandigital)==9) {
      cat(curPandigital," at ", x, " and digits 1..", i,"\n")
      curPandigital = c()
    }
  }
  x = x + 1
}

