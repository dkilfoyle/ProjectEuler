digit.sum = function(x) {
  xx = as.numeric(strsplit(as.character(x),"")[[1]])
  sum(xx)
}

library(gmp)
x=factorialZ(100)
digit.sum(x)

multiply.vectornum = function(vn, multiple) {
  carry = 0
  for (d in 1:length(vn)) {
    c = carry # c = any carries left over from last number
    carry = 0
    t = vn[d] * multiple
    while (t >= 10) {
      t = t - 10
      carry = carry + 1
    }
    # there are carry 10s in original t with t left over
    # now bring in c from last carry
    s = t + c
    while ( s >= 10) {
      s = s - 10
      carry = carry + 1
    }
    vn[d] = s
  }
  if (carry > 0) {
    cc = 0
    while (carry > 0) {
      while (carry >= 10) {
        carry = carry - 10
        cc = cc + 1
      }
      vn=append(vn, carry)
      carry = cc
      cc = 0
    }
  }
  vn
}

multiply.vectornum2 = function(vn, multiple) {
  carry = 0
  for (d in 1:length(vn)) {
    c = carry # c = any carries left over from last number
    carry = 0
    t = vn[d] * multiple
    carry = floor(t / 10)
    t = t %% 10
    # there are carry 10s in original t with t left over
    # now bring in c from last carry
    s = t + c
    carry = carry + floor(s / 10)
    s = s %% 10
    vn[d] = s
  }
  if (carry > 0) {
    cc = 0
    while (carry > 0) {
      cc = floor(carry / 10)
      carry = carry %% 10
      vn=append(vn, carry)
      carry = cc
      cc = 0
    }
  }
  vn
}

st = proc.time()
x=c(1)
for (i in 1:100) {
  x= multiply.vectornum(x, i)
  #print(sprintf("%i : %s", i, paste(as.character(x), collapse="")))
}
sum(x)
print(proc.time() - st)

st = proc.time()
x=c(1)
for (i in 1:100) {
  x= multiply.vectornum2(x, i)
  #print(sprintf("%i : %s", i, paste(as.character(x), collapse="")))
}
sum(x)
print(proc.time() - st)

sum(x)