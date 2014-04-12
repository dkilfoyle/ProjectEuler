source("utils.r")

factorize = function(x) {
  x = as.integer(x)
  div = seq_len(abs(x))
  factors = div[x %% div == 0L]
  factors
}

prob12 = function() {
  cur.tri = 1L
  i = 2L
  while (T) {
    cur.tri = cur.tri + i
    ans = count.divisors(cur.tri)
    cat(paste("i:",i,"  cur.tri:", cur.tri, "  ans:", ans,"\n"))
    if (ans > 500) break()
    i = i + 1
  }
  cur.tri
}