trinums = sapply(1:100, function(x) return((x/2)*(x+1)))
words = scan("words.txt", what="character")
x=strsplit(words[2],split=",")[[1]]
x=str_sub(x, 2, -2)
x[1] = "A"
y=sapply(x, function(a) { return(sum(which(LETTERS %in% strsplit(a,split="")[[1]])))})
sum(sapply(y, function(a) { return(a %in% trinums) }))