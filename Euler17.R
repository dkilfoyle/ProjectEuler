ones = c("One","Two","Three","Four","Five","Six","Seven","Eight","Nine")
tens = c("Twenty","Thirty","Forty","Fifty","Sixty","Seventy","Eighty","Ninety")
teens = c("Eleven","Twelve","Thirteen","Fourteen","Fifteen","Sixteen","Seventeen","Eighteen","Nineteen")

numtxt = c(ones, "Ten", teens, paste(rep(tens,each=10),rep(c("",ones), 8)))
numtxt2 = gsub(" ","",numtxt)

ans = c(numtxt, 
        paste(rep(ones,each=100),"hundred", c("", rep("and",99)), c("", numtxt))
)
ans = gsub(" ","", ans)
print(sum(nchar(ans))+nchar("onethousand"))


# 1 to 99
x = sum(nchar(numtxt2))*10

# one hundred, one hundred and two hundred 
y = sum(nchar(ones))*100 + (nchar("hundred")*900)

# and
z = nchar("and")*99*9

print(x+y+z)