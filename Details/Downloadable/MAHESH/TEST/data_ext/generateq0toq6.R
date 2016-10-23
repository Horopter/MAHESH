f <- getwd()
script.dir <- dirname(parent.frame(2)$ofile)
setwd(script.dir)


library('zoo')
asNumeric <- function(x) as.numeric(as.character(x))
factorsNumeric <- function(d) modifyList(d, lapply(d[, sapply(d,is.factor)],asNumeric))
m <- read.csv(file="data3.csv",header=TRUE,sep=",")
n1 <- m[,1]
n14 <- m[,14]
m <- factorsNumeric(m)
m <- transform(m,q1=W1/T1)
m <- transform(m,q2=q1+W2/T2)
m <- transform(m,q3=q2+W3/T3)
m <- transform(m,q4=q3+W4/T4)
m <- transform(m,q5=q4+W6/T5)
m <- transform(m,q6=q5+W6/T6)
m[,16:21] <- t(na.locf(t(m[,16:21]), fromLast = FALSE, na.rm = FALSE))
m[,1] <- n1
m[,14] <- n14
p <- cbind(m[,1],m[,8],m[,14],m[,16:21],m[,15])
colnames(p)<-c("movies","W1","lang","q1","q2","q3","q4","q5","q6","gross")
write.csv(p,file="dat3.csv")
setwd(f)