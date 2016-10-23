f <- getwd()
script.dir <- dirname(parent.frame(2)$ofile)
setwd(script.dir)


m <- read.csv(file="data1.csv",header=TRUE,sep=",")
p <- m[,1:3]
p <- transform(p,r1=as.Date(release,format="%m/%d/%Y")+7)
p <- transform(p,r2=as.Date(r1,format="%m/%d/%Y")+7)
p <- transform(p,r3=as.Date(r2,format="%m/%d/%Y")+7)
p <- transform(p,r4=as.Date(r3,format="%m/%d/%Y")+7)
p <- transform(p,r5=as.Date(r4,format="%m/%d/%Y")+7)
p <- transform(p,r6=as.Date(r5,format="%m/%d/%Y")+7)
write.csv(p,file="dat1.csv",row.names=FALSE)
m <- read.csv(file="data2.csv",header=TRUE,sep=",")
n <- m[,2:8]
d <- rowSums(n)
n$s0 <- d
n <- transform(n, p0=100*p0/s0)
n <- transform(n, p1=100*p1/s0)
n <- transform(n, p2=100*p2/s0)
n <- transform(n, p3=100*p3/s0)
n <- transform(n, p4=100*p4/s0)
n <- transform(n, p5=100*p5/s0)
n <- transform(n, p6=100*p6/s0)
n$s0 <- NULL
m <- cbind(movies=m[,1],n)
write.csv(m,file="dat2.csv",row.names=FALSE)
setwd(f)