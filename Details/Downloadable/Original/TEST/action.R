rm(list=ls())
cat('\014')
asNumeric <- function(x) as.numeric(as.character(x))
factorsNumeric <- function(d) modifyList(d, lapply(d[, sapply(d,is.factor)],asNumeric))
setwd('trends')
source('generatep0top6.R')
setwd('..')
setwd('data_ext')
source('generateq0toq6.R')
setwd('..')
m1 <- read.csv(file="trends/dat2.csv",header=TRUE,sep=",")
m2 <- read.csv(file="data_ext/dat3.csv",header=TRUE,sep=",")
m <- cbind.data.frame(m1$movies,m2$W1,m2$lang,m2$q1,m2$q2,m2$q3,m2$q4,m2$q5,m2$q6,m1$p0,m1$p1,m1$p2,m1$p3,m1$p4,m1$p5,m1$p6,m2$gross)
colnames(m) <- c("movies","W1","lang","q1","m$gross","q3","q4","q5","q6","p0","p1","p2","p3","p4","p5","p6","gross")
mp <- read.csv(file="coef.csv",sep=",",header=FALSE)
p <- mp[1,]
p <- asNumeric(p)
est1 <- p[1]*m[,2]+ifelse(m[,3]=="E",p[2],p[3])+p[4]*m[,10]+p[5]*m[,4]+p[6]*m[,11]+p[7]*m[,4]*m[,11]
p <- mp[2,]
p <- asNumeric(p)
est2 <- p[1]*m[,2]+ifelse(m[,3]=="E",p[2],p[3])+p[4]*m[,10]+p[5]*m[,4]+p[6]*m[,11]+p[7]*m[,5]+p[8]*m[,12]+p[9]*m[,4]*m[,11]+p[10]*m[,5]*m[,12]
p <- mp[3,]
p <- asNumeric(p)
est3 <- p[1]*m[,2]+ifelse(m[,3]=="E",p[2],p[3])+p[4]*m[,10]+p[5]*m[,4]+p[6]*m[,11]+p[7]*m[,5]+p[8]*m[,12]+p[9]*m[,6]+p[10]*m[,13]+p[11]*m[,4]*m[,11]+p[12]*m[,5]*m[,12]+p[13]*m[,6]*m[,13]
p <- mp[4,]
p <- asNumeric(p)
est4 <- p[1]*m[,2]+ifelse(m[,3]=="E",p[2],p[3])+p[4]*m[,10]+p[5]*m[,4]+p[6]*m[,11]+p[7]*m[,5]+p[8]*m[,12]+p[9]*m[,6]+p[10]*m[,13]+p[11]*m[,7]+p[12]*m[,14]+p[13]*m[,4]*m[,11]+p[14]*m[,5]*m[,12]+p[15]*m[,6]*m[,13]+p[16]*m[,7]*m[,14]
p <- mp[5,]
p <- asNumeric(p)
est5 <- p[1]*m[,2]+ifelse(m[,3]=="E",p[2],p[3])+p[4]*m[,10]+p[5]*m[,4]+p[6]*m[,11]+p[7]*m[,5]+p[8]*m[,12]+p[9]*m[,6]+p[10]*m[,13]+p[11]*m[,7]+p[12]*m[,14]+p[13]*m[,8]+p[14]*m[,15]+p[15]*m[,4]*m[,11]+p[16]*m[,5]*m[,12]+p[17]*m[,6]*m[,13]+p[18]*m[,7]*m[,14]+p[19]*m[,8]*m[,15]
p <- mp[6,]
p <- asNumeric(p)
est6 <- p[1]*m[,2]+ifelse(m[,3]=="E",p[2],p[3])+p[4]*m[,4]+p[5]*m[,5]+p[6]*m[,6]+p[7]*m[,7]+p[8]*m[,8]+p[9]*m[,10]+p[10]*m[,11]+p[11]*m[,12]+p[12]*m[,13]+p[13]*m[,14]+p[14]*m[,15]+p[15]*m[,9]+p[16]*m[,16]+p[17]*m[,9]*m[,16]
n <- cbind(m[,17],est1,est2,est3,est4,est5,est6)
n <- transform(n,maxx=apply(n[,2:6],1,max))
n <- transform(n,meanx=apply(n[,2:6],1,mean))
n <- transform(n,minx=apply(n[,2:6],1,min))
n <- transform(n,adj.e.1=ifelse(n[,2]<m[,2],m[,2],n$maxx))
n <- transform(n,adj.e.2=ifelse(n[,2]<m[,2],m[,2],n$meanx))
n <- transform(n,adj.e.3=ifelse(n[,2]<m[,2],m[,2],n$minx))
n <- transform(n,error1=((abs(n$adj.e.1-m$gross)/n$adj.e.1)*100))
n <- transform(n,error2=((abs(n$adj.e.2-m$gross)/n$adj.e.2)*100))
n <- transform(n,error3=((abs(n$adj.e.3-m$gross)/n$adj.e.3)*100))
write.csv(n,file="x_gross.csv",row.names=FALSE)
j <- n[,14:16]
j <- transform(j,min_err=apply(j,1,min))
k <- mean(j$min_err)
print(paste0("The minimum error is : : ",k))
b <- cbind(1:nrow(n))
b1 <- cbind(b,m[,17],n[,11])
b2 <- cbind(b,m[,17],n[,12])
b3 <- cbind(b,m[,17],n[,13])
write.csv(b1,file="predict1.csv",row.names=FALSE)
write.csv(b2,file="predict2.csv",row.names=FALSE)
write.csv(b3,file="predict3.csv",row.names=FALSE)
pdf(file="predict_max.pdf")
	y<-b1[,2]
	x<-b1[,1]
	z<-b1[,3]
	plot(x,y)
	lines(x,y,col="red")
	lines(x,z,col="green")
dev.off()
pdf(file="predict_mean.pdf")
	y<-b2[,2]
	x<-b2[,1]
	z<-b2[,3]
	plot(x,y)
	lines(x,y,col="red")
	lines(x,z,col="green")
dev.off()
pdf(file="predict_min.pdf")
	y<-b3[,2]
	x<-b3[,1]
	z<-b3[,3]
	plot(x,y)
	lines(x,y,col="red")
	lines(x,z,col="green")
dev.off()

	