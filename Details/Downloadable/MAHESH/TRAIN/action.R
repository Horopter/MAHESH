f <- getwd()
script.dir <- dirname(parent.frame(2)$ofile)
setwd(script.dir)
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
colnames(m) <- c("movies","W1","lang","q1","q2","q3","q4","q5","q6","p0","p1","p2","p3","p4","p5","p6","gross")
fit1 <- glm(formula = m[, 17] ~ 0 + m[, 2] + m[, 3] + m[, 10] + m[, 4]*m[, 11])
p <- coef(summary(fit1))
p <- asNumeric(p)
est1 <- p[1]*m[,2]+ifelse(m[,3]=="E",p[2],p[3])+p[4]*m[,10]+p[5]*m[,4]+p[6]*m[,11]+p[7]*m[,4]*m[,11]
p <- cbind(t(p[1:7]),t(rep(0,12)))
write.table(p,file="../TEST/coef.csv",row.names=FALSE,col.names=FALSE,sep=",")
fit2 <- glm(formula = m[, 17] ~ 0 + m[, 2] + m[, 3] + m[, 10] + m[, 4]*m[, 11] + m[,5]*m[,12])
p <- coef(summary(fit2))
p <- asNumeric(p)
est2 <- p[1]*m[,2]+ifelse(m[,3]=="E",p[2],p[3])+p[4]*m[,10]+p[5]*m[,4]+p[6]*m[,11]+p[7]*m[,5]+p[8]*m[,12]+p[9]*m[,4]*m[,11]+p[10]*m[,5]*m[,12]
p <- cbind(t(p[1:10]),t(rep(0,9)))
write.table(p,file="../TEST/coef.csv",append=TRUE,row.names=FALSE,col.names=FALSE,sep=",")
fit3 <- glm(formula = m[, 17] ~ 0 + m[, 2] + m[, 3] + m[, 10] + m[, 4]*m[, 11] + m[,5]*m[,12]+ m[,6]*m[,13])
p <- coef(summary(fit3))
p <- asNumeric(p)
est3 <- p[1]*m[,2]+ifelse(m[,3]=="E",p[2],p[3])+p[4]*m[,10]+p[5]*m[,4]+p[6]*m[,11]+p[7]*m[,5]+p[8]*m[,12]+p[9]*m[,6]+p[10]*m[,13]+p[11]*m[,4]*m[,11]+p[12]*m[,5]*m[,12]+p[13]*m[,6]*m[,13]
p <- cbind(t(p[1:13]),t(rep(0,6)))
write.table(p,file="../TEST/coef.csv",append=TRUE,row.names=FALSE,col.names=FALSE,sep=",")
fit4 <- glm(formula = m[, 17] ~ 0 + m[, 2] + m[, 3] + m[, 10] + m[, 4]*m[, 11] + m[,5]*m[,12]+ m[,6]*m[,13]+m[,7]*m[,14])
p <- coef(summary(fit4))
p <- asNumeric(p)
est4 <- p[1]*m[,2]+ifelse(m[,3]=="E",p[2],p[3])+p[4]*m[,10]+p[5]*m[,4]+p[6]*m[,11]+p[7]*m[,5]+p[8]*m[,12]+p[9]*m[,6]+p[10]*m[,13]+p[11]*m[,7]+p[12]*m[,14]+p[13]*m[,4]*m[,11]+p[14]*m[,5]*m[,12]+p[15]*m[,6]*m[,13]+p[16]*m[,7]*m[,14]
p <- cbind(t(p[1:16]),t(rep(0,3)))
write.table(p,file="../TEST/coef.csv",append=TRUE,row.names=FALSE,col.names=FALSE,sep=",")
fit5 <- glm(formula = m[, 17] ~ 0 + m[, 2] + m[, 3] + m[, 10] + m[, 4]*m[, 11] + m[,5]*m[,12]+ m[,6]*m[,13]+m[,7]*m[,14]+m[,8]*m[,15])
p <- coef(summary(fit5))
p <- asNumeric(p)
est5 <- p[1]*m[,2]+ifelse(m[,3]=="E",p[2],p[3])+p[4]*m[,10]+p[5]*m[,4]+p[6]*m[,11]+p[7]*m[,5]+p[8]*m[,12]+p[9]*m[,6]+p[10]*m[,13]+p[11]*m[,7]+p[12]*m[,14]+p[13]*m[,8]+p[14]*m[,15]+p[15]*m[,4]*m[,11]+p[16]*m[,5]*m[,12]+p[17]*m[,6]*m[,13]+p[18]*m[,7]*m[,14]+p[19]*m[,8]*m[,15]
p <- cbind(t(p[1:19]))
write.table(p,file="../TEST/coef.csv",append=TRUE,row.names=FALSE,col.names=FALSE,sep=",")
fit6 <- glm(formula = m[, 17] ~ 0 + m[, 2] + m[, 3] + m[, 10] + m[, 4]+m[, 5] + m[,6]+m[,7]+ m[,8]+m[,10]+m[,11]+m[,12]+m[,13]+m[,14]+m[,15]+m[,9]+m[,16]+m[,9]*m[,16])
p <- coef(summary(fit6))
p <- asNumeric(p)
est6 <- p[1]*m[,2]+ifelse(m[,3]=="E",p[2],p[3])+p[4]*m[,4]+p[5]*m[,5]+p[6]*m[,6]+p[7]*m[,7]+p[8]*m[,8]+p[9]*m[,10]+p[10]*m[,11]+p[11]*m[,12]+p[12]*m[,13]+p[13]*m[,14]+p[14]*m[,15]+p[15]*m[,9]+p[16]*m[,16]+p[17]*m[,9]*m[,16]
p <- cbind(t(p[1:17]),t(rep(0,2)))
write.table(p,file="../TEST/coef.csv",append=TRUE,row.names=FALSE,col.names=FALSE,sep=",")
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
print(paste0("The correlation between actual gross amount and max_predict value is :",cor(n$adj.e.1,m2$gross)))
print(paste0("The correlation between actual gross amount and min_predict value is :",cor(n$adj.e.3,m2$gross)))
print(paste0("The correlation between actual gross amount and mean_predict value is :",cor(n$adj.e.2,m2$gross)))
write.csv(n,file="x_gross.csv",row.names=FALSE)
j <- n[,14:16]
j <- transform(j,min_err=apply(j,1,min))
k <- mean(j$min_err)
print(paste0("The minimum error is training data is : : ",k))
b <- cbind(1:nrow(n))
b1 <- cbind(b,m[,17],n[,11])
b2 <- cbind(b,m[,17],n[,12])
b3 <- cbind(b,m[,17],n[,13])
b4 <- cbind(b,n)
write.csv(b1,file="predict1.csv",row.names=FALSE)
write.csv(b2,file="predict2.csv",row.names=FALSE)
write.csv(b3,file="predict3.csv",row.names=FALSE)
fit0 <- glm(formula = m2$gross ~ m2$W1)
p <- coef(summary(fit0))
zzz <- p[1] + p[2]*m2$W1
pdf(file="first_rel.pdf")
	par(col="red")
	y <- m2$gross
	x <- m2$W1
	plot(x,y,xlab="First week")
	par(col="blue")
	y <- zzz
	x <- m2$W1
	points(x,y,xlab="First week",ylab="Gross/Estimate")
dev.off()
pdf(file="est_rel0.pdf")
	x <- b4[,1]
	g0 <- b4[,2]
	a0 <- b4[,3]
	plot(x,g0,xlab="Movie Ordinal",ylab="Earning in Cr INR")
	lines(x,g0,xlab="Movie Ordinal",ylab="Earning in Cr INR",col="red")
	lines(x,a0,xlab="Movie Ordinal",ylab="Earning in Cr INR",col="green")
dev.off()
pdf(file="est_rel1.pdf")
	x <- b4[,1]
	g0 <- b4[,2]
	b0 <- b4[,4]
	plot(x,g0,xlab="Movie Ordinal",ylab="Earning in Cr INR")
	lines(x,g0,xlab="Movie Ordinal",ylab="Earning in Cr INR",col="red")
	lines(x,b0,xlab="Movie Ordinal",ylab="Earning in Cr INR",col="green")
dev.off()
pdf(file="est_rel2.pdf")
	x <- b4[,1]
	g0 <- b4[,2]
	c0 <- b4[,5]
	plot(x,g0,xlab="Movie Ordinal",ylab="Earning in Cr INR")
	lines(x,g0,xlab="Movie Ordinal",ylab="Earning in Cr INR",col="red")
	lines(x,c0,xlab="Movie Ordinal",ylab="Earning in Cr INR",col="green")
dev.off()
pdf(file="est_rel3.pdf")
	x <- b4[,1]
	g0 <- b4[,2]
	d0 <- b4[,6]
	plot(x,g0,xlab="Movie Ordinal",ylab="Earning in Cr INR")
	lines(x,g0,xlab="Movie Ordinal",ylab="Earning in Cr INR",col="red")
	lines(x,d0,xlab="Movie Ordinal",ylab="Earning in Cr INR",col="green")
dev.off()
pdf(file="est_rel4.pdf")
	x <- b4[,1]
	g0 <- b4[,2]
	e0 <- b4[,7]
	plot(x,g0,xlab="Movie Ordinal",ylab="Earning in Cr INR")
	lines(x,g0,xlab="Movie Ordinal",ylab="Earning in Cr INR",col="red")
	lines(x,e0,xlab="Movie Ordinal",ylab="Earning in Cr INR",col="green")
dev.off()
pdf(file="predict_max_train.pdf")
	y<-b1[,2]
	x<-b1[,1]
	z<-b1[,3]
	plot(x,y,xlab="Movie Ordinal",ylab="Earning in Cr INR")
	lines(x,y,col="red")
	lines(x,z,col="green")
dev.off()
pdf(file="predict_mean_train.pdf")
	y<-b2[,2]
	x<-b2[,1]
	z<-b2[,3]
	plot(x,y,xlab="Movie Ordinal",ylab="Earning in Cr INR")
	lines(x,y,col="red")
	lines(x,z,col="green")
dev.off()
pdf(file="predict_min_train.pdf")
	y<-b3[,2]
	x<-b3[,1]
	z<-b3[,3]
	plot(x,y,xlab="Movie Ordinal",ylab="Earning in Cr INR")
	lines(x,y,col="red")
	lines(x,z,col="green")
dev.off()
setwd(f)