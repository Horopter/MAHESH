f <- getwd()
script.dir <- dirname(parent.frame(2)$ofile)
setwd(script.dir)
 #install.packages('pracma')
 library('splines')
 trim <- function (x) gsub("^\\s+|\\s+$", "", x)
 picture <- function(a)
 {
 print(a)
 m <- read.csv(file.path(".","MovieTrends-long",trim(a),"7.csv"),sep=",")
 print(m)
 m <- subset(m,m[,3]!=0)
 m <- cbind(m,0:(nrow(m)-1))
 m <- cbind(m,(m[,3]/(nrow(m)-1))*100)
 mypath <- file.path(".","MoviePlots",paste("myplot_",trim(a),".pdf", sep = ""))
 pdf(file=mypath,width = 80, height=30)
    mytitle = paste("Title : ",a)
	print(m)
	x <- m[,4]
	y <- m[,5]
	plot(x,y,main=a,xlab="Time",ylab="Normalized impact value",cex.lab=1)
	lines(x,y,main=a,xlab="Time",ylab="Normalized impact value",cex.lab=1)
 dev.off()
 }
 plot_all1 = function(a,g)
 {
	mypath <- file.path(".","MoviePlots",paste("FINALSCATTER1",".pdf", sep = ""))
	pdf(file=mypath,width = 80, height=80)
    mytitle = paste("Title : ","FINAL")
	n <- data.frame()
	for(i in 1:g)
	{
		b <- a[i,1]
		m <- read.csv(file.path(".","MovieTrends-long",trim(b),"7.csv"),sep=",")
		m <- subset(m, m[,3]!=0)
		m <- cbind(m,0:(nrow(m)-1))
		m <- cbind(m,(m[,3]/(nrow(m)-1))*10000)
		#colnames(m)<- c("start_date","end_date","values","serial","normalized")
		n <- rbind(n,m[,c(4,5)])
	}
	n<-n[with(n, order(n[,2])), ]
	x <- n[,2]
	y <- n[,1]
	z <- x
	plot(x,y,main=a)
	fit1<-lm(y ~ x + I(sin(x)) + I(cos(x))+ I(sin(2*x)) + I(cos(2*x)))
	fit2 <- lm( y~ns(x, 2) )
	fit3 <- lm( y~ns(x, 3) )
	fit4 <- lm( y~ns(x, 4) )
	fit5 <- lm( y~ns(x, 5) )
	fit6 <- lm( y~ns(x, 6) )
	fit7 <- lm( y~ns(x, 7) )
	fit8 <- lm( y~ns(x, 8) )
	fit9 <- lm( y~ns(x, 9) )
	fit10 <- lm(y ~ x + cos(x*pi))
	lines(x,predict(fit1, data.frame(x=z)), col='violet')
	lines(x,predict(fit2, data.frame(x=z)), col='cyan')
	lines(x,predict(fit3, data.frame(x=z)), col='green')
	lines(x,predict(fit4, data.frame(x=z)), col='yellow')
	lines(x,predict(fit5, data.frame(x=z)), col='orange')
	lines(x,predict(fit6, data.frame(x=z)), col='blue')
	lines(x,predict(fit7, data.frame(x=z)), col='red')
	lines(x,predict(fit8, data.frame(x=z)), col='black')
	lines(x,predict(fit9, data.frame(x=z)), col='brown')
	lines(x,predict(fit10, data.frame(x=z)), col='magenta')
	dev.off()
 }
 plot_all2 = function(a,g)
 {
	mypath <- file.path(".","MoviePlots",paste("FINALSCATTER2",".pdf", sep = ""))
	pdf(file=mypath,width = 80, height=80)
    mytitle = paste("Title : ","FINAL")
	n <- data.frame()
	for(i in 1:g)
	{
		b <- a[i,1]
		m <- read.csv(file.path(".","MovieTrends-long",trim(b),"7.csv"),sep=",")
		m <- subset(m, m[,3]!=0)
		m <- cbind(m,0:(nrow(m)-1))
		m <- cbind(m,(m[,3]/(nrow(m)-1))*10000)
		#colnames(m)<- c("start_date","end_date","values","serial","normalized")
		n <- rbind(n,m[,c(4,5)])
	}
	n<-n[with(n, order(n[,2])), ]
	x <- n[,2]
	y <- n[,1]
	z <- x
	plot(x,y)
	lines(x,y)
	fit1<-lm(y ~ x + I(sin(x)) + I(cos(x))+ I(sin(2*x)) + I(cos(2*x)))
	fit2 <- lm( y~ns(x, 2) )
	print(fit2)
	fit3 <- lm( y~ns(x, 3) )
	print(fit3)
	fit4 <- lm( y~ns(x, 4) )
	print(fit4)
	fit5 <- lm( y~ns(x, 5) )
	print(fit5)
	fit6 <- lm( y~ns(x, 6) )
	print(fit6)
	fit7 <- lm( y~ns(x, 7) )
	print(fit7)
	fit8 <- lm( y~ns(x, 8) )
	print(fit8)
	fit9 <- lm( y~ns(x, 9) )
	print(fit9)
	fit10 <- lm( x~ns(y, 9))
	lines(x,predict(fit1, data.frame(x=z)), col='violet')
	lines(x,predict(fit2, data.frame(x=z)), col='cyan')
	lines(x,predict(fit3, data.frame(x=z)), col='green')
	lines(x,predict(fit4, data.frame(x=z)), col='yellow')
	lines(x,predict(fit5, data.frame(x=z)), col='orange')
	lines(x,predict(fit6, data.frame(x=z)), col='blue')
	lines(x,predict(fit7, data.frame(x=z)), col='red')
	lines(x,predict(fit8, data.frame(x=z)), col='black')
	lines(x,predict(fit9, data.frame(x=z)), col='brown')
	lines(x,predict(fit10, data.frame(x=z)), col='grey')
	dev.off()
 }
 take_pic = function(g)
{
dir.create(file.path(".","MoviePlots"), showWarnings = FALSE,recursive=TRUE)
vinod <- read.csv(file="dat1.csv",header=TRUE,sep=",")
vin <- vinod
i <- sapply(vinod, is.factor)
vin[i] <- lapply(vinod[i], as.character)
for(i in 1:g)
{
picture(vin[i,1])
}
}
pic_final = function(g)
{
dir.create(file.path(".","MoviePlots"), showWarnings = FALSE,recursive=TRUE)
vinod <- read.csv(file="dat1.csv",header=TRUE,sep=",")
vin <- vinod
i <- sapply(vinod, is.factor)
vin[i] <- lapply(vinod[i], as.character)
tryCatch({
	plot_all1(vin,g)
	plot_all2(vin,g)
	},warning = function(war) {print(war)}
	,error = function(err) {print(err)}
	)
}

finalize = function(g)
{
	tryCatch({
	take_pic(g)
	},warning = function(war) {print(war)}
	,error = function(err) {print(err)}
	)
	tryCatch({
	pic_final(g)
	},warning = function(war) {print(war)}
	,error = function(err) {print(err)}
	)
}
#finalize(1)
finalize(length(list.files(path=file.path(".","MovieTrends"))))
setwd(f)