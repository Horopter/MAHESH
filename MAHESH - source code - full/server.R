library(shiny)
library(zoo)
shinyServer(function(input, output,session) {
  asNumeric <- function(x) as.numeric(as.character(x))
  factorsNumeric <- function(d) modifyList(d, lapply(d[, sapply(d,is.factor)],asNumeric))
  fun1 <- function(m)
  {
    p <- m[,1:3]
    p <- transform(p,R1=as.Date(release,format="%m/%d/%Y")+7)
    p <- transform(p,R2=as.Date(R1,format="%m/%d/%Y")+7)
    p <- transform(p,R3=as.Date(R2,format="%m/%d/%Y")+7)
    p <- transform(p,R4=as.Date(R3,format="%m/%d/%Y")+7)
    p <- transform(p,R5=as.Date(R4,format="%m/%d/%Y")+7)
    p <- transform(p,R6=as.Date(R5,format="%m/%d/%Y")+7)
    p <- transform(p,r1=as.character(R1))
    p <- transform(p,r2=as.character(R2))
    p <- transform(p,r3=as.character(R3))
    p <- transform(p,r4=as.character(R4))
    p <- transform(p,r5=as.character(R5))
    p <- transform(p,r6=as.character(R6))
    p <- p[,c(1:3,10:15)]
    return(p)
  }
  fun2 <- function(m)
  {
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
    return(m)
  }
  fun3 <- function(m)
  {
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
    return(p)
  }
  fun4 <- function(m,est1,est2,est3,est4,est5,est6)
  {
    n <- cbind.data.frame(gross=m[,17],est1,est2,est3,est4,est5,est6)
    n <- transform(n,maxx=apply(n[,2:6],1,max))
    n <- transform(n,meanx=apply(n[,2:6],1,mean))
    n <- transform(n,minx=apply(n[,2:6],1,min))
    n <- transform(n,adj.e.1=ifelse(n[,2]<m[,2],m[,2],n$maxx))
    n <- transform(n,adj.e.2=ifelse(n[,2]<m[,2],m[,2],n$meanx))
    n <- transform(n,adj.e.3=ifelse(n[,2]<m[,2],m[,2],n$minx))
    n <- transform(n,error1=((abs(n$adj.e.1-m$gross)/n$adj.e.1)*100))
    n <- transform(n,error2=((abs(n$adj.e.2-m$gross)/n$adj.e.2)*100))
    n <- transform(n,error3=((abs(n$adj.e.3-m$gross)/n$adj.e.3)*100))
    return(n)
  }
  fun5 <- function(m,flag)
  {
    fit1 <- glm(formula = m[, 17] ~ 0 + m[, 2] + m[, 3] + m[, 10] + m[, 4]*m[, 11])
    p <- coef(summary(fit1))
    p <- asNumeric(p)
    est1 <- p[1]*m[,2]+ifelse(m[,3]=="E",p[2],p[3])+p[4]*m[,10]+p[5]*m[,4]+p[6]*m[,11]+p[7]*m[,4]*m[,11]
    p <- cbind(t(p[1:7]),t(rep(0,12)))
    d1 <- p
    fit2 <- glm(formula = m[, 17] ~ 0 + m[, 2] + m[, 3] + m[, 10] + m[, 4]*m[, 11] + m[,5]*m[,12])
    p <- coef(summary(fit2))
    p <- asNumeric(p)
    est2 <- p[1]*m[,2]+ifelse(m[,3]=="E",p[2],p[3])+p[4]*m[,10]+p[5]*m[,4]+p[6]*m[,11]+p[7]*m[,5]+p[8]*m[,12]+p[9]*m[,4]*m[,11]+p[10]*m[,5]*m[,12]
    p <- cbind(t(p[1:10]),t(rep(0,9)))
    d2<-p
    fit3 <- glm(formula = m[, 17] ~ 0 + m[, 2] + m[, 3] + m[, 10] + m[, 4]*m[, 11] + m[,5]*m[,12]+ m[,6]*m[,13])
    p <- coef(summary(fit3))
    p <- asNumeric(p)
    est3 <- p[1]*m[,2]+ifelse(m[,3]=="E",p[2],p[3])+p[4]*m[,10]+p[5]*m[,4]+p[6]*m[,11]+p[7]*m[,5]+p[8]*m[,12]+p[9]*m[,6]+p[10]*m[,13]+p[11]*m[,4]*m[,11]+p[12]*m[,5]*m[,12]+p[13]*m[,6]*m[,13]
    p <- cbind(t(p[1:13]),t(rep(0,6)))
    d3 <- p
    fit4 <- glm(formula = m[, 17] ~ 0 + m[, 2] + m[, 3] + m[, 10] + m[, 4]*m[, 11] + m[,5]*m[,12]+ m[,6]*m[,13]+m[,7]*m[,14])
    p <- coef(summary(fit4))
    p <- asNumeric(p)
    est4 <- p[1]*m[,2]+ifelse(m[,3]=="E",p[2],p[3])+p[4]*m[,10]+p[5]*m[,4]+p[6]*m[,11]+p[7]*m[,5]+p[8]*m[,12]+p[9]*m[,6]+p[10]*m[,13]+p[11]*m[,7]+p[12]*m[,14]+p[13]*m[,4]*m[,11]+p[14]*m[,5]*m[,12]+p[15]*m[,6]*m[,13]+p[16]*m[,7]*m[,14]
    p <- cbind(t(p[1:16]),t(rep(0,3)))
    d4 <- p
    fit5 <- glm(formula = m[, 17] ~ 0 + m[, 2] + m[, 3] + m[, 10] + m[, 4]*m[, 11] + m[,5]*m[,12]+ m[,6]*m[,13]+m[,7]*m[,14]+m[,8]*m[,15])
    p <- coef(summary(fit5))
    p <- asNumeric(p)
    est5 <- p[1]*m[,2]+ifelse(m[,3]=="E",p[2],p[3])+p[4]*m[,10]+p[5]*m[,4]+p[6]*m[,11]+p[7]*m[,5]+p[8]*m[,12]+p[9]*m[,6]+p[10]*m[,13]+p[11]*m[,7]+p[12]*m[,14]+p[13]*m[,8]+p[14]*m[,15]+p[15]*m[,4]*m[,11]+p[16]*m[,5]*m[,12]+p[17]*m[,6]*m[,13]+p[18]*m[,7]*m[,14]+p[19]*m[,8]*m[,15]
    p <- cbind(t(p[1:19]))
    d5 <- p
    fit6 <- glm(formula = m[, 17] ~ 0 + m[, 2] + m[, 3] + m[, 10] + m[, 4]+m[, 5] + m[,6]+m[,7]+ m[,8]+m[,10]+m[,11]+m[,12]+m[,13]+m[,14]+m[,15]+m[,9]+m[,16]+m[,9]*m[,16])
    p <- coef(summary(fit6))
    p <- asNumeric(p)
    est6 <- p[1]*m[,2]+ifelse(m[,3]=="E",p[2],p[3])+p[4]*m[,4]+p[5]*m[,5]+p[6]*m[,6]+p[7]*m[,7]+p[8]*m[,8]+p[9]*m[,10]+p[10]*m[,11]+p[11]*m[,12]+p[12]*m[,13]+p[13]*m[,14]+p[14]*m[,15]+p[15]*m[,9]+p[16]*m[,16]+p[17]*m[,9]*m[,16]
    p <- cbind(t(p[1:17]),t(rep(0,2)))
    d6 <- p
    j <- cbind.data.frame(est1=est1,est2=est2,est3=est3,est4=est4,est5=est5,est6=est6)
    if(flag==1)
      return(j)
    else if(flag==0)
    {
      j <- rbind(d1,d2,d3,d4,d5,d6)
      return(j)
    }
  }
  fun6 <- function(mp,m)
  {
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
    j <- cbind.data.frame(est1=est1,est2=est2,est3=est3,est4=est4,est5=est5,est6=est6)
    return(j)
  }
  output$Dates <- renderTable({
    inFile <- input$data1
    if (is.null(inFile))
      return(NULL)
    read.csv(inFile$datapath, header=input$header, sep=input$sep)
  })
  output$Opinion <- renderTable({
    inFile <- input$data2
    if (is.null(inFile))
      return(NULL)
    read.csv(inFile$datapath, header=input$header, sep=input$sep)
  })
  output$Economics <- renderTable({
    inFile <- input$data3
    if (is.null(inFile))
      return(NULL)
    read.csv(inFile$datapath, header=input$header, sep=input$sep)
  })
  output$dat1 <- renderTable({
    inFile <- input$data1
    if (is.null(inFile))
      return(NULL)
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p1 <- fun1(m)
    assign('p1',p1,envir=.GlobalEnv)
    
  })
  output$dat2 <- renderTable({
    inFile <- input$data2
    if (is.null(inFile))
      return(NULL)
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p2 <- fun2(m)
    assign('p2',p2,envir=.GlobalEnv)
    
  })
  output$dat3 <- renderTable({
    inFile <- input$data3
    if (is.null(inFile))
      return(NULL)
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p3 <- fun3(m)
    assign('p3',p3,envir=.GlobalEnv)
  })
  output$result <- renderTable({
    inFile <- input$data2
    if (is.null(inFile))
      return(NULL)
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p2 <- fun2(m)
    inFile <- input$data3
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p3 <- fun3(m)
    m1 <- p2
    m2 <- p3
    m <- cbind.data.frame(m1$movies,m2$W1,m2$lang,m2$q1,m2$q2,m2$q3,m2$q4,m2$q5,m2$q6,m1$p0,m1$p1,m1$p2,m1$p3,m1$p4,m1$p5,m1$p6,m2$gross)
    colnames(m) <- c("movies","W1","lang","q1","q2","q3","q4","q5","q6","p0","p1","p2","p3","p4","p5","p6","gross")
    j <- fun5(m,1)
    fun4(m,j[,1],j[,2],j[,3],j[,4],j[,5],j[,6])
  })
  output$coeff <- renderTable({
    inFile <- input$data2
    if (is.null(inFile))
      return(NULL)
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p2 <- fun2(m)
    inFile <- input$data3
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p3 <- fun3(m)
    m1 <- p2
    m2 <- p3
    m <- cbind.data.frame(m1$movies,m2$W1,m2$lang,m2$q1,m2$q2,m2$q3,m2$q4,m2$q5,m2$q6,m1$p0,m1$p1,m1$p2,m1$p3,m1$p4,m1$p5,m1$p6,m2$gross)
    colnames(m) <- c("movies","W1","lang","q1","q2","q3","q4","q5","q6","p0","p1","p2","p3","p4","p5","p6","gross")
    j <- fun5(m,0)
  })
  output$Dates2 <- renderTable({
    inFile <- input$data4
    if (is.null(inFile))
      return(NULL)
    read.csv(inFile$datapath, header=input$header, sep=input$sep)
  })
  output$Opinion2 <- renderTable({
    inFile <- input$data5
    if (is.null(inFile))
      return(NULL)
    read.csv(inFile$datapath, header=input$header, sep=input$sep)
  })
  output$Economics2 <- renderTable({
    inFile <- input$data6
    if (is.null(inFile))
      return(NULL)
    read.csv(inFile$datapath, header=input$header, sep=input$sep)
  })
  output$dat4 <- renderTable({
    inFile <- input$data4
    if (is.null(inFile))
      return(NULL)
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p1 <- fun1(m)
    
  })
  output$dat5 <- renderTable({
    inFile <- input$data5
    if (is.null(inFile))
      return(NULL)
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p2 <- fun2(m)
  })
  output$dat6 <- renderTable({
    inFile <- input$data6
    if (is.null(inFile))
      return(NULL)
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p3 <- fun3(m)
  })
  output$result2 <- renderTable({
    inFile <- input$data2
    if (is.null(inFile))
      return(NULL)
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p2 <- fun2(m)
    inFile <- input$data3
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p3 <- fun3(m)
    m1 <- p2
    m2 <- p3
    m <- cbind.data.frame(m1$movies,m2$W1,m2$lang,m2$q1,m2$q2,m2$q3,m2$q4,m2$q5,m2$q6,m1$p0,m1$p1,m1$p2,m1$p3,m1$p4,m1$p5,m1$p6,m2$gross)
    colnames(m) <- c("movies","W1","lang","q1","q2","q3","q4","q5","q6","p0","p1","p2","p3","p4","p5","p6","gross")
    coeff <- fun5(m,0)
    inFile <- input$data5
    if (is.null(inFile))
      return(NULL)
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p2 <- fun2(m)
    inFile <- input$data6
    if (is.null(inFile))
      return(NULL)
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p3 <- fun3(m)
    m1 <- p2
    m2 <- p3
    m <- cbind.data.frame(m1$movies,m2$W1,m2$lang,m2$q1,m2$q2,m2$q3,m2$q4,m2$q5,m2$q6,m1$p0,m1$p1,m1$p2,m1$p3,m1$p4,m1$p5,m1$p6,m2$gross)
    colnames(m) <- c("movies","W1","lang","q1","q2","q3","q4","q5","q6","p0","p1","p2","p3","p4","p5","p6","gross")
    j <- fun6(coeff,m)
    fun4(m,j[,1],j[,2],j[,3],j[,4],j[,5],j[,6])
  })
  output$first_rel_train <- renderPlot({
    inFile <- input$data2
    if (is.null(inFile))
      return(NULL)
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p2 <- fun2(m)
    inFile <- input$data3
    if (is.null(inFile))
      return(NULL)
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p3 <- fun3(m)
    m1 <- p2
    m2 <- p3
    m <- cbind.data.frame(m1$movies,m2$W1,m2$lang,m2$q1,m2$q2,m2$q3,m2$q4,m2$q5,m2$q6,m1$p0,m1$p1,m1$p2,m1$p3,m1$p4,m1$p5,m1$p6,m2$gross)
    colnames(m) <- c("movies","W1","lang","q1","q2","q3","q4","q5","q6","p0","p1","p2","p3","p4","p5","p6","gross")
    lm <- fun5(m,1)
    n <- fun4(m,lm[,1],lm[,2],lm[,3],lm[,4],lm[,5],lm[,6])
    j <- n[,14:16]
    j <- transform(j,min_err=apply(j,1,min))
    k <- mean(j$min_err)
    b <- cbind(1:nrow(n))
    b1 <- cbind(b,m[,17],n[,11])
    b2 <- cbind(b,m[,17],n[,12])
    b3 <- cbind(b,m[,17],n[,13])
    b4 <- cbind(b,n)
    fit0 <- glm(formula = m2$gross ~ m2$W1)
    p <- coef(summary(fit0))
    zzz <- p[1] + p[2]*m2$W1
    par(col="red")
    y <- m2$gross
    x <- m2$W1
    plot(x,y,xlab="First week")
    par(col="blue")
    y <- zzz
    x <- m2$W1
    points(x,y,xlab="First week",ylab="Gross/Estimate")
  })
  output$est_rel_train_0 <- renderPlot({
    inFile <- input$data2
    if (is.null(inFile))
      return(NULL)
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p2 <- fun2(m)
    inFile <- input$data3
    if (is.null(inFile))
      return(NULL)
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p3 <- fun3(m)
    m1 <- p2
    m2 <- p3
    m <- cbind.data.frame(m1$movies,m2$W1,m2$lang,m2$q1,m2$q2,m2$q3,m2$q4,m2$q5,m2$q6,m1$p0,m1$p1,m1$p2,m1$p3,m1$p4,m1$p5,m1$p6,m2$gross)
    colnames(m) <- c("movies","W1","lang","q1","q2","q3","q4","q5","q6","p0","p1","p2","p3","p4","p5","p6","gross")
    lm <- fun5(m,1)
    n <- fun4(m,lm[,1],lm[,2],lm[,3],lm[,4],lm[,5],lm[,6])
    j <- n[,14:16]
    j <- transform(j,min_err=apply(j,1,min))
    k <- mean(j$min_err)
    b <- cbind(1:nrow(n))
    b1 <- cbind(b,m[,17],n[,11])
    b2 <- cbind(b,m[,17],n[,12])
    b3 <- cbind(b,m[,17],n[,13])
    b4 <- cbind(b,n)
    fit0 <- glm(formula = m2$gross ~ m2$W1)
    p <- coef(summary(fit0))
    zzz <- p[1] + p[2]*m2$W1
    x <- b4[,1]
	g0 <- b4[,2]
	a0 <- b4[,3]
	plot(x,g0,xlab="Movie Ordinal",ylab="Earning in Cr INR")
	lines(x,g0,xlab="Movie Ordinal",ylab="Earning in Cr INR",col="red")
	lines(x,a0,xlab="Movie Ordinal",ylab="Earning in Cr INR",col="green")
  })
  output$est_rel_train_1 <- renderPlot({
    inFile <- input$data2
    if (is.null(inFile))
      return(NULL)
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p2 <- fun2(m)
    inFile <- input$data3
    if (is.null(inFile))
      return(NULL)
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p3 <- fun3(m)
    m1 <- p2
    m2 <- p3
    m <- cbind.data.frame(m1$movies,m2$W1,m2$lang,m2$q1,m2$q2,m2$q3,m2$q4,m2$q5,m2$q6,m1$p0,m1$p1,m1$p2,m1$p3,m1$p4,m1$p5,m1$p6,m2$gross)
    colnames(m) <- c("movies","W1","lang","q1","q2","q3","q4","q5","q6","p0","p1","p2","p3","p4","p5","p6","gross")
    lm <- fun5(m,1)
    n <- fun4(m,lm[,1],lm[,2],lm[,3],lm[,4],lm[,5],lm[,6])
    j <- n[,14:16]
    j <- transform(j,min_err=apply(j,1,min))
    k <- mean(j$min_err)
    b <- cbind(1:nrow(n))
    b1 <- cbind(b,m[,17],n[,11])
    b2 <- cbind(b,m[,17],n[,12])
    b3 <- cbind(b,m[,17],n[,13])
    b4 <- cbind(b,n)
    fit0 <- glm(formula = m2$gross ~ m2$W1)
    p <- coef(summary(fit0))
    zzz <- p[1] + p[2]*m2$W1
    x <- b4[,1]
	g0 <- b4[,2]
	b0 <- b4[,4]
	plot(x,g0,xlab="Movie Ordinal",ylab="Earning in Cr INR")
	lines(x,g0,xlab="Movie Ordinal",ylab="Earning in Cr INR",col="red")
	lines(x,b0,xlab="Movie Ordinal",ylab="Earning in Cr INR",col="green")
  })
  output$est_rel_train_2 <- renderPlot({
    inFile <- input$data2
    if (is.null(inFile))
      return(NULL)
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p2 <- fun2(m)
    inFile <- input$data3
    if (is.null(inFile))
      return(NULL)
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p3 <- fun3(m)
    m1 <- p2
    m2 <- p3
    m <- cbind.data.frame(m1$movies,m2$W1,m2$lang,m2$q1,m2$q2,m2$q3,m2$q4,m2$q5,m2$q6,m1$p0,m1$p1,m1$p2,m1$p3,m1$p4,m1$p5,m1$p6,m2$gross)
    colnames(m) <- c("movies","W1","lang","q1","q2","q3","q4","q5","q6","p0","p1","p2","p3","p4","p5","p6","gross")
    lm <- fun5(m,1)
    n <- fun4(m,lm[,1],lm[,2],lm[,3],lm[,4],lm[,5],lm[,6])
    j <- n[,14:16]
    j <- transform(j,min_err=apply(j,1,min))
    k <- mean(j$min_err)
    b <- cbind(1:nrow(n))
    b1 <- cbind(b,m[,17],n[,11])
    b2 <- cbind(b,m[,17],n[,12])
    b3 <- cbind(b,m[,17],n[,13])
    b4 <- cbind(b,n)
    fit0 <- glm(formula = m2$gross ~ m2$W1)
    p <- coef(summary(fit0))
    zzz <- p[1] + p[2]*m2$W1
    x <- b4[,1]
	g0 <- b4[,2]
	c0 <- b4[,5]
	plot(x,g0,xlab="Movie Ordinal",ylab="Earning in Cr INR")
	lines(x,g0,xlab="Movie Ordinal",ylab="Earning in Cr INR",col="red")
	lines(x,c0,xlab="Movie Ordinal",ylab="Earning in Cr INR",col="green")
  })
  output$est_rel_train_3 <- renderPlot({
    inFile <- input$data2
    if (is.null(inFile))
      return(NULL)
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p2 <- fun2(m)
    inFile <- input$data3
    if (is.null(inFile))
      return(NULL)
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p3 <- fun3(m)
    m1 <- p2
    m2 <- p3
    m <- cbind.data.frame(m1$movies,m2$W1,m2$lang,m2$q1,m2$q2,m2$q3,m2$q4,m2$q5,m2$q6,m1$p0,m1$p1,m1$p2,m1$p3,m1$p4,m1$p5,m1$p6,m2$gross)
    colnames(m) <- c("movies","W1","lang","q1","q2","q3","q4","q5","q6","p0","p1","p2","p3","p4","p5","p6","gross")
    lm <- fun5(m,1)
    n <- fun4(m,lm[,1],lm[,2],lm[,3],lm[,4],lm[,5],lm[,6])
    j <- n[,14:16]
    j <- transform(j,min_err=apply(j,1,min))
    k <- mean(j$min_err)
    b <- cbind(1:nrow(n))
    b1 <- cbind(b,m[,17],n[,11])
    b2 <- cbind(b,m[,17],n[,12])
    b3 <- cbind(b,m[,17],n[,13])
    b4 <- cbind(b,n)
    fit0 <- glm(formula = m2$gross ~ m2$W1)
    p <- coef(summary(fit0))
    zzz <- p[1] + p[2]*m2$W1
    x <- b4[,1]
	g0 <- b4[,2]
	d0 <- b4[,6]
	plot(x,g0,xlab="Movie Ordinal",ylab="Earning in Cr INR")
	lines(x,g0,xlab="Movie Ordinal",ylab="Earning in Cr INR",col="red")
	lines(x,d0,xlab="Movie Ordinal",ylab="Earning in Cr INR",col="green")
  })
  output$est_rel_train_4 <- renderPlot({
    inFile <- input$data2
    if (is.null(inFile))
      return(NULL)
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p2 <- fun2(m)
    inFile <- input$data3
    if (is.null(inFile))
      return(NULL)
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p3 <- fun3(m)
    m1 <- p2
    m2 <- p3
    m <- cbind.data.frame(m1$movies,m2$W1,m2$lang,m2$q1,m2$q2,m2$q3,m2$q4,m2$q5,m2$q6,m1$p0,m1$p1,m1$p2,m1$p3,m1$p4,m1$p5,m1$p6,m2$gross)
    colnames(m) <- c("movies","W1","lang","q1","q2","q3","q4","q5","q6","p0","p1","p2","p3","p4","p5","p6","gross")
    lm <- fun5(m,1)
    n <- fun4(m,lm[,1],lm[,2],lm[,3],lm[,4],lm[,5],lm[,6])
    j <- n[,14:16]
    j <- transform(j,min_err=apply(j,1,min))
    k <- mean(j$min_err)
    b <- cbind(1:nrow(n))
    b1 <- cbind(b,m[,17],n[,11])
    b2 <- cbind(b,m[,17],n[,12])
    b3 <- cbind(b,m[,17],n[,13])
    b4 <- cbind(b,n)
    fit0 <- glm(formula = m2$gross ~ m2$W1)
    p <- coef(summary(fit0))
    zzz <- p[1] + p[2]*m2$W1
    x <- b4[,1]
	g0 <- b4[,2]
	e0 <- b4[,7]
	plot(x,g0,xlab="Movie Ordinal",ylab="Earning in Cr INR")
	lines(x,g0,xlab="Movie Ordinal",ylab="Earning in Cr INR",col="red")
	lines(x,e0,xlab="Movie Ordinal",ylab="Earning in Cr INR",col="green")
  })
  output$predict_max_train <- renderPlot({
    inFile <- input$data2
    if (is.null(inFile))
      return(NULL)
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p2 <- fun2(m)
    inFile <- input$data3
    if (is.null(inFile))
      return(NULL)
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p3 <- fun3(m)
    m1 <- p2
    m2 <- p3
    m <- cbind.data.frame(m1$movies,m2$W1,m2$lang,m2$q1,m2$q2,m2$q3,m2$q4,m2$q5,m2$q6,m1$p0,m1$p1,m1$p2,m1$p3,m1$p4,m1$p5,m1$p6,m2$gross)
    colnames(m) <- c("movies","W1","lang","q1","q2","q3","q4","q5","q6","p0","p1","p2","p3","p4","p5","p6","gross")
    lm <- fun5(m,1)
    n <- fun4(m,lm[,1],lm[,2],lm[,3],lm[,4],lm[,5],lm[,6])
    j <- n[,14:16]
    j <- transform(j,min_err=apply(j,1,min))
    k <- mean(j$min_err)
    b <- cbind(1:nrow(n))
    b1 <- cbind(b,m[,17],n[,11])
    b2 <- cbind(b,m[,17],n[,12])
    b3 <- cbind(b,m[,17],n[,13])
    b4 <- cbind(b,n)
    fit0 <- glm(formula = m2$gross ~ m2$W1)
    p <- coef(summary(fit0))
    zzz <- p[1] + p[2]*m2$W1
    y<-b1[,2]
	x<-b1[,1]
	z<-b1[,3]
	plot(x,y,xlab="Movie Ordinal",ylab="Earning in Cr INR")
	lines(x,y,col="red")
	lines(x,z,col="green")
  })
  output$predict_mean_train <- renderPlot({
    inFile <- input$data2
    if (is.null(inFile))
      return(NULL)
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p2 <- fun2(m)
    inFile <- input$data3
    if (is.null(inFile))
      return(NULL)
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p3 <- fun3(m)
    m1 <- p2
    m2 <- p3
    m <- cbind.data.frame(m1$movies,m2$W1,m2$lang,m2$q1,m2$q2,m2$q3,m2$q4,m2$q5,m2$q6,m1$p0,m1$p1,m1$p2,m1$p3,m1$p4,m1$p5,m1$p6,m2$gross)
    colnames(m) <- c("movies","W1","lang","q1","q2","q3","q4","q5","q6","p0","p1","p2","p3","p4","p5","p6","gross")
    lm <- fun5(m,1)
    n <- fun4(m,lm[,1],lm[,2],lm[,3],lm[,4],lm[,5],lm[,6])
    j <- n[,14:16]
    j <- transform(j,min_err=apply(j,1,min))
    k <- mean(j$min_err)
    b <- cbind(1:nrow(n))
    b1 <- cbind(b,m[,17],n[,11])
    b2 <- cbind(b,m[,17],n[,12])
    b3 <- cbind(b,m[,17],n[,13])
    b4 <- cbind(b,n)
    fit0 <- glm(formula = m2$gross ~ m2$W1)
    p <- coef(summary(fit0))
    zzz <- p[1] + p[2]*m2$W1
    y<-b2[,2]
	x<-b2[,1]
	z<-b2[,3]
	plot(x,y,xlab="Movie Ordinal",ylab="Earning in Cr INR")
	lines(x,y,col="red")
	lines(x,z,col="green")
  })
  output$predict_min_train <- renderPlot({
    inFile <- input$data2
    if (is.null(inFile))
      return(NULL)
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p2 <- fun2(m)
    inFile <- input$data3
    if (is.null(inFile))
      return(NULL)
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p3 <- fun3(m)
    m1 <- p2
    m2 <- p3
    m <- cbind.data.frame(m1$movies,m2$W1,m2$lang,m2$q1,m2$q2,m2$q3,m2$q4,m2$q5,m2$q6,m1$p0,m1$p1,m1$p2,m1$p3,m1$p4,m1$p5,m1$p6,m2$gross)
    colnames(m) <- c("movies","W1","lang","q1","q2","q3","q4","q5","q6","p0","p1","p2","p3","p4","p5","p6","gross")
    lm <- fun5(m,1)
    n <- fun4(m,lm[,1],lm[,2],lm[,3],lm[,4],lm[,5],lm[,6])
    j <- n[,14:16]
    j <- transform(j,min_err=apply(j,1,min))
    k <- mean(j$min_err)
    b <- cbind(1:nrow(n))
    b1 <- cbind(b,m[,17],n[,11])
    b2 <- cbind(b,m[,17],n[,12])
    b3 <- cbind(b,m[,17],n[,13])
    b4 <- cbind(b,n)
    fit0 <- glm(formula = m2$gross ~ m2$W1)
    p <- coef(summary(fit0))
    zzz <- p[1] + p[2]*m2$W1
    y<-b3[,2]
	x<-b3[,1]
	z<-b3[,3]
	plot(x,y,xlab="Movie Ordinal",ylab="Earning in Cr INR")
	lines(x,y,col="red")
	lines(x,z,col="green")
  })
  output$first_rel_test <- renderPlot({
	inFile <- input$data2
    if (is.null(inFile))
      return(NULL)
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p2 <- fun2(m)
    inFile <- input$data3
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p3 <- fun3(m)
    m1 <- p2
    m2 <- p3
    m <- cbind.data.frame(m1$movies,m2$W1,m2$lang,m2$q1,m2$q2,m2$q3,m2$q4,m2$q5,m2$q6,m1$p0,m1$p1,m1$p2,m1$p3,m1$p4,m1$p5,m1$p6,m2$gross)
    colnames(m) <- c("movies","W1","lang","q1","q2","q3","q4","q5","q6","p0","p1","p2","p3","p4","p5","p6","gross")
    coeff <- fun5(m,0)
    inFile <- input$data5
    if (is.null(inFile))
      return(NULL)
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p2 <- fun2(m)
    inFile <- input$data6
    if (is.null(inFile))
      return(NULL)
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p3 <- fun3(m)
    m1 <- p2
    m2 <- p3
    m <- cbind.data.frame(m1$movies,m2$W1,m2$lang,m2$q1,m2$q2,m2$q3,m2$q4,m2$q5,m2$q6,m1$p0,m1$p1,m1$p2,m1$p3,m1$p4,m1$p5,m1$p6,m2$gross)
    colnames(m) <- c("movies","W1","lang","q1","q2","q3","q4","q5","q6","p0","p1","p2","p3","p4","p5","p6","gross")
    lm <- fun6(coeff,m)
    n <- fun4(m,lm[,1],lm[,2],lm[,3],lm[,4],lm[,5],lm[,6])
    j <- n[,14:16]
    j <- transform(j,min_err=apply(j,1,min))
    k <- mean(j$min_err)
    b <- cbind(1:nrow(n))
    b1 <- cbind(b,m[,17],n[,11])
    b2 <- cbind(b,m[,17],n[,12])
    b3 <- cbind(b,m[,17],n[,13])
    b4 <- cbind(b,n)
    fit0 <- glm(formula = m2$gross ~ m2$W1)
    p <- coef(summary(fit0))
    zzz <- p[1] + p[2]*m2$W1
    par(col="red")
    y <- m2$gross
    x <- m2$W1
    plot(x,y,xlab="First week")
    par(col="blue")
    y <- zzz
    x <- m2$W1
    points(x,y,xlab="First week",ylab="Gross/Estimate")
  })
  output$est_rel_test_0 <- renderPlot({
	inFile <- input$data2
    if (is.null(inFile))
      return(NULL)
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p2 <- fun2(m)
    inFile <- input$data3
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p3 <- fun3(m)
    m1 <- p2
    m2 <- p3
    m <- cbind.data.frame(m1$movies,m2$W1,m2$lang,m2$q1,m2$q2,m2$q3,m2$q4,m2$q5,m2$q6,m1$p0,m1$p1,m1$p2,m1$p3,m1$p4,m1$p5,m1$p6,m2$gross)
    colnames(m) <- c("movies","W1","lang","q1","q2","q3","q4","q5","q6","p0","p1","p2","p3","p4","p5","p6","gross")
    coeff <- fun5(m,0)
    inFile <- input$data5
    if (is.null(inFile))
      return(NULL)
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p2 <- fun2(m)
    inFile <- input$data6
    if (is.null(inFile))
      return(NULL)
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p3 <- fun3(m)
    m1 <- p2
    m2 <- p3
    m <- cbind.data.frame(m1$movies,m2$W1,m2$lang,m2$q1,m2$q2,m2$q3,m2$q4,m2$q5,m2$q6,m1$p0,m1$p1,m1$p2,m1$p3,m1$p4,m1$p5,m1$p6,m2$gross)
    colnames(m) <- c("movies","W1","lang","q1","q2","q3","q4","q5","q6","p0","p1","p2","p3","p4","p5","p6","gross")
    lm <- fun6(coeff,m)
    n <- fun4(m,lm[,1],lm[,2],lm[,3],lm[,4],lm[,5],lm[,6])
    j <- n[,14:16]
    j <- transform(j,min_err=apply(j,1,min))
    k <- mean(j$min_err)
    b <- cbind(1:nrow(n))
    b1 <- cbind(b,m[,17],n[,11])
    b2 <- cbind(b,m[,17],n[,12])
    b3 <- cbind(b,m[,17],n[,13])
    b4 <- cbind(b,n)
    fit0 <- glm(formula = m2$gross ~ m2$W1)
    p <- coef(summary(fit0))
    zzz <- p[1] + p[2]*m2$W1
    x <- b4[,1]
	g0 <- b4[,2]
	a0 <- b4[,3]
	plot(x,g0,xlab="Movie Ordinal",ylab="Earning in Cr INR")
	lines(x,g0,xlab="Movie Ordinal",ylab="Earning in Cr INR",col="red")
	lines(x,a0,xlab="Movie Ordinal",ylab="Earning in Cr INR",col="green")
  })
  output$est_rel_test_1 <- renderPlot({
	inFile <- input$data2
    if (is.null(inFile))
      return(NULL)
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p2 <- fun2(m)
    inFile <- input$data3
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p3 <- fun3(m)
    m1 <- p2
    m2 <- p3
    m <- cbind.data.frame(m1$movies,m2$W1,m2$lang,m2$q1,m2$q2,m2$q3,m2$q4,m2$q5,m2$q6,m1$p0,m1$p1,m1$p2,m1$p3,m1$p4,m1$p5,m1$p6,m2$gross)
    colnames(m) <- c("movies","W1","lang","q1","q2","q3","q4","q5","q6","p0","p1","p2","p3","p4","p5","p6","gross")
    coeff <- fun5(m,0)
    inFile <- input$data5
    if (is.null(inFile))
      return(NULL)
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p2 <- fun2(m)
    inFile <- input$data6
    if (is.null(inFile))
      return(NULL)
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p3 <- fun3(m)
    m1 <- p2
    m2 <- p3
    m <- cbind.data.frame(m1$movies,m2$W1,m2$lang,m2$q1,m2$q2,m2$q3,m2$q4,m2$q5,m2$q6,m1$p0,m1$p1,m1$p2,m1$p3,m1$p4,m1$p5,m1$p6,m2$gross)
    colnames(m) <- c("movies","W1","lang","q1","q2","q3","q4","q5","q6","p0","p1","p2","p3","p4","p5","p6","gross")
    lm <- fun6(coeff,m)
    n <- fun4(m,lm[,1],lm[,2],lm[,3],lm[,4],lm[,5],lm[,6])
    j <- n[,14:16]
    j <- transform(j,min_err=apply(j,1,min))
    k <- mean(j$min_err)
    b <- cbind(1:nrow(n))
    b1 <- cbind(b,m[,17],n[,11])
    b2 <- cbind(b,m[,17],n[,12])
    b3 <- cbind(b,m[,17],n[,13])
    b4 <- cbind(b,n)
    fit0 <- glm(formula = m2$gross ~ m2$W1)
    p <- coef(summary(fit0))
    zzz <- p[1] + p[2]*m2$W1
    x <- b4[,1]
	g0 <- b4[,2]
	b0 <- b4[,4]
	plot(x,g0,xlab="Movie Ordinal",ylab="Earning in Cr INR")
	lines(x,g0,xlab="Movie Ordinal",ylab="Earning in Cr INR",col="red")
	lines(x,b0,xlab="Movie Ordinal",ylab="Earning in Cr INR",col="green")
  })
  output$est_rel_test_2 <- renderPlot({
	inFile <- input$data2
    if (is.null(inFile))
      return(NULL)
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p2 <- fun2(m)
    inFile <- input$data3
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p3 <- fun3(m)
    m1 <- p2
    m2 <- p3
    m <- cbind.data.frame(m1$movies,m2$W1,m2$lang,m2$q1,m2$q2,m2$q3,m2$q4,m2$q5,m2$q6,m1$p0,m1$p1,m1$p2,m1$p3,m1$p4,m1$p5,m1$p6,m2$gross)
    colnames(m) <- c("movies","W1","lang","q1","q2","q3","q4","q5","q6","p0","p1","p2","p3","p4","p5","p6","gross")
    coeff <- fun5(m,0)
    inFile <- input$data5
    if (is.null(inFile))
      return(NULL)
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p2 <- fun2(m)
    inFile <- input$data6
    if (is.null(inFile))
      return(NULL)
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p3 <- fun3(m)
    m1 <- p2
    m2 <- p3
    m <- cbind.data.frame(m1$movies,m2$W1,m2$lang,m2$q1,m2$q2,m2$q3,m2$q4,m2$q5,m2$q6,m1$p0,m1$p1,m1$p2,m1$p3,m1$p4,m1$p5,m1$p6,m2$gross)
    colnames(m) <- c("movies","W1","lang","q1","q2","q3","q4","q5","q6","p0","p1","p2","p3","p4","p5","p6","gross")
    lm <- fun6(coeff,m)
    n <- fun4(m,lm[,1],lm[,2],lm[,3],lm[,4],lm[,5],lm[,6])
    j <- n[,14:16]
    j <- transform(j,min_err=apply(j,1,min))
    k <- mean(j$min_err)
    b <- cbind(1:nrow(n))
    b1 <- cbind(b,m[,17],n[,11])
    b2 <- cbind(b,m[,17],n[,12])
    b3 <- cbind(b,m[,17],n[,13])
    b4 <- cbind(b,n)
    fit0 <- glm(formula = m2$gross ~ m2$W1)
    p <- coef(summary(fit0))
    zzz <- p[1] + p[2]*m2$W1
    x <- b4[,1]
	g0 <- b4[,2]
	c0 <- b4[,5]
	plot(x,g0,xlab="Movie Ordinal",ylab="Earning in Cr INR")
	lines(x,g0,xlab="Movie Ordinal",ylab="Earning in Cr INR",col="red")
	lines(x,c0,xlab="Movie Ordinal",ylab="Earning in Cr INR",col="green")
  })
  output$est_rel_test_3 <- renderPlot({
	inFile <- input$data2
    if (is.null(inFile))
      return(NULL)
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p2 <- fun2(m)
    inFile <- input$data3
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p3 <- fun3(m)
    m1 <- p2
    m2 <- p3
    m <- cbind.data.frame(m1$movies,m2$W1,m2$lang,m2$q1,m2$q2,m2$q3,m2$q4,m2$q5,m2$q6,m1$p0,m1$p1,m1$p2,m1$p3,m1$p4,m1$p5,m1$p6,m2$gross)
    colnames(m) <- c("movies","W1","lang","q1","q2","q3","q4","q5","q6","p0","p1","p2","p3","p4","p5","p6","gross")
    coeff <- fun5(m,0)
    inFile <- input$data5
    if (is.null(inFile))
      return(NULL)
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p2 <- fun2(m)
    inFile <- input$data6
    if (is.null(inFile))
      return(NULL)
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p3 <- fun3(m)
    m1 <- p2
    m2 <- p3
    m <- cbind.data.frame(m1$movies,m2$W1,m2$lang,m2$q1,m2$q2,m2$q3,m2$q4,m2$q5,m2$q6,m1$p0,m1$p1,m1$p2,m1$p3,m1$p4,m1$p5,m1$p6,m2$gross)
    colnames(m) <- c("movies","W1","lang","q1","q2","q3","q4","q5","q6","p0","p1","p2","p3","p4","p5","p6","gross")
    lm <- fun6(coeff,m)
    n <- fun4(m,lm[,1],lm[,2],lm[,3],lm[,4],lm[,5],lm[,6])
    j <- n[,14:16]
    j <- transform(j,min_err=apply(j,1,min))
    k <- mean(j$min_err)
    b <- cbind(1:nrow(n))
    b1 <- cbind(b,m[,17],n[,11])
    b2 <- cbind(b,m[,17],n[,12])
    b3 <- cbind(b,m[,17],n[,13])
    b4 <- cbind(b,n)
    fit0 <- glm(formula = m2$gross ~ m2$W1)
    p <- coef(summary(fit0))
    zzz <- p[1] + p[2]*m2$W1
    x <- b4[,1]
	g0 <- b4[,2]
	d0 <- b4[,6]
	plot(x,g0,xlab="Movie Ordinal",ylab="Earning in Cr INR")
	lines(x,g0,xlab="Movie Ordinal",ylab="Earning in Cr INR",col="red")
	lines(x,d0,xlab="Movie Ordinal",ylab="Earning in Cr INR",col="green")
  })
  output$est_rel_test_4 <- renderPlot({
	inFile <- input$data2
    if (is.null(inFile))
      return(NULL)
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p2 <- fun2(m)
    inFile <- input$data3
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p3 <- fun3(m)
    m1 <- p2
    m2 <- p3
    m <- cbind.data.frame(m1$movies,m2$W1,m2$lang,m2$q1,m2$q2,m2$q3,m2$q4,m2$q5,m2$q6,m1$p0,m1$p1,m1$p2,m1$p3,m1$p4,m1$p5,m1$p6,m2$gross)
    colnames(m) <- c("movies","W1","lang","q1","q2","q3","q4","q5","q6","p0","p1","p2","p3","p4","p5","p6","gross")
    coeff <- fun5(m,0)
    inFile <- input$data5
    if (is.null(inFile))
      return(NULL)
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p2 <- fun2(m)
    inFile <- input$data6
    if (is.null(inFile))
      return(NULL)
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p3 <- fun3(m)
    m1 <- p2
    m2 <- p3
    m <- cbind.data.frame(m1$movies,m2$W1,m2$lang,m2$q1,m2$q2,m2$q3,m2$q4,m2$q5,m2$q6,m1$p0,m1$p1,m1$p2,m1$p3,m1$p4,m1$p5,m1$p6,m2$gross)
    colnames(m) <- c("movies","W1","lang","q1","q2","q3","q4","q5","q6","p0","p1","p2","p3","p4","p5","p6","gross")
    lm <- fun6(coeff,m)
    n <- fun4(m,lm[,1],lm[,2],lm[,3],lm[,4],lm[,5],lm[,6])
    j <- n[,14:16]
    j <- transform(j,min_err=apply(j,1,min))
    k <- mean(j$min_err)
    b <- cbind(1:nrow(n))
    b1 <- cbind(b,m[,17],n[,11])
    b2 <- cbind(b,m[,17],n[,12])
    b3 <- cbind(b,m[,17],n[,13])
    b4 <- cbind(b,n)
    fit0 <- glm(formula = m2$gross ~ m2$W1)
    p <- coef(summary(fit0))
    zzz <- p[1] + p[2]*m2$W1
    x <- b4[,1]
	g0 <- b4[,2]
	e0 <- b4[,7]
	plot(x,g0,xlab="Movie Ordinal",ylab="Earning in Cr INR")
	lines(x,g0,xlab="Movie Ordinal",ylab="Earning in Cr INR",col="red")
	lines(x,e0,xlab="Movie Ordinal",ylab="Earning in Cr INR",col="green")
  })
  output$predict_max_test <- renderPlot({
	inFile <- input$data2
    if (is.null(inFile))
      return(NULL)
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p2 <- fun2(m)
    inFile <- input$data3
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p3 <- fun3(m)
    m1 <- p2
    m2 <- p3
    m <- cbind.data.frame(m1$movies,m2$W1,m2$lang,m2$q1,m2$q2,m2$q3,m2$q4,m2$q5,m2$q6,m1$p0,m1$p1,m1$p2,m1$p3,m1$p4,m1$p5,m1$p6,m2$gross)
    colnames(m) <- c("movies","W1","lang","q1","q2","q3","q4","q5","q6","p0","p1","p2","p3","p4","p5","p6","gross")
    coeff <- fun5(m,0)
    inFile <- input$data5
    if (is.null(inFile))
      return(NULL)
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p2 <- fun2(m)
    inFile <- input$data6
    if (is.null(inFile))
      return(NULL)
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p3 <- fun3(m)
    m1 <- p2
    m2 <- p3
    m <- cbind.data.frame(m1$movies,m2$W1,m2$lang,m2$q1,m2$q2,m2$q3,m2$q4,m2$q5,m2$q6,m1$p0,m1$p1,m1$p2,m1$p3,m1$p4,m1$p5,m1$p6,m2$gross)
    colnames(m) <- c("movies","W1","lang","q1","q2","q3","q4","q5","q6","p0","p1","p2","p3","p4","p5","p6","gross")
    lm <- fun6(coeff,m)
    n <- fun4(m,lm[,1],lm[,2],lm[,3],lm[,4],lm[,5],lm[,6])
    j <- n[,14:16]
    j <- transform(j,min_err=apply(j,1,min))
    k <- mean(j$min_err)
    b <- cbind(1:nrow(n))
    b1 <- cbind(b,m[,17],n[,11])
    b2 <- cbind(b,m[,17],n[,12])
    b3 <- cbind(b,m[,17],n[,13])
    b4 <- cbind(b,n)
    fit0 <- glm(formula = m2$gross ~ m2$W1)
    p <- coef(summary(fit0))
    zzz <- p[1] + p[2]*m2$W1
    y<-b1[,2]
	x<-b1[,1]
	z<-b1[,3]
	plot(x,y,xlab="Movie Ordinal",ylab="Earning in Cr INR")
	lines(x,y,col="red")
	lines(x,z,col="green")
  })
  output$predict_mean_test <- renderPlot({
	inFile <- input$data2
    if (is.null(inFile))
      return(NULL)
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p2 <- fun2(m)
    inFile <- input$data3
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p3 <- fun3(m)
    m1 <- p2
    m2 <- p3
    m <- cbind.data.frame(m1$movies,m2$W1,m2$lang,m2$q1,m2$q2,m2$q3,m2$q4,m2$q5,m2$q6,m1$p0,m1$p1,m1$p2,m1$p3,m1$p4,m1$p5,m1$p6,m2$gross)
    colnames(m) <- c("movies","W1","lang","q1","q2","q3","q4","q5","q6","p0","p1","p2","p3","p4","p5","p6","gross")
    coeff <- fun5(m,0)
    inFile <- input$data5
    if (is.null(inFile))
      return(NULL)
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p2 <- fun2(m)
    inFile <- input$data6
    if (is.null(inFile))
      return(NULL)
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p3 <- fun3(m)
    m1 <- p2
    m2 <- p3
    m <- cbind.data.frame(m1$movies,m2$W1,m2$lang,m2$q1,m2$q2,m2$q3,m2$q4,m2$q5,m2$q6,m1$p0,m1$p1,m1$p2,m1$p3,m1$p4,m1$p5,m1$p6,m2$gross)
    colnames(m) <- c("movies","W1","lang","q1","q2","q3","q4","q5","q6","p0","p1","p2","p3","p4","p5","p6","gross")
    lm <- fun6(coeff,m)
    n <- fun4(m,lm[,1],lm[,2],lm[,3],lm[,4],lm[,5],lm[,6])
    j <- n[,14:16]
    j <- transform(j,min_err=apply(j,1,min))
    k <- mean(j$min_err)
    b <- cbind(1:nrow(n))
    b1 <- cbind(b,m[,17],n[,11])
    b2 <- cbind(b,m[,17],n[,12])
    b3 <- cbind(b,m[,17],n[,13])
    b4 <- cbind(b,n)
    fit0 <- glm(formula = m2$gross ~ m2$W1)
    p <- coef(summary(fit0))
    zzz <- p[1] + p[2]*m2$W1
    y<-b2[,2]
	x<-b2[,1]
	z<-b2[,3]
	plot(x,y,xlab="Movie Ordinal",ylab="Earning in Cr INR")
	lines(x,y,col="red")
	lines(x,z,col="green")
  })
  output$predict_min_test <- renderPlot({
	  inFile <- input$data2
    if (is.null(inFile))
      return(NULL)
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p2 <- fun2(m)
    inFile <- input$data3
    if (is.null(inFile))
      return(NULL)
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p3 <- fun3(m)
    m1 <- p2
    m2 <- p3
    m <- cbind.data.frame(m1$movies,m2$W1,m2$lang,m2$q1,m2$q2,m2$q3,m2$q4,m2$q5,m2$q6,m1$p0,m1$p1,m1$p2,m1$p3,m1$p4,m1$p5,m1$p6,m2$gross)
    colnames(m) <- c("movies","W1","lang","q1","q2","q3","q4","q5","q6","p0","p1","p2","p3","p4","p5","p6","gross")
    coeff <- fun5(m,0)
    inFile <- input$data5
    if (is.null(inFile))
      return(NULL)
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p2 <- fun2(m)
    inFile <- input$data6
    if (is.null(inFile))
      return(NULL)
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p3 <- fun3(m)
    m1 <- p2
    m2 <- p3
    m <- cbind.data.frame(m1$movies,m2$W1,m2$lang,m2$q1,m2$q2,m2$q3,m2$q4,m2$q5,m2$q6,m1$p0,m1$p1,m1$p2,m1$p3,m1$p4,m1$p5,m1$p6,m2$gross)
    colnames(m) <- c("movies","W1","lang","q1","q2","q3","q4","q5","q6","p0","p1","p2","p3","p4","p5","p6","gross")
    lm <- fun6(coeff,m)
    n <- fun4(m,lm[,1],lm[,2],lm[,3],lm[,4],lm[,5],lm[,6])
    j <- n[,14:16]
    j <- transform(j,min_err=apply(j,1,min))
    k <- mean(j$min_err)
    b <- cbind(1:nrow(n))
    b1 <- cbind(b,m[,17],n[,11])
    b2 <- cbind(b,m[,17],n[,12])
    b3 <- cbind(b,m[,17],n[,13])
    b4 <- cbind(b,n)
    fit0 <- glm(formula = m2$gross ~ m2$W1)
    p <- coef(summary(fit0))
    zzz <- p[1] + p[2]*m2$W1
    y<-b3[,2]
	x<-b3[,1]
	z<-b3[,3]
	plot(x,y,xlab="Movie Ordinal",ylab="Earning in Cr INR")
	lines(x,y,col="red")
	lines(x,z,col="green")
  })
  output$verdict_train <- renderUI({
    inFile <- input$data2
    if (is.null(inFile))
      return(NULL)
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p2 <- fun2(m)
    inFile <- input$data3
    if (is.null(inFile))
      return(NULL)
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p3 <- fun3(m)
    m1 <- p2
    m2 <- p3
    m <- cbind.data.frame(m1$movies,m2$W1,m2$lang,m2$q1,m2$q2,m2$q3,m2$q4,m2$q5,m2$q6,m1$p0,m1$p1,m1$p2,m1$p3,m1$p4,m1$p5,m1$p6,m2$gross)
    colnames(m) <- c("movies","W1","lang","q1","q2","q3","q4","q5","q6","p0","p1","p2","p3","p4","p5","p6","gross")
    lm <- fun5(m,1)
    n <- fun4(m,lm[,1],lm[,2],lm[,3],lm[,4],lm[,5],lm[,6])
    str1 <- paste0("The correlation between actual gross amount and max_predict value is :",cor(n$adj.e.1,m2$gross))
    str2 <- paste0("The correlation between actual gross amount and min_predict value is :",cor(n$adj.e.3,m2$gross))
    str3 <- paste0("The correlation between actual gross amount and mean_predict value is :",cor(n$adj.e.2,m2$gross))
    HTML(paste(str1, str2, str3, sep = '<br/>'))
  })
  output$verdict_test <- renderUI({
    inFile <- input$data2
    if (is.null(inFile))
      return(NULL)
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p2 <- fun2(m)
    inFile <- input$data3
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p3 <- fun3(m)
    m1 <- p2
    m2 <- p3
    m <- cbind.data.frame(m1$movies,m2$W1,m2$lang,m2$q1,m2$q2,m2$q3,m2$q4,m2$q5,m2$q6,m1$p0,m1$p1,m1$p2,m1$p3,m1$p4,m1$p5,m1$p6,m2$gross)
    colnames(m) <- c("movies","W1","lang","q1","q2","q3","q4","q5","q6","p0","p1","p2","p3","p4","p5","p6","gross")
    coeff <- fun5(m,0)
    inFile <- input$data5
    if (is.null(inFile))
      return(NULL)
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p2 <- fun2(m)
    inFile <- input$data6
    if (is.null(inFile))
      return(NULL)
    m <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    p3 <- fun3(m)
    m1 <- p2
    m2 <- p3
    m <- cbind.data.frame(m1$movies,m2$W1,m2$lang,m2$q1,m2$q2,m2$q3,m2$q4,m2$q5,m2$q6,m1$p0,m1$p1,m1$p2,m1$p3,m1$p4,m1$p5,m1$p6,m2$gross)
    colnames(m) <- c("movies","W1","lang","q1","q2","q3","q4","q5","q6","p0","p1","p2","p3","p4","p5","p6","gross")
    lm <- fun6(coeff,m)
    n <- fun4(m,lm[,1],lm[,2],lm[,3],lm[,4],lm[,5],lm[,6])
    str1 <- paste0("The correlation between actual gross amount and max_predict value is :",cor(n$adj.e.1,m2$gross))
    str2 <- paste0("The correlation between actual gross amount and min_predict value is :",cor(n$adj.e.3,m2$gross))
    str3 <- paste0("The correlation between actual gross amount and mean_predict value is :",cor(n$adj.e.2,m2$gross))
    HTML(paste(str1, str2, str3,sep = '<br/>'))
  })
  
  session$onSessionEnded(function() { 
    stopApp()
    q("no") 
  })
})
