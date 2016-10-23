cat('\014')#clear the screen
rm(list=ls())#clear the environment
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir) #change directory to current directory
library('R2HTML')
make_dir <- function(directory)
{
  if(!dir.exists(directory))
  {
    dir.create(directory);
  }
}
values <- function(x,m)
{
if(grepl("Cr",x[m]))
{
x[m]<-gsub("Cr"," ",x[m])
}
else if(grepl("L",x[m]))
{
x[m]<-gsub("L"," ",x[m])
x[m] <- as.numeric(x[m])/100
}
else if(grepl("K",x[m]))
{
x[m]<-gsub("K"," ",x[m])
x[m] <- as.numeric(x[m])/10000
}
return(x)
}
corpusbind<- function(x)
{
	make_dir('html')
  for(i in x)
  {
    linkname <- paste0("Links/",i,".txt");
    l <- readChar(linkname, file.info(linkname)$size);
    filename <- paste0("BollyStats/",i,"- ",l,".bollystat");
	m <- read.csv(filename,sep=" ")
	m <- m[-1,]
	setwd('html')
	colnames(m)<-c("Films", "Production" ,"Week", "Weekly_in_Cr", "Domestic_Total_in_Cr", "Theatres")
	m<-t(apply(m,1,function(x){values(x,4)}))
	m<-t(apply(m,1,function(x){values(x,5)}))
	HTML(m,file=paste0(l,".html"),row.names = FALSE)
	setwd('..')
  }

}
managecorpus <- function(y)
{
  corpusbind(y)
}
managecorpus(1:234)
