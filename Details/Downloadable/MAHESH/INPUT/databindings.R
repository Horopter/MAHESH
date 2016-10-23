f <- getwd()
script.dir <- dirname(parent.frame(2)$ofile)
setwd(script.dir)
#clear the screen
#clear the environment
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir) #change directory to current directory
make_dir <- function(directory)
{
  if(!dir.exists(directory))
  {
    dir.create(directory);
  }
}
corpusbind<- function(x)
{
  corpus <- matrix(nrow=0,ncol = 8)
  for(i in x)
  {
    linkname <- paste0("Links/",i,".txt");
    l <- readChar(linkname, file.info(linkname)$size);
    filename <- paste0("BollyStats/",i,"- ",l,".bollystat");
    m <- as.matrix(read.table(filename, sep=" ",header=TRUE,row.names = NULL));
    m <- m[-1,]
    m[,2] <- str_replace_all(m[,2],"\\?"," Qm")
    m[,2] <- str_replace_all(m[,2],"\\*","")
    m[,2] <- str_replace_all(m[,2],"\\:","")
    m[,2] <- str_replace_all(m[,2],"\\/","-")
    m <- cbind(m,l)
    corpus <- rbind2(corpus,m)
  }
  corpus <- corpus[,-1]
  colnames(corpus)<-c("Films", "Production" ,"Week", "Weekly", "Domestic_Total", "Theatres", "as_on_date")
  corpus <-corpus[order(corpus[,1]),]
  return(corpus)
}
managecorpus <- function(x,y)
{
  corpus <- corpusbind(y)
  make_dir(x)
  setwd(x)
  write.table(corpus,file=paste0("bindings.bollystat"),row.names = F)
  cat(unique(corpus[,1]),file="cinemaNames.bollystat",sep="\r\n")
  setwd("..")
}
managecorpus("current",1:234)
setwd(f)
