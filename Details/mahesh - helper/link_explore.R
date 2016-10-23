cat('\014')#clear the screen
rm(list=ls())#clear the environment
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir) #change directory to current directory
corpusbind<- function(x)
{
  for(i in x)
  {
    linkname <- paste0("Links/",i,".txt");
    l <- readChar(linkname, file.info(linkname)$size);
	m <- paste0("<a href=\"",l,".html\">",l,"</a>")
    write(m,file="linky.txt",append=TRUE)
  }

}
managecorpus <- function(y)
{
  corpusbind(y)
}
managecorpus(1:234)
