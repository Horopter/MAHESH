cat('\014')#clear the screen
rm(list=ls())#clear the environment
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir) #change directory to current directory
make_dir <- function(directory)
{
  if(!dir.exists(directory))
  {
    dir.create(directory);
  }
}
require('stringr')
make_dir("current");
filename <- paste0("current/","bindings",".bollystat")
m <- as.matrix(read.table(filename, sep=" ",header=TRUE))
u <- unique(m[,1])
setwd("current")
make_dir("Film segments")
setwd("Film segments")
for(i in 1:length(u))
{
  p <- m[m[,1]==u[i],]
  write.table(p,file=paste0(i,". ",u[i],".bollystat"),row.names = F)
}
setwd("..")
setwd("..")