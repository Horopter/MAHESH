f <- getwd()
script.dir <- dirname(parent.frame(2)$ofile)
setwd(script.dir)
#clear the screen
#clear the environment
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir) #change directory to current directory
require('RSelenium')
require('stringr')
newtabspacereduce <- function(htmlString) {
  htmlString <- gsub("[\n\t]+","\n",htmlString)
  htmlString <- gsub("\\s+"," ",htmlString)
  return(htmlString)
}
giveTableFromCorpus <- function(appData)
{
  appData <- newtabspacereduce(appData[1])
  cess <- do.call(rbind,strsplit(as.character(appData[1]),'<table'))
  cess2 <- do.call(rbind,strsplit(as.character(cess[2]),'</table>'))
  cess3 <- do.call(rbind,strsplit(as.character(cess2[1]),' width=\"100%\" cellspacing=\"1\" cellpadding=\"3\" border=\"0\" rules=\"cols\" frame=\"vsides\"> '))
  cess4 <- gsub('<tr.*?>',"\t",cess3[2])
  cess5 <- gsub('<t[db].*?>',"\t",cess4)
  cess6 <- gsub('</t[rdb].*?>',"\t",cess5)
  cess7 <- gsub('<s.*?>',"\t",cess6)
  cess8 <- gsub('</s.*?>',"\t",cess7)
  cess9 <- gsub('\t ',"\t",cess8)
  cess10 <- gsub('[\t]+',"\t",cess9)
  cess11 <- do.call(rbind,strsplit(as.character(cess10),'\t'))
  cess12 <- cess11[cess11 != ""]
  cess13 <- matrix(unlist(cess12), ncol = 6, byrow = TRUE)
  return(cess13)
}
getNameOfFile <- function(appData)
{
  my.string <- appData[1]
  left.border  <- '<div id=\"week-date-compare\">\n    As on '
  right.border <- '</div>'
  pattern <- paste0("(?<=", left.border, ").*(?=", right.border, ")")
  rx <- regexpr(pattern, text=my.string, perl=TRUE)
  s <- substring(my.string, rx, rx+attr(rx, "match.length")-1)
  filename <- gsub(",","",s)
  return(filename)
}
make_dir <- function(directory)
{
  if(!dir.exists(directory))
  {
    dir.create(directory);
  }
}
getBollystats <- function(x)
{
  m<-RSelenium::checkForServer()
  n <- RSelenium::startServer()
  p <- require(RSelenium)
  remDr <- remoteDriver(remoteServerAddr = "localhost" 
                        , port = 4444
                        , browserName = "firefox" , autoClose = TRUE
  )
  remDr$open()
  remDr$navigate("http://www.boxofficeindia.co.in/weekly-collections-%E2%80%93-box-office/")
  option <- remDr$findElement(using = 'xpath', paste0("//*/option[",x,"]"))
  option$clickElement()
  appData <- remDr$getPageSource()
  f <- getNameOfFile(appData)
  table <- giveTableFromCorpus(appData)
  setwd('BollyStats')
  write.table(table,file=paste0(x,"- ",f,".bollystat"))
  setwd("..")
  k <- file.create(paste0("Links/",x,".txt"))
  cat(f,file=paste0("Links/",x,".txt"))
  remDr$closeWindow()
  remDr$close()
}
fetchBoxOfficeCollection <- function(range)
{
  make_dir('BollyStats')
  make_dir('Links')
  for(i in range)
  {
    if(!file.exists(paste0("Links/",i,".txt")))
    {
      getBollystats(i)
    }
    if(file.exists(paste0("BollyStats/",i,"- ",".bollystat")))
    {
      print(paste("Error occurred at number test",i))
	  print(getwd())
	  file.remove(paste0("Links/",i,".txt"))
    }
  }
}
fetchBoxOfficeCollection(1:234)
setwd(f)