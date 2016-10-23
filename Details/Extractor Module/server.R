library(shiny)
library(shinyFiles)
if(!require('RSelenium'))
{
  install.packages('Rselenium')
  library('Rselenium')
}
if(!require('stringr'))
{
  install.packages('stringr')
  library('stringr')
}
if(!require('gtrendsR'))
{
  install.packages('gtrendsR')
  library('gtrendsR')
}
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
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

TrendFinder = function(x,auth,b)
{
  moviename <- x[1]
  moviename <- trim(moviename)
  print(paste(moviename,"movie",sep=" "))
  vin <- x
  if(dir.exists(file.path(".","MovieTrends",moviename))&&length(list.files(path=file.path(".","MovieTrends",moviename)))>=7)
  {
    print(paste(moviename,"exists",sep=" "))
    return()
  }
  dir.create(file.path(".","MovieTrends",moviename), showWarnings = FALSE,recursive=TRUE)
  setwd(file.path(".","MovieTrends",moviename))
  if(b==1)
    y<-moviename
  else
    y <- paste(moviename,"movie",sep=" ")
  st.date <- as.Date(vin[3],format="%m/%d/%Y")
  end.date <- as.Date(vin[2],format="%m/%d/%Y")
  if(st.date <= end.date && end.date <= as.Date(Sys.time()))
  {
    tryCatch({
      results <- gtrends(query=y,start_date=st.date,end_date=end.date)$trend
      if(!is.null(results)||results!="")
        write.csv(results,file="0.csv",row.names=FALSE)
    }, warning = function(war) {print(war)
    }, error = function(err) {print(err)
    })
  }
  st.date <- as.Date(vin[2],format="%m/%d/%Y")
  end.date <- as.Date(vin[4])
  if(st.date <= end.date && end.date <= as.Date(Sys.time()))
  {
    tryCatch({
      results <- gtrends(query=y,start_date=st.date,end_date=end.date)$trend
      if(!is.null(results)||results!="")
        write.csv(results,file="1.csv",row.names=FALSE)
    }, warning = function(war) {print(war)
    }, error = function(err) {print(err)
    })
  }
  st.date <- as.Date(vin[4])
  end.date <- as.Date(vin[5])
  if(st.date <= end.date && end.date <= as.Date(Sys.time()))
  {
    tryCatch({
      results <- gtrends(query=y,start_date=st.date,end_date=end.date)$trend
      if(!is.null(results)||results!="")
        write.csv(results,file="2.csv",row.names=FALSE)
    }, warning = function(war) {print(war)
    }, error = function(err) {print(err)
    })
  }
  st.date <- as.Date(vin[5])
  end.date <- as.Date(vin[6])
  if(st.date <= end.date && end.date <= as.Date(Sys.time()))
  {
    tryCatch({
      results <- gtrends(query=y,start_date=st.date,end_date=end.date)$trend
      if(!is.null(results)||results!="")
        write.csv(results,file="3.csv",row.names=FALSE)
    }, warning = function(war) {print(war)
    }, error = function(err) {print(err)
    })
  }
  st.date <- as.Date(vin[6])
  end.date <- as.Date(vin[7])
  if(st.date <= end.date && end.date <= as.Date(Sys.time()))
  {
    tryCatch({
      results <- gtrends(query=y,start_date=st.date,end_date=end.date)$trend
      if(!is.null(results)||results!="")
        write.csv(results,file="4.csv",row.names=FALSE)
    }, warning = function(war) {print(war)
    }, error = function(err) {print(err)
    })
  }
  st.date <- as.Date(vin[7])
  end.date <- as.Date(vin[8])
  if(st.date <= end.date && end.date <= as.Date(Sys.time()))
  {
    tryCatch({
      results <- gtrends(query=y,start_date=st.date,end_date=end.date)$trend
      if(!is.null(results)||results!="")
        write.csv(results,file="5.csv",row.names=FALSE)
    }, warning = function(war) {print(war)
    }, error = function(err) {print(err)
    })
  }
  st.date <- as.Date(vin[8])
  end.date <- as.Date(vin[9])
  if(st.date <= end.date && end.date <= as.Date(Sys.time()))
  {
    tryCatch({
      results <- gtrends(query=y,start_date=st.date,end_date=end.date)$trend
      if(!is.null(results)||results!="")
        write.csv(results,file="6.csv",row.names=FALSE)
    }, warning = function(war) {print(war)
    }, error = function(err) {print(err)
    })
  }
  setwd('..')
  setwd('..')
}

collect_info = function()
{
  auth <- gconnect("sirmvit.movieanalytics@gmail.com","zykowod3!")
  vinod <- read.csv(file="dat1.csv",header=TRUE,sep=",")
  vin <- vinod
  i <- sapply(vinod, is.factor)
  vin[i] <- lapply(vinod[i], as.character)
  tryCatch({
    do.call(rbind,apply(vin,1,function(x) TrendFinder(x,auth,1)))
  },warning = function(war) {print(war)}
  ,error = function(err) {print(err)}
  )
  print("****Round 2 begins****")
  tryCatch({
    do.call(rbind,apply(vin,1,function(x) TrendFinder(x,auth,2)))
  },warning = function(war) {print(war)}
  ,error = function(err) {print(err)}
  )
}
TrendFinder2 = function(x,auth)
{
  moviename <- x[1]
  moviename <- trim(moviename)
  
  results <- 0
  print(paste(moviename,"movie",sep=" "))
  vin <- x
  dir.create(file.path(".","MovieTrends-long",moviename), showWarnings = FALSE,recursive=TRUE)
  setwd(file.path(".","MovieTrends-long",moviename))
  y <- paste(moviename,"movie",sep=" ")
  tryCatch({
    results <- gtrends(query=y)$trend
  },
  error=function(err)
  {
    tryCatch({results <- gtrends(query=moviename)$trend},
             error=function(err)
             {
               print(paste(moviename,"gone haywire",sep=" "))
             })
  })
  if(!is.null(results))
  {
    write.csv(results,file="7.csv",row.names=FALSE)
  }
  setwd('..')
  setwd('..')
}
TrendFinder3 = function(x,auth)
{
  moviename <- x[1]
  moviename <- trim(moviename)
  
  results <- 0
  print(paste(moviename,sep=" "))
  vin <- x
  dir.create(file.path(".","MovieTrends-long",moviename), showWarnings = FALSE,recursive=TRUE)
  setwd(file.path(".","MovieTrends-long",moviename))
  y <- paste(moviename,"movie",sep=" ")
  tryCatch({
    results <- gtrends(query=y)$trend
  },
  error=function(err)
  {
    tryCatch({results <- gtrends(query=moviename)$trend},
             error=function(err)
             {
               print(paste(moviename,"gone haywire",sep=" "))
             })
  })
  if(!is.null(results))
  {
    write.csv(results,file="7.csv",row.names=FALSE)
  }
  setwd('..')
  setwd('..')
}

shinyServer(function(input, output, session) {
  volumes <- getVolumes() #c('R Installation'=R.home())
  shinyDirChoose(input, 'directory', roots=volumes, session=session, restrictions=system.file(package='base'))
  output$directorypath <- renderPrint({
    dir <- parseDirPath(volumes, input$directory)
    setwd(as.character(dir$datapath))
    fetchBoxOfficeCollection(1:234)
    managecorpus("current",1:234)
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
    collect_info()
    auth <- gconnect("sirmvit.movieanalytics@gmail.com","zykowod3!")
    vinod <- read.csv(file="dat1.csv",header=TRUE,sep=",")
    vin <- vinod
    i <- sapply(vinod, is.factor)
    vin[i] <- lapply(vinod[i], as.character)
    tryCatch({
      do.call(rbind,apply(vin,1,function(x) TrendFinder(x,auth)))
    },warning = function(war) {print(war)}
    ,error = function(err) {print(err)}
    )
    return('Processing')
    })
  
})