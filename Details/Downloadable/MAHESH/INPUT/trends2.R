f <- getwd()
script.dir <- dirname(parent.frame(2)$ofile)
setwd(script.dir)
library('gtrendsR')
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
TrendFinder = function(x,auth)
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
TrendFinder2 = function(x,auth)
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
collect_info = function()
{
auth <- gconnect("santoshdesaitaegeuk@gmail.com","blair witch proj3ct!")
vinod <- read.csv(file="dat1.csv",header=TRUE,sep=",")
vin <- vinod
i <- sapply(vinod, is.factor)
vin[i] <- lapply(vinod[i], as.character)
tryCatch({
	do.call(rbind,apply(vin,1,function(x) TrendFinder(x,auth)))
	},warning = function(war) {print(war)}
	,error = function(err) {print(err)}
	)
}
collect_info()
setwd(f)