library('gtrendsR')
TrendFinder = function(x,auth)
{
	moviename <- x[1]
	cat('\014')
	results <- 0
	print(paste(x[1],"movie",sep=" "))
	vin <- x
	# if(file.exists(file.path(".","MovieTrends-long",moviename,"7.csv")))
	# {
		# print(paste(moviename,"exists",sep=" "))
		# return()
	# }
	dir.create(file.path(".","MovieTrends-long",moviename), showWarnings = FALSE,recursive=TRUE)
	setwd(file.path(".","MovieTrends-long",moviename))
	y <- paste(x[1],"movie",sep=" ")
	tryCatch({
		results <- gtrends(query=y)$trend
		},
		error=function(err)
		{
				tryCatch({results <- gtrends(query=x[1])$trend},
				error=function(err)
				{
					print(paste(x[1],"gone haywire",sep=" "))
				})
		})
	if(!is.null(results))
	{
		write.csv(results,file="7.csv",row.names=FALSE,col.names=FALSE)
	}
	setwd('..')
	setwd('..')
}
collect_info = function()
{
auth <- gconnect("santoshdesaitaegeuk@gmail.com","don'tdaretoknowit")
vinod <- read.csv(file="dcsv1.csv",header=TRUE,sep=",")
vin <- vinod
i <- sapply(vinod, is.factor)
vin[i] <- lapply(vinod[i], as.character)
do.call(rbind,apply(vin,1,function(x) TrendFinder(x,auth)))
}