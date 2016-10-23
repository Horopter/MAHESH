install.packages('gtrends.R')
library('gtrendsR')
TrendFinder = function(x,auth)
{
	moviename <- x[1]
	cat('\014')
	print(paste(x[1],"movie",sep=" "))
	vin <- x
	if(dir.exists(file.path(".","MovieTrends",moviename)))
	{
		print(paste(moviename,"exists",sep=" "))
		return()
	}
	dir.create(file.path(".","MovieTrends",moviename), showWarnings = FALSE,recursive=TRUE)
	setwd(file.path(".","MovieTrends",moviename))
	y <- paste(x[1],"movie",sep=" ")
	results <- gtrends(query=y,start_date=as.Date(vin[3]),end_date=as.Date(vin[2]))$trend
	print(results)
	if(!is.null(results))
		write.csv(results,file="0.csv")
	results <- gtrends(query=y,start_date=as.Date(vin[2]),end_date=as.Date(vin[5]))$trend
	print(results)
	if(!is.null(results))
		write.csv(results,file="1.csv")
	results <- gtrends(query=y,start_date=as.Date(vin[5]),end_date=as.Date(vin[6]))$trend
	print(results)
	if(!is.null(results))
		write.csv(results,file="2.csv")
	results <- gtrends(query=y,start_date=as.Date(vin[6]),end_date=as.Date(vin[7]))$trend
	print(results)
	if(!is.null(results))
		write.csv(results,file="3.csv")
	results <- gtrends(query=y,start_date=as.Date(vin[7]),end_date=as.Date(vin[8]))$trend
	print(results)
	if(!is.null(results))
		write.csv(results,file="4.csv")
	results <- gtrends(query=y,start_date=as.Date(vin[8]),end_date=as.Date(vin[9]))$trend
	print(results)
	if(!is.null(results))
		write.csv(results,file="5.csv")
	results <- gtrends(query=y,start_date=as.Date(vin[9]),end_date=as.Date(vin[10]))$trend
	print(results)
	if(!is.null(results))
		write.csv(results,file="6.csv")
	setwd('..')
	setwd('..')
}
collect_info = function()
{
auth <- gconnect("sirmvit.movieanalytics@gmail.com","zykowod3!")
vinod <- read.csv(file="data_test.csv",header=TRUE,sep=",")
vin <- vinod
i <- sapply(vinod, is.factor)
vin[i] <- lapply(vinod[i], as.character)
do.call(rbind,apply(vin,1,function(x) TrendFinder(x,auth)))
}