#install.packages('gtrendsR')
library('gtrendsR')
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
cat('\014')
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

collect_info()