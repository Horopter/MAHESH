# This is the web sraping code for extraction of dataset

# the libraries used
library(rvest)
library(plyr)
library(stringr)
library(RJSONIO)
library(MASS)
library(car)

# creating a corpus
if (!file.exists("MovieData")) {
  dir.create("MovieData")
}
setwd("MovieData")

# creating a vector for the year from which movies are required
year <- c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014)

# Scraping the top 100 highest crossing movies per year from boxofficemojo.com, 
# reference: http://www.boxofficemojo.com/yearly/

for( i in 1:length(year)){
  urlmov <- paste0("http://www.boxofficemojo.com/yearly/chart/?yr=", year[i], "&p=.html")
  movie_name <- read_html(urlmov)%>%               # The name of the movies
    html_nodes("td td b font a")%>%
    html_text()
  
  gross_earning <- read_html(urlmov)%>%            # The gross earning by boxofficemojo
    html_nodes("td td tr+ tr td+ td font b")%>%
    html_text()
  
  theater <- read_html(urlmov)%>%                  # The number of total number of theaters/screens, movie was shown
    html_nodes("tr+ tr td:nth-child(5) font")%>%
    html_text()
  
  moviedata <- data.frame(movie_names= movie_name, gross_earning= gross_earning, theatre_count= theater[3:102], yearofrelease = year[i])
  
  filename <- paste0(year[i], ".csv")
  sink(file = filename) %>% # open file to write
    cat(write.csv(moviedata))
  sink()
}

# combining all datasets together together in one dataframe
files_data <- list.files(getwd(), pattern = ".csv")
moviesDF <- do.call(rbind, lapply(files_data, read.csv))

# To extract other variable we have use restful API of imdb which can be found on the link below
# http://www.omdbapi.com/ 
# to extract data as per the movie name following operations are performed on the movie name column
# to make it more readable as per the url requirement of the "omdbapi"
#
moviesDF$new_name <- str_replace_all(moviesDF$movie_names, "[\\?!]", "")
moviesDF$new_name <- str_replace(moviesDF$new_name, "\\((.*?)\\)", "")
moviesDF$new_name <- str_trim(moviesDF$new_name, side = "both")
moviesDF$new_name <- str_replace_all(moviesDF$new_name, "2000$", "")
moviesDF$new_name <- str_trim(moviesDF$new_name, side = "right")
moviesDF$new_name <- str_replace_all(moviesDF$new_name,"[^a-zA-Z0-9\\-'.,]+" , "+")
moviesDF$new_name <- str_replace(moviesDF$new_name,"^The\\+" , "")
moviesDF$new_name <- str_replace(moviesDF$new_name, "^Tyler\\+Perry's\\+", "")

# while due to some differences in the movie names, following specific string transformation
# functions are performed on the movie names to make them more readable
#
moviesDF$new_name[433] <- str_to_lower(moviesDF$new_name[433])
str_sub(moviesDF$new_name[991], start = 1, end = -1) <- "9+"
moviesDF$new_name[c(229,1076, 1023)] <- str_replace(moviesDF$new_name[c(229,1076, 1023)], "\\-", "")
moviesDF$new_name[c(24, 153, 332, 426, 550, 552, 723, 814, 875, 917, 1090, 1131, 1364, 1370, 
                    1399)] <- str_replace(moviesDF$new_name[c(24, 153, 332, 426, 550, 552, 723, 814, 875, 917, 1090, 1131,
                                                              1364, 1370, 1399)], "and\\+", "")
str_sub(moviesDF$new_name[c(34, 1201)], start = 1, end = 9) <- ""
str_sub(moviesDF$new_name[59], start = 8, end = -1) <- ""
str_sub(moviesDF$new_name[698], start = 9, end = -1) <- "+2"
str_sub(moviesDF$new_name[c(436,965)], start = 9, end = -1) <- ""
str_sub(moviesDF$new_name[157], start = 5, end = 6) <- "13"
str_sub(moviesDF$yearofrelease[188], start = 1, end = -1) <- "2000"
str_sub(moviesDF$yearofrelease[c(365,366)], start = 1, end = -1) <- "2002"
str_sub(moviesDF$yearofrelease[489], start = 1, end = -1) <- "2003"
str_sub(moviesDF$yearofrelease[489], start = 1, end = -1) <- "2003"
str_sub(moviesDF$yearofrelease[c(510,930)], start = 1, end = -1) <- "2007"
str_sub(moviesDF$yearofrelease[668], start = 1, end = -1) <- "2005"
str_sub(moviesDF$yearofrelease[1096], start = 1, end = -1) <- "2009"
str_sub(moviesDF$yearofrelease[1164], start = 1, end = -1) <- "2010"
str_sub(moviesDF$yearofrelease[c(1265, 1273)], start = 1, end = -1) <- "2011"
moviesDF$new_name[231] <- str_replace(moviesDF$new_name[231], "The\\+", "")
str_sub(moviesDF$new_name[306], start = 1, end = -1) <- "X-Men+2"
str_sub(moviesDF$new_name[319], start = 10, end = 11) <- "3-D"
str_sub(moviesDF$new_name[570], start = -2, end = -1) <- "3-D"
moviesDF$new_name[366]<- str_c(moviesDF$new_name[366], "...", sep= "")
moviesDF$new_name[433]<- str_c("AVP+", moviesDF$new_name[433], sep= "")
moviesDF$new_name[570]<- str_c("The+", moviesDF$new_name[570], sep= "")
str_sub(moviesDF$new_name[510], start = 4, end = 4) <- "+and+"
str_sub(moviesDF$new_name[1045], start = -4, end = -4) <- "+and+"
str_sub(moviesDF$new_name[570], start = -6, end = -4) <- ""
str_sub(moviesDF$new_name[634], start = 9, end = -1) <- ""
str_sub(moviesDF$new_name[c(670,1256)], start = -3, end = -1) <- ""
str_sub(moviesDF$new_name[848], start = -5, end = -1) <- ""
str_sub(moviesDF$new_name[c(810,1211)], start = 1, end = 11) <- ""
str_sub(moviesDF$new_name[946], start = 1, end = -11) <- ""
str_sub(moviesDF$new_name[748], start = 2, end = 2) <- "%3A"
str_sub(moviesDF$new_name[961], start = -4, end = -1) <- ""
str_sub(moviesDF$new_name[1297], start = 8, end = 8) <- "!+"

# removing some of the invalid movie's names
moviesDF <- moviesDF[-c(61, 993, 551, 1133, 1278, 1282, 1294), ] # 1278(TV Episode), 1282(TV episode), 1294(TV episode)

# Scraping the data using 'omdbapi'
for(i in 1:nrow(moviesDF)){
  movie <- fromJSON(paste0("http://www.omdbapi.com/?t=", moviesDF$new_name[i], "&y=", moviesDF$yearofrelease[i], "&tomatoes=true&r=json"))
  moviesDF$IMDB_Rating[i] <- movie[[16]]        # the imdb rating of the each movie
  moviesDF$Genre[i] <- movie[[6]]               # the Genre to which the movie belongs
  moviesDF$Tomato_Meter[i] <- movie[[20]]       # the Rotten Tomatoes movie Meter
  moviesDF$Tomato_Rating[i] <- movie[[22]]      # The Rotten Tomatoe's movie rating
  moviesDF$Tomato_UserMeter[i] <- movie[[27]]   # The Rotten Tomatoe's User Meter
  moviesDF$Tomato_UserRating[i] <- movie[[28]]  # The Rotten Tomatoe's User Rating
  moviesDF$Rated[i] <- movie[[3]]               # Which rating the movie belonged to
  moviesDF$BoxOffice[i] <- movie[[31]]          # The box office gross extimated by rotten tomatoes
}
#
# Dropping the unnecessary columns
drops <- c("new_name", "X", "BoxOffice")
moviesDF <- moviesDF[ ,!(names(moviesDF) %in% drops)]

#----------------------
# scraping the other movie data required
# scraping ticket prices
tckt_year <- read_html('http://natoonline.org/data/ticket-price/')%>%
  html_nodes('.column-1')%>%
  html_text()

tckt_pr <- read_html('http://natoonline.org/data/ticket-price/')%>%
  html_nodes('.column-2')%>%
  html_text()

price.data <- data.frame(year= tckt_year, price= tckt_pr)
price.data <- price.data[2:16, ]

#------------------------
# scraping total Box Office earning for the entire year
box_year <- read_html('http://natoonline.org/data/boxoffice/')%>%
  html_nodes('.column-1')%>%
  html_text()

tot_earn <- read_html('http://natoonline.org/data/boxoffice/')%>%
  html_nodes('.column-2')%>%
  html_text()

boxoffice_earning <- data.frame(year= box_year, price= tot_earn)
boxoffice_earning <- boxoffice_earning[2:16, ]

#------------------------
# Scraping the total number of screen available in U.S.
year <- read_html('http://natoonline.org/data/us-movie-screens/')%>%
  html_nodes('.column-1')%>%
  html_text()

tot_indoor <- read_html('http://natoonline.org/data/us-movie-screens/')%>%
  html_nodes('.column-2')%>%
  html_text()

tot_drivein <- read_html('http://natoonline.org/data/us-movie-screens/')%>%
  html_nodes('.column-3')%>%
  html_text()

tot_screen <- read_html('http://natoonline.org/data/us-movie-screens/')%>%
  html_nodes('.column-4')%>%
  html_text()

tot.screen.data<- data.frame(year= year, indoor_screen=tot_indoor, drivein_screen= tot_drivein, 
                             total_screen= tot_screen)
tot.screen.data <- tot.screen.data[2:16, ]

#-------------------------------------

#Do some data cleaning and tranformation of data types
moviesDF$gross_earning <- as.integer(gsub("[$,]","",moviesDF$gross_earning))
moviesDF$theatre_count <- as.integer(gsub(",","",moviesDF$theatre_count))
moviesDF[moviesDF=="N/A"] <- NA
moviesDF <- transform(moviesDF, yearofrelease= as.numeric(yearofrelease), 
                      IMDB_Rating= as.numeric(IMDB_Rating), Tomato_UserRating= as.numeric(Tomato_UserRating),
                      Tomato_UserMeter= as.numeric(Tomato_UserMeter),Tomato_Rating= as.numeric(Tomato_Rating),
                      Tomato_Meter= as.numeric(Tomato_Meter))

# ---------------------------- Conversion of Genre to factors in Regression------------------

#Split the genre by comma
genre <-strsplit( moviesDF$Genre,',')
maxLen <- max(sapply(genre, length))
genre1 <- rep(NA,dim(moviesDF)[1])
genre2 <- rep(NA,dim(moviesDF)[1])
genre3 <- rep(NA,dim(moviesDF)[1])
for(i in 1:dim(moviesDF)[1]){
  if(!is.na(genre[[i]][1])){
    genre1[i] <- genre[[i]][1]
  }
  if(!is.na(genre[[i]][2])){
    genre2[i] <- genre[[i]][2]
  }
  if(!is.na(genre[[i]][3])){
    genre3[i] <- genre[[i]][3]
  }
}

#get rid of white space
genre1 <- str_trim(genre1)
genre2 <- str_trim(genre2)
genre3 <- str_trim(genre3)

#Find the categories of genre
table(genre1)
table(genre2)
table(genre3)

#create dummy variables
Action <- rep(0,dim(moviesDF)[1])
Adult <- rep(0,dim(moviesDF)[1])
Adventure <- rep(0,dim(moviesDF)[1])
Animation <- rep(0,dim(moviesDF)[1])
Biography <- rep(0,dim(moviesDF)[1])
Comedy <- rep(0,dim(moviesDF)[1])
Crime <- rep(0,dim(moviesDF)[1])
Documentary <- rep(0,dim(moviesDF)[1])
Drama <- rep(0,dim(moviesDF)[1])
Family <- rep(0,dim(moviesDF)[1])
Fantasy <- rep(0,dim(moviesDF)[1])
History <- rep(0,dim(moviesDF)[1])
Horror <- rep(0,dim(moviesDF)[1])
Music <- rep(0,dim(moviesDF)[1])
Musical <- rep(0,dim(moviesDF)[1])
Mystery <- rep(0,dim(moviesDF)[1])
Romance <- rep(0,dim(moviesDF)[1])
SciFi <- rep(0,dim(moviesDF)[1])
Short <- rep(0,dim(moviesDF)[1])
Sport <- rep(0,dim(moviesDF)[1])
Thriller <- rep(0,dim(moviesDF)[1])
War <- rep(0,dim(moviesDF)[1])
Western <- rep(0,dim(moviesDF)[1])

# splitting the genre into factors
for(i in 1:dim(moviesDF)[1]){
  if(any(genre1[i]=='Action',genre2[i]=='Action',genre3[i]=='Action',na.rm =T)){
    Action[i] <- 1
  }
  if(any(genre1[i]=='Adult',genre2[i]=='Adult',genre3[i]=='Adult',na.rm =T)){
    Adult[i] <- 1
  }
  if(any(genre1[i]=='Adventure',genre2[i]=='Adventure',genre3[i]=='Adventure',na.rm =T)){
    Adventure[i] <- 1
  }
  if(any(genre1[i]=='Animation',genre2[i]=='Animation',genre3[i]=='Animation',na.rm =T)){
    Animation[i] <- 1
  }
  if(any(genre1[i]=='Biography',genre2[i]=='Biography',genre3[i]=='Biography',na.rm =T)){
    Biography[i] <- 1
  }
  if(any(genre1[i]=='Comedy',genre2[i]=='Comedy',genre3[i]=='Comedy',na.rm =T)){
    Comedy[i] <- 1
  }
  if(any(genre1[i]=='Crime',genre2[i]=='Crime',genre3[i]=='Crime',na.rm =T)){
    Crime[i] <- 1
  }
  if(any(genre1[i]=='Documentary',genre2[i]=='Documentary',genre3[i]=='Documentary',na.rm =T)){
    Documentary[i] <- 1
  }
  if(any(genre1[i]=='Drama',genre2[i]=='Drama',genre3[i]=='Drama',na.rm =T)){
    Drama[i] <- 1
  }
  if(any(genre1[i]=='Family',genre2[i]=='Family',genre3[i]=='Family',na.rm =T)){
    Family[i] <- 1
  }
  if(any(genre1[i]=='Fantasy',genre2[i]=='Fantasy',genre3[i]=='Fantasy',na.rm =T)){
    Fantasy[i] <- 1
  }
  if(any(genre1[i]=='History',genre2[i]=='History',genre3[i]=='History',na.rm =T)){
    History[i] <- 1
  }
  if(any(genre1[i]=='Horror',genre2[i]=='Horror',genre3[i]=='Horror',na.rm =T)){
    Horror[i] <- 1
  }
  if(any(genre1[i]=='Music',genre2[i]=='Music',genre3[i]=='Music',na.rm =T)){
    Music[i] <- 1
  }
  if(any(genre1[i]=='Musical',genre2[i]=='Musical',genre3[i]=='Musical',na.rm =T)){
    Musical[i] <- 1
  }
  if(any(genre1[i]=='Mystery',genre2[i]=='Mystery',genre3[i]=='Mystery',na.rm =T)){
    Mystery[i] <- 1
  }
  if(any(genre1[i]=='Romance',genre2[i]=='Romance',genre3[i]=='Romance',na.rm =T)){
    Romance[i] <- 1
  }
  if(any(genre1[i]=='Sci-Fi',genre2[i]=='Sci-Fi',genre3[i]=='Sci-Fi',na.rm =T)){
    SciFi[i] <- 1
  }
  if(any(genre1[i]=='Short',genre2[i]=='Short',genre3[i]=='Short',na.rm =T)){
    Short[i] <- 1
  }
  if(any(genre1[i]=='Sport',genre2[i]=='Sport',genre3[i]=='Sport',na.rm =T)){
    Sport[i] <- 1
  }
  if(any(genre1[i]=='Thriller',genre2[i]=='Thriller',genre3[i]=='Thriller',na.rm =T)){
    Thriller[i] <- 1
  }
  if(any(genre1[i]=='War',genre2[i]=='War',genre3[i]=='War',na.rm =T)){
    War[i] <- 1
  }
  if(any(genre1[i]=='Western',genre2[i]=='Western',genre3[i]=='Western',na.rm =T)){
    Western[i] <- 1
  }
}

#-------------------------------------------------------------------------------------------
# creating a new dataframe
film <- cbind(moviesDF,as.factor(Action),as.factor(Adult),as.factor(Adventure),as.factor(Animation),as.factor(Biography),as.factor(Comedy),
              as.factor(Crime), as.factor(Documentary),as.factor(Drama),as.factor(Family),as.factor(Fantasy),as.factor(History),as.factor(Horror),
              as.factor(Music),as.factor(Musical), as.factor(Mystery),as.factor(Romance),as.factor(SciFi), as.factor(Short),as.factor(Sport),as.factor(Thriller),
              as.factor(War),as.factor(Western) )

my_names<-c("Name","gross_earning","theatre_count","year","IMDB_Rating","Genre","Tomato_Meter","Tomato_Rating","Tomato_User_Meter",
            "Tomato_User_Rating","MPAA_Rating","Action","Adult","Adventure","Animation","Biography","Comedy","Crime",
            "Documentary","Drama","Family","Fantasy","History","Horror","Music","Musical","Mystery","Romance","SciFi","Short","Sport","Thriller","War","Western")
names(film) <- my_names
names(film)

# ----------------------------------------Model Creation------------------------------------
# Initial full model
lm1 <- lm(gross_earning~ theatre_count+IMDB_Rating+Tomato_Meter+Tomato_Rating+Tomato_User_Meter
          +Tomato_User_Rating+MPAA_Rating+Action+Adventure+Animation+Biography+Comedy
          +Crime+Documentary+Drama+Family+Fantasy+History+Horror+Music+Musical+
            Mystery+ Romance+ SciFi+ Short+ Sport+ Thriller+War+Western, data= film)
summary(lm1)

# adjusting the inflation value
price.data$price <- as.numeric(gsub("[$]","",price.data$price))
price.data$inflation <- price.data$price/price.data$price[15]

# adding to the film Database
film <- merge(price.data,film,by='year')
# calculating the number of tickets sold
film$num_of_tickt_sold <- film$gross_earning/film$price
# calculating the gross earning for each movie after Inflation adjustment
film$gross_earning_after <- film$gross_earning/(film$inflation)

# creating a sample
s <- sample(1:nrow(film), 800)
film.train <- film[s, ]
film.test <- film[-s, ]

# full model after inflation adjustment
lm2 <- lm(gross_earning_after~theatre_count+IMDB_Rating+Tomato_Meter+Tomato_Rating+
            Tomato_User_Meter+Tomato_User_Rating+Action+Adventure+Animation+Biography+
            Comedy+Crime+Documentary+Drama+Family+Fantasy+History+Horror+Music+Musical+
            Mystery+Romance+SciFi+Short+Sport+Thriller+War+Western,data=film.train)
summary(lm2)
# the full model shows an R-square value of .51

pred <- predict(lm2, newdata = film.test)
mse <- mean((film.test$gross_earning_after-pred)^2, na.rm = TRUE)

# 2nd model based upon the p-values of the variables
lm3 <- lm(gross_earning_after~theatre_count+Tomato_Meter+Tomato_Rating+Tomato_User_Meter+Tomato_User_Rating+
            Animation+Adventure+Drama+Fantasy+Music+Mystery+Sport+Western, data = film.train)
summary(lm3)
pred1 <- predict(lm3, newdata = film.test)
mse1 <- mean((film.test$gross_earning_after-pred1)^2, na.rm = TRUE)

# stepise selection
stepAIC(lm2, direction = "both")
# model after stepwise selection
lm4 <- lm(formula = gross_earning_after ~ theatre_count + Tomato_Meter + 
            Tomato_Rating + Tomato_User_Meter + Tomato_User_Rating + 
            Adventure + Animation + Drama + Family + Fantasy + Horror + 
            Music + Short + Sport + War + Western, data = film.train)
summary(lm4) # R-square == 0.506
pred2 <- predict(lm4, newdata = film.test)
mse2 <- mean((film.test$gross_earning_after-pred2)^2, na.rm = TRUE)

# trying the normality plots
r_student1 <- rstudent(model = lm4)
qqnorm(r_student1, xlab = "external standardized residuals",
       ylab = "Probability", main = "Normal Probablity plot")
qqline(r_student1)

# calculating the box cox
bc01 <- boxcox(lm4,plotit=T, interp=T)
bc01   # Lists log-likelihoods and corresponding lambdas.

maxlambda01 <- bc01$x[bc01$y==max(bc01$y)]
maxlambda01 #[1] -0.26

# checking multicollinearity in the model
vif(lm4)
# therefore, removing the Tomato_Rating and Tomato_User_Rating

# models after transformation, log transformation
lm5 <- lm(formula = gross_earning_after ~ theatre_count + Tomato_Meter + Tomato_User_Meter +
            Adventure + Animation + Drama + Family + Fantasy + Horror + Music + Short +
            Sport + War + Western, data = film.train)
summary(lm5) # R-square == 0.4958
pred3 <- predict(lm5, newdata = film.test)
mse3 <- mean((film.test$gross_earning_after-pred3)^2, na.rm = TRUE)

# normality plots
r_student2 <- rstudent(model = lm5)
qqnorm(r_student2, xlab = "extelm5l standardized residuals",
       ylab = "Probability", main = "Normal Probablity plot")
qqline(r_student2)

# checking for multicollinearity
vif(lm5)

# calculating the box cox
bc02 <- boxcox(lm5,plotit=T, interp=T)
bc02   # Lists log-likelihoods and corresponding lambdas.
bc02$out
maxlambda02 <- bc02$x[bc02$y==max(bc02$y)]
maxlambda02 #[1] -0.26

# the model based on box-cox value
lm6 <- lm(formula = ((gross_earning_after)^(-0.26)) ~ theatre_count + Tomato_Meter + Tomato_User_Meter +
            Adventure + Animation + Drama + Family + Fantasy + Horror + Music + Short +
            Sport + War + Western, data = film.train)
summary(lm6) # R-square == 0.54
pred4 <- predict(lm6, newdata = film.test)
mse4 <- mean((film.test$gross_earning_after-(pred4)^(1/(-0.26)))^2, na.rm = TRUE)

# trying the normality plots
r_student3 <- rstudent(model = lm6)
qqnorm(r_student3, xlab = "external standardized residuals",
       ylab = "Probability", main = "Normal Probablity plot")
qqline(r_student3)

# The 'log' Transformation
lm7 <- lm(formula = log(gross_earning_after) ~ theatre_count + Tomato_Meter + Tomato_User_Meter +
            Adventure + Animation + Drama + Family + Fantasy + Horror + Music + Short +
            Sport + War + Western, data = film.train)
summary(lm7) # R-square == 0.54
pred5 <- predict(lm7, newdata = film.test)
mse5 <- mean((film.test$gross_earning_after-exp(pred5))^2, na.rm = TRUE)

# trying the normality plots
r_student4 <- rstudent(model = lm7)
qqnorm(r_student4, xlab = "external standardized residuals",
       ylab = "Probability", main = "Normal Probablity plot")
qqline(r_student4)

# checking the contribution of each variable using the anova test

anova.lm7 <- aov(formula = log(gross_earning_after) ~ theatre_count + Tomato_Meter + Tomato_User_Meter +
                   Adventure + Animation + Drama + Family + Fantasy + Horror + Music + Short +
                   Sport + War + Western, data = film.train)
summary(anova.lm7)

# observing the F and P values we can remove some variables, as these are insignificant 
lm8 <- lm(formula = log(gross_earning_after) ~ theatre_count + Tomato_Meter + Tomato_User_Meter +
            Drama + Fantasy + Horror + Music + Short + Sport, data = film.train)
summary(lm8) # R-square == 0.54
pred6 <- predict(lm7, newdata = film.test)
mse6 <- mean((film.test$gross_earning_after-exp(pred6))^2, na.rm = TRUE)

# trying the normality plots
r_student4 <- rstudent(model = lm8)
qqnorm(r_student4, xlab = "external standardized residuals",
       ylab = "Probability", main = "Normal Probablity plot")
qqline(r_student4)

# Therefore, lm8 is the correct model.
#-----------------------------------------------------------------------------------------------------------------

# save the R session as image so that we can reuse it again.
save.image("LinearModelsProj1.RData")





# save the R session as image so that we can reuse it again.
save.image("LinearModelsProj1.RData")
load("LinearModelsProj1.RData")

#R rated movies
R <- film[which(film$MPAA_Rating == 'R'),]

r.model.full <- lm(gross_earning_after~theatre_count+IMDB_Rating+Tomato_Meter+Tomato_Rating+Tomato_User_Meter+Tomato_User_Rating+Action+Adventure+Animation+Biography+Comedy+Crime+Documentary+Drama+Fantasy+History+Horror+Music+Musical+Mystery+Romance+SciFi+Sport+Thriller+War+Western,data=R)
summary(r.model.full)

step <- stepAIC(r.model.full,direction="both")
step$anova

r.model <- lm(gross_earning_after ~ theatre_count + Tomato_Meter + Tomato_User_Meter  + Action+ Crime + Horror + Mystery + Sport + Thriller,data=R)
summary(r.model)

qqnorm(r.model$res)
qqline(r.model$res)

r.model.log <- lm(log(gross_earning_after) ~ theatre_count + Tomato_Meter + Tomato_User_Meter  + Action+ Crime + Horror + Mystery + Sport + Thriller,data=R)
summary(r.model.log)

#models for outlying movies
box <- boxplot(film$gross_earning_after)
#Subset the outlying movies based on boxplot and in total there are 122
film.outlier <- film[which(film$gross_earning_after %in% box$out),]
#Descending Order
film.order <- film.outlier[order(film.outlier$gross_earning_after,decreasing=T),]
#Model
model.outlier <- lm(gross_earning_after~theatre_count+IMDB_Rating+Tomato_Meter+Tomato_User_Meter+Action+Adventure+Animation+Biography+Comedy+Crime+Drama+Fantasy+Mystery+Romance+SciFi+Sport+Thriller,data=film.order)
summary(model.outlier)

model.outlier.reduce <- lm(gross_earning_after~theatre_count+IMDB_Rating+Tomato_Meter+Tomato_User_Meter+Drama+Fantasy,data=film.order)
summary(model.outlier.reduce)
