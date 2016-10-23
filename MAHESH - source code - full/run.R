if (!require("shiny")) {
  install.packages("shiny", repos="http://cran.rstudio.com/") 
  library("shiny")
}
if (!require("shinythemes")) {
  install.packages("shinythemes", repos="http://cran.rstudio.com/") 
  library("shinythemes")
}
if (!require("zoo")) {
  install.packages("zoo", repos="http://cran.rstudio.com/") 
  library("zoo")
}
folder_address = 'C://Users//Horopter//Documents//mahesh'
runApp(folder_address, launch.browser=TRUE)