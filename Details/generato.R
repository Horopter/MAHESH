setwd('C:/Users/Horopter/Downloads')
dir.create(file.path('HTML'), showWarnings = FALSE)
f0 <- read.csv(file='data3_train.csv',sep=",",header=TRUE)
f1 <- read.csv(file='app_data_train.csv',sep=",",header=TRUE)
f2 <- read.csv(file='predict2.csv',sep=",",header=TRUE,row.names=1)
f3 <- cbind.data.frame(f1,earn=f2[,2])
f <- cbind.data.frame(m=f3$movies,w=f3$wiki,y=f3$yt.links,e=f3$earn)
id <- substring(f3$yt.links, 33)
library(stringr)
wiki <- str_split_fixed(f$w, "/wiki/", 2)
wiki <- wiki[,2]
label <- cbind.data.frame(m=f3$movies,id=id, wiki=wiki,earn=f$e,g=f0$gross)
s1 <- "<html>\n<head>\n<link rel=\"stylesheet\" href=\"http://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css\">\n<script src=\"https://ajax.googleapis.com/ajax/libs/jquery/1.12.2/jquery.min.js\"></script>\n<script src=\"http://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/js/bootstrap.min.js\"></script>\n<style>\nul {\nlist-style:none;\n}\n</style>\n</head>\n<body>\n<script type=\"text/javascript\">\nvar searchTerm=\""
s2 <- "\";\nvar url=\"http://en.wikipedia.org/w/api.php?action=parse&format=json&page=\"+searchTerm+\"&prop=text&callback=?\";\n$.getJSON(url,function(data){\n wikiHTML = data.parse.text[\"*\"];\n $wikiDOM = $(\"<document>\"+wikiHTML+\"</document>\");\n $text = $wikiDOM.find(\'.infobox\').html();\n $text = $text.replace(/\\n/g,\"\");\n $text = $text.replace(/href=\\\"\\/wiki\\//g,\"href=\\\"http://en.wikipedia.com/wiki/\");\n $text = $text.replace(/src=\\\"\\/\\/upload/g,\"src=\\\"https:\\/\\/upload\");\n $text = $text.replace(/\\[\\d+\\]/g, \"\");\n $(\"#div_text\").append($text);\n});\n</script>\n<div class=\"col-lg-12\"><h1 class=\"bg-primary text-center\"> Summary </h1></div>\n<div id=\"div_text\" class=\"col-sm-12\" align=\"center\" bgcolor=\"#D6BF86\"></div>\n<br/>\n<br/>\n<div class=\"col-lg-12\"><h1 class=\"bg-primary text-center\"> Trailer </h1></div>\n<br/>\n<br/>\n<br/>\n<div class=\"col-sm-12\" align=\"center\">\n<div class=\"embed-responsive embed-responsive-16by9\" align=\"center\">\n <iframe class=\"embed-responsive-item\" src=\"https://www.youtube.com/embed/";
s3 <- "\" align=\"center\"></iframe>\n</div>\n</div>\n<div class=\"col-lg-12\"><h1 class=\"bg-primary text-center\"> Prediction </h1></div>\n<div class=\"col-sm-12\" align=\"center\">\n<pre>Our prediction of revenue for this movie is  ";
s4 <- "   Crores INR.";
s5 <- "\n The movie earned ";
s6 <- " Crores INR at the box office (We aren't considering non-theatrical business).\nError of +2% means that the movies released from two weeks prior didn\'t perform so well.\nError of +5% means that the movies released from four weeks prior didn\'t perform so well.\nError of +10% means that this movie exceeds expectations.\nError of +15% and above means that this movie is a blockbuster.\nError of -2% to -5% means that this movie lost to its competitors in its month of release.\nError of -10% means that this movie is an average hit.\nError of -15% and below means that this movie is a flop.\n{ Estimate is given in terms of how much can be earned in India. Hence non-hindi films may differ in verdict. }\n</pre>\n</div>\n</body>\n</html>";
apply(label, 1, function(x){
  filey <- paste0(s1,x[3],s2,x[2],s3,x[4],s4,s5,x[5],s6)
  write(filey,file=paste0('HTML/',x[1],'.html'))
})
print(label)