

##### Paqueterias
pkg <- c("base","downloader","dplyr","fBasics","forecast","grid",
         "gridExtra","httr","jsonlite","lmtest","lubridate","moments",
         "matrixStats", "PerformanceAnalytics","plyr","plotly","quantmod",
         "reshape2","RCurl", "stats","scales","tseries",
         "TTR","TSA","XML","xts","zoo","yaml")

inst <- pkg %in% suppressMessages(installed.packages())
if(length(pkg[!inst]) > 0) install.packages(pkg[!inst])
instpackages <- lapply(pkg, library, character.only=TRUE)

#####
RawGitHub <- "https://raw.githubusercontent.com/IFFranciscoME/"
ROandaAPI <- paste(RawGitHub,"ROandaAPI/master/ROandaAPI.R",sep="")
downloader::source_url(ROandaAPI,prompt=FALSE,quiet=TRUE)

<<<<<<< HEAD
hola<-1
hola2<-2
=======
a=1;
>>>>>>> 0d419e89ac35df149bd1ae8ef2e901aea130e69f
carlos<-2547