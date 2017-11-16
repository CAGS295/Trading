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

type <- "practice"    # Account Type
ID <- "1742531"     # Account ID
token <- "ada4a61b0d5bc0e5939365e01450b614-4121f84f01ad78942c46fc3ac777baa6"
inst<- "EUR_USD"
Aux<-AccountInfo(OA_At,OA_Ai,OA_Ak)
i=0;
flag=TRUE;
s=ActualPrice(type,token,"EUR_USD")
Sys.sleep(5)
while(flag){
  i=i+1;
  #s<- 
    s=rbind(s,ActualPrice(type,token,"EUR_USD")) 
    
    ##rbind(s,)
  Sys.sleep(5)
}
#####












