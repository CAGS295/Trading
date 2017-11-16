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

#####
#Params
K=100000;n=21;d=3;dd=5;sig=5;atr=5;thrsh=.075;upp_lim=35;low_lim=-35;flag=0;inst="USD_MXN";lever=50;exposure=.1*lever;
tr_size=100;
#####
#Account
type <- "practice"    # Account Type
ID <- "1742531"     # Account ID
token <- "ada4a61b0d5bc0e5939365e01450b614-4121f84f01ad78942c46fc3ac777baa6"
inst<- "EUR_USD"
#####
Aux<-AccountInfo(OA_At,OA_Ai,OA_Ak)

#####
flag=TRUE;
s=ActualPrice(type,token,"EUR_USD")
while(flag){
  aux=ActualPrice(type,token,"EUR_USD")
  if(s[1,1] != aux[1,1]){
    s=rbind(aux,s)
    if(length(s) == tr_size+1) s=s[-dim(s)[1],]
    if(dim(s)[1]>=tr_size){ #modelo
      
    }
  }
}
#####












