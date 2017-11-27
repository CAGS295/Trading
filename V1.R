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
K=100000;tr_size=21;d=3;dd=5;sig=5;atr=5;thrsh=.075;upp_lim=35;low_lim=-35;flag=0;inst="USD_MXN";lever=50;exposure=.1*lever;
grn="S5";align=17;tz="America/Mexico_City"
#####
#Account

conf=yaml.load_file('OANDAkey.yaml')
type=conf$ACC_TYPE
ID= conf$ACC_ID
token=conf$API_KEY_OANDA
inst=conf$PAIR

#Aux<-AccountInfo(type,ID,token)

#####
flag1=TRUE;
s=HisPrices(type,grn,align,tz,token,inst,Count = 1)
while(flag1){
  aux=HisPrices(type,grn,align,tz,token,inst,Count = 1)
  if(s[1,1] != aux[1,1]){
    s=rbind(s,aux)
    if(dim(s)[1] == (tr_size+10)) {s=s[-dim(s)[1],]}
    if(dim(s)[1]>=tr_size){ #modelo
      idx=SMI(s[,c(3:5)],tr_size,d,dd,sig)
      
    }
  }
}
#####












