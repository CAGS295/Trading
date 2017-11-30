
##### 
pkg <- c("base","downloader","dplyr","fBasics","forecast","grid",
         "gridExtra","httr","jsonlite","lmtest","lubridate","moments",
         "matrixStats", "PerformanceAnalytics","plyr","plotly","quantmod",
         "reshape2","RCurl", "stats","scales","tseries",
         "TTR","TSA","XML","xts","zoo","yaml","RCurl")

inst <- pkg %in% suppressMessages(installed.packages())
if(length(pkg[!inst]) > 0) install.packages(pkg[!inst])
instpackages <- lapply(pkg, library, character.only=TRUE)
source('fs.R')
 #Paqueterias
#####
# RawGitHub <- "https://raw.githubusercontent.com/IFFranciscoME/"
# ROandaAPI <- paste(RawGitHub,"ROandaAPI/master/ROandaAPI.R",sep="")
# downloader::source_url(ROandaAPI,prompt=FALSE,quiet=TRUE)

#Params
#####
K=100000;n=21;d=3;dd=5;sig=5;atr=5;thrsh=.0000075;upp_lim=35;low_lim=-35;flag=0;lever=50;exposure=.1*lever;
grn="S5";align=17;tz="America/Mexico_City" 
#Account
##### 
conf=yaml.load_file('OANDAkey.yaml')
type=conf$ACC_TYPE
ID= conf$ACC_ID
token=conf$API_KEY_OANDA
inst=conf$PAIR   
#Aux<-Acc_info(type,ID,token)
#Model
#####
tr_size=n;
flag1=TRUE;

s=HPrice(type,grn,align,tz,token,inst,Count = 1)
while(flag1){
  #
  try({
    aux<-HPrice(type,grn,align,tz,token,inst,Count = 1)
    if(s[dim(s)[1],5] != aux[1,5]){
      s=rbind(s,aux)
      if(dim(s)[1] == (tr_size+1)) {s=s[-1,]}
      if(dim(s)[1]>=tr_size){ #modelo
        atr_=ATR(s[,3:5],atr)[tr_size,2]
        idx=SMI(s[,3:5],n,d,dd,sig)[(tr_size-3):tr_size,]
        if(atr_>thrsh){
          #STR1(idx)
          OrderHandler(STR1(idx),.15)
        }
      }
    }
  })
  
}
#####







