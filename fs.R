
STR1<- function(smi,pos=1){
  control = data.frame(smi,VC=NA,Cruce=NA,Intervalo=NA,Int_diff=NA)
  control$VC=ifelse((control$SMI-control$signal)>0, 1, 0)
  control$Cruce=c(NA,diff(control$VC))
  #control$Posicion[n]=0
  size=nrow(control)
  for(i in 1:size){
    
    if(control$SMI[i] > upp_lim && upp_lim < control$signal[i]){
      control$Intervalo[i]= 1
    }else if(control$SMI[i] < low_lim && control$signal[i] < low_lim){
      control$Intervalo[i]= -1
    }else{
      control$Intervalo[i]= 0
    }
  }
  control$Int_diff=c(NA,diff(control$Intervalo))
  
  for(i in size){
    
    if((control$Intervalo[i-1] != 0 || control$Intervalo[i] != 0) && control$Cruce[i] != 0){
      flag = control$Cruce[i]
      #hubo cruce
    }else{
      warning("standby",immediate. = TRUE)
      return('standby')
    }
    if(control$Int_diff[i] != 0){
      
      if(control$Int_diff[i] == -1 && flag == -1){
        warning("sell",immediate. = TRUE)
        return('sell')
      }else if(control$Int_diff[i] == 1  && flag == 1){
          warning("buy",immediate. = TRUE)
          return('buy')
      }else{warning("standby",immediate. = TRUE)
        return('stanby')}
    }else{warning("standby",immediate. = TRUE)
      return('standby')}
  }
  #return(control)
}



Norder <-function(AccountType,AccountID,Token,OrderType,Instrument,Count,Side,Expiry, Price)
{
  if(AccountType == "practice"){
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else print("Account type error. Must be practice or live")
  
  auth       <- c(Authorization = paste("Authorization: Bearer",Token,sep=" "))
  Queryhttp  <- paste(httpaccount,"/v1/accounts/",sep="")
  Queryhttp1 <- paste(Queryhttp,AccountID,sep="")
  Queryhttp2 <- paste(Queryhttp1,"/orders",sep="")
  
  if(OrderType == 'market'){
    Param <- c(instrument=Instrument, units=Count, side=Side, type=OrderType)#,stopLoss=SL, takeProfit=TP, trailingStop=TS)
  } else  if(OrderType == 'limit'){
    Param <- c(instrument=Instrument,units=Count,side=Side,type=OrderType,
               price=Price, expiry=Expiry, stopLoss=SL, takeProfit=TP, trailingStop=TS)
  } else  if(OrderType == 'stop'){
    Param <- c(instrument=Instrument,units=Count,side=Side,type=OrderType,
               price=Price, stopLoss=SL, takeProfit=TP, expiry=Expiry, trailingStop=TS)
  } else  if(OrderType == 'marketIfTouched'){
    Param <- c(instrument=Instrument,units=Count,side=Side,type=OrderType,
               price=Price, stopLoss=SL, takeProfit=TP, expiry=Expiry, trailingStop=TS)
  } else print("Order Type error. Must be: 'market', 'limit', 'stop', 'marketIfTouched'")
  
  PF <- postForm(Queryhttp2, style="POST", .params=Param,
                 .opts=list(httpheader=auth,ssl.verifypeer = FALSE))
  InstJson <- fromJSON(PF, simplifyDataFrame = TRUE)
  
  return(InstJson)
}


HPrice<-function(AccountType,Granularity,DayAlign,TimeAlign,Token,Instrument,
         Start,End,Count){
  if(AccountType == "practice"){
    httpaccount  <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount  <- "https://api-fxtrade.oanda.com"
    } else print("Account type error. Must be practice or live")
  
  if(!is.null(Count)) {
    
    auth           <- c(Authorization = paste("Bearer",Token,sep=" "))
    QueryHistPrec  <- paste(httpaccount,"/v1/candles?instrument=",sep="")
    QueryHistPrec1 <- paste(QueryHistPrec,Instrument,sep="")
    
    qcount  <- paste("count=",Count,sep="")
    
    qcandleFormat <- "candleFormat=midpoint"
    qgranularity  <- paste("granularity=",Granularity,sep="")
    qdailyalignment    <- paste("dailyAlignment=",DayAlign,sep="")
    
    QueryHistPrec2 <- paste(QueryHistPrec1,qcandleFormat,qgranularity,
                            qdailyalignment,qcount,sep="&")
    
  }
  
  else {
    
    auth           <- c(Authorization = paste("Bearer",Token,sep=" "))
    QueryHistPrec  <- paste(httpaccount,"/v1/candles?instrument=",sep="")
    QueryHistPrec1 <- paste(QueryHistPrec,Instrument,sep="")
    
    qstart <- paste("start=",Start,sep="")
    qend   <- paste("end=",End,sep="")
    
    qcandleFormat  <- "candleFormat=midpoint"
    qgranularity   <- paste("granularity=",Granularity,sep="")
    qdailyalignment    <- paste("dailyAlignment=",DayAlign,sep="")
    
    QueryHistPrec2 <- paste(QueryHistPrec1,qstart,qend,qcandleFormat,qgranularity,
                            qdailyalignment,sep="&")
  }
  
  InstHistP <- getURL(QueryHistPrec2,cainfo=system.file("CurlSSL","cacert.pem",
                                                        package="RCurl"),httpheader=auth)
  InstHistPjson <- fromJSON(InstHistP, simplifyDataFrame = TRUE)
  Prices        <- data.frame(InstHistPjson[[3]])
  Prices$time <- paste(substr(Prices$time,1,10),substr(Prices$time,12,19), sep=" ")
  colnames(Prices) <- c("TimeStamp","Open","High","Low","Close","TickVolume","Complete")
  Prices$TimeStamp <- as.POSIXct(strptime(Prices$TimeStamp, "%Y-%m-%d %H:%M:%OS"),
                                 origin="1970-01-01",tz = "UTC")
  attributes(Prices$TimeStamp)$tzone <- TimeAlign
  return(Prices)
}

InstPos<-function(AccountType,AccountID,Token)
{
  
  if(AccountType == "practice"){
    httpaccount  <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else print("Account type error. Must be practice or live")
  
  auth      <- c(Authorization = paste("Bearer",Token,sep=" "))
  Queryhttp <- paste(httpaccount,"/v1/accounts/",sep="")
  Querythttp1 <- paste(Queryhttp,AccountID,sep="")
  Querythttp2 <- paste(Querythttp1,"/positions",sep="")
  QueryInst1  <- getURL(Querythttp2,cainfo=system.file("CurlSSL","cacert.pem",
                                                       package="RCurl"),httpheader=auth)
  InstJson <- fromJSON(QueryInst1, simplifyDataFrame = TRUE)
  
  return(InstJson)
}

Acc_info<-function(AccountType,AccountID,Token)
{
  if(AccountType == "practice"){
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live"){
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else print("Account type error. Must be practice or live")
  
  auth      <- c(Authorization = paste("Bearer",Token,sep=" "))
  Queryhttp <- paste(httpaccount,"/v1/accounts/",sep="")
  QueryInfo <- paste(Queryhttp,AccountID,sep="")
  CtaInfo   <- getURL(QueryInfo,cainfo=system.file("CurlSSL",
                                                   "cacert.pem",package="RCurl"),httpheader=auth)
  CtaInfoJson <- fromJSON(CtaInfo, simplifyDataFrame = TRUE)
  return(CtaInfoJson)
}





OrderHandler<-function(direction,rate=.15,margin=.025){
  ##  exposure porcentaje de margen libre por invertir.
  ##  'check size' hay que sustituirse por el número de lotes de la operación
  ##  Puede ser que solo funciones con lotes enteros, se recomienda redondear si da error
  ##  hay que plantear una solución para guardar la información sin que consuma mucho tiempo de ejecución
  ##  se tiene que guardar en un archivo que se pueda recuperar
  ##  hay que asegurarse que la orden se ejecute solo si hay margen libre para hacerlo, ojo se puede
  ##  hacer un filtro para que solo se ejecute cuando hay margen disponible o no hacerlo y manejar la excepción con un try o trycatch.
  ##  
   try({if(direction=='standby') return();
     ask=InstPos(type,ID,token)$positions$side
     if(direction == ask){
       size=Acc_info(type,ID,token)$marginAvail*rate/margin;
       Norder(type,ID,token,OrderType = 'market',inst,formatC(size,format='d'),direction)
     }else {
       ClosePosition(type,ID,token,inst)
       if(ask == 'buy') direction = 'sell'
       else direction ='buy'
       size=Acc_info(type,ID,token)$marginAvail*rate/margin;
       Norder(type,ID,token,OrderType = 'market',inst,formatC(size,format='d'),direction)
     }
   })
}


