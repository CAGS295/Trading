
#Dicha función es nuestro modelo, el cual se basa en SMI (Stochastic Momentum Index), donde arroja señales de compra,
#venta o de mantenerse dependiendo del cruce que tengan las medias móviles exponenciales y si cruzan en el momento siguiente los niveles 
#establecidos como criterio de sobreventa. Se tiene el vector de SMI el cual si en la posición i es mayor que el límite superior y 
#al mismo tiempo el límite superior es menor que la señal entonces es 1, si el vector SMI en la posición i es menor que el limite inferior
#y la señal es menor al limite inferior entonces es menos -1, por último si no sucede ninguna de las dos es 0.

#Una vez teniendo dichos valores se prosigue con un if, donde si el intervalo en la posición i-1 es diferente de cero o el intervalo
#en la posición i es distinto de cero y el cruce en la posición i es distinto de cero entonces hubo un cruce, en ese momento se hace
#standby y se prosigue a otro if donde si el int iff es distinto de cero entra a otro if que busca si el int_diff es igual a menos uno
#y la vendera también entonces manda señal de venta, si las dos son uno es señal de compra, si no es ninguna de las dos se queda en 
#standby.

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

ClosePosition <- function(AccountType, AccountID, Token, Inst)
{
  
  if(AccountType == "practice") {
    httpaccount <- "https://api-fxpractice.oanda.com"
  } else 
    if(AccountType == "live") {
      httpaccount <- "https://api-fxtrade.oanda.com"
    } else print("Account type error. Must be practice or live")
  
  Queryhttp  <- paste(httpaccount,"/v1/accounts/", sep = "")
  Queryhttp1 <- paste(Queryhttp,AccountID, sep = "")
  Queryhttp2 <- paste(Queryhttp1,"/positions/", sep = "")
  Queryhttp3 <- paste(Queryhttp2,Inst, sep = "")
  
  auth  <- c(Authorization = paste("Authorization: Bearer",Token, sep=" "))
  
  DELETEPosition <- httpDELETE(Queryhttp3, cainfo=system.file("CurlSSL","cacert.pem",
                                                              package="RCurl"), httpheader=auth)
  FinalData  <- fromJSON(DELETEPosition, simplifyDataFrame = TRUE)
  
  return(FinalData)
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

#OrderHandler es la función que se encarga de ejecutar las señales de compra, venta o de mantener, así como de cerrar posiciones. Dicha
#función ejecuta opciones de compra del 15% del capital disposible, con lo cual se resuelve el problema de terminar con el capital
#disponible, ya que se estaría creando un ciclo infinito.
#Esta funcion se lleva a cabo de manera correcta gracias a que recibe las señales de nuestro modelo propueto.
#Dicha función basicamente pregunta si hay operaciones abiertas, si no las hay abre la operación que indica el modelo, si existe una
#operación pregunta cual es ese, y si resulta que es la misma  vuelve a comprar, pero si es la contraría cierra dicha operación y compra
#o vende en ese mismo momento, dependiendo de la señal que arroje el modelo.

OrderHandler<-function(direction,rate=.15,margin=.025){
   
   try({if(direction=='standby') return();
     ask=InstPos(type,ID,token)$positions$side
     if(is.null(ask) ||  direction == ask){
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


