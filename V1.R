#Se cargan y se instalan los paquete necesarios para realizar nuestro código.
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

#En esta parte se definen los paramentros necesarios para llevar a cabo nuestras estrategias asi como los paramentros de nuestra posicion en la cuenta.

#Params
#####
K=100000;n=21;d=3;dd=5;sig=5;atr=5;thrsh=.000075;upp_lim=35;low_lim=-35;flag=0;lever=50;exposure=.1*lever;
grn="S5";align=17;tz="America/Mexico_City" 

#En esta parte se cargan los parametros de la cuenta, esto con la finalidad de poder realizar operaciones.

#Account
##### 
conf=yaml.load_file('OANDAkey.yaml')
type=conf$ACC_TYPE
ID= conf$ACC_ID
token=conf$API_KEY_OANDA
inst=conf$PAIR   
#Aux<-Acc_info(type,ID,token)


#Esta parte de codigo es la que hace que funcione en tiempo real, el cual consta de un ciclo que se va actualizando, donde descarga 
#un precio historico y lo guarda en la variable s. En el siguiente instante vuelve a pedir otro precio historico el cual lo compara 
#con el primer precio descargado, si el precio es igual se descarta, pero si es distinto se incluye en la variable aux, la cual sera 
#llenada hasta alcanzar el numero de variables necesarias para realizar los primeros calculos del modelo. Cuando se llega al limite 
#de la variable y otro precio historico cumple la condicion para entrar, se descarta el valor mas antiguo y se incluye el nuevo.

#En seguida se calcula el ATR, el cual no es mÃ¡s que un indicador tecnico que mide la volatilidad del mercado. Despues calculamenos
#el SMI el cual es indice de momento estocastico, uno de los indices mas utilizados al momento de realizar trading, y
#se define en la variable idx. Posteriormente con un if se condiciona que si el ATR es mayor que la comision (thrsh) se procede
#con la funcion  Order Handler, la cual realiza las señales de compra, venta o mantener, dependiendo de lo que arroje nuestro modelo
#definido en la funcion STR1.

#Cabe señalar que el thrsh es un parametro que indica las comisiones por operacion, entonces la finalidad del if que incluye al
#atr_ y al thrsh, es que no se den señales falsas que solo te hagan perdier dinero, sino que se deba cumplir la condicion de que
#la volatilidad presente en la operacion sea mayor al costo por operacion.
#Model
#####
tr_size=n;
flag1=TRUE;

s=HPrice(type,grn,align,tz,token,inst,Count = 1)
while(flag1){
  try({
    aux<-HPrice(type,grn,align,tz,token,inst,Count = 1)
    if(s[dim(s)[1],5] != aux[1,5]){
      s=rbind(s,aux)
      if(dim(s)[1] == (tr_size+1)) {s=s[-1,]}
      if(dim(s)[1]>=tr_size){ #modelo
        atr_=ATR(s[,3:5],atr)[tr_size,2]
        idx=SMI(s[,3:5],n,d,dd,sig)[(tr_size-3):tr_size,]
        #sprintf("ATR: %f\n",atr_)
        if(atr_>thrsh){
          OrderHandler(STR1(idx),.15)
        }
      }
    }
  })
  
}
#####







