
STR1<- function(smi){
  control = data.frame(smi,VC=NA,Cruce=NA,Intervalo=NA,Int_diff=NA,Posicion=NA)
  control$VC=ifelse((control$SMI-control$signal)>0, 1, 0)
  control$Cruce=c(NA,diff(control$VC))
  #control$Posicion[n]=0
  return(control)
}