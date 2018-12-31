
###############################################
##### FUNCION DE LIMPIEZA E IMPUTACION ########
###############################################

imp<-function(nom_arch,pmin,pmax){
  #librarias necesarias
  library(readODS)
  library(tseries)
  library(forecast) 
  #Importa datos
  datos<-read_ods(nom_arch)
  #######################################
  ###Primeros datos significativos#######
  #######################################
  rmax<-data.frame(frec=rle(datos[,colnames(datos)=="TMAX"])$lengths,tmax=rle(datos[,colnames(datos)=="TMAX"])$values)
  null<--99.9
  p<-0
  s<-0
  while(s<10){
    p=p+1
    if(rmax[p,1]==1){s<-s+1}
    else{s<-s+0}
    }
  na_in<-c(datos[1:p,colnames(datos)=="TMAX"]==null,rep(FALSE,nrow(datos)-p))
  datos<-datos[!na_in,]
  ######################
  rmin<-data.frame(frec=rle(datos[,colnames(datos)=="TMIN"])$lengths,tmin=rle(datos[,colnames(datos)=="TMIN"])$values)
  p<-0
  s<-0
  while(s<10){
    p=p+1
    if(rmin[p,1]==1){s<-s+1}
    else{s<-s+0}
  }
  na_in<-c(datos[1:p,colnames(datos)=="TMIN"]==null,rep(FALSE,nrow(datos)-p))
  datos<-datos[!na_in,]
  
  #####################################################################
  #####################################################################
  ###Fecha de inicio
  inicio<-paste(datos$Año[1],datos$Mes[1],datos$Dia[1],sep="-")
  inds <- seq(as.Date(inicio), as.Date("2018-12-31"), by = "day") ## ultima fecha actualizable  
  ###### TMIN ####
  tmin<-datos[,colnames(datos)=="TMIN"]
  rmin<-data.frame(frec=rle(datos[,colnames(datos)=="TMIN"])$lengths,tmin=rle(datos[,colnames(datos)=="TMIN"])$values)
  for(i in 1:nrow(rmin)){
    if(rmin[i,2]==-99.9){
      ts2<-ts(tmin[1:sum(rmin[1:(i-1),1])], 
              start = c(datos$Año[1], as.numeric(format(inds[1], "%j")),calendar=TRUE),
              frequency = 365)
      mod<-Arima(ts2,order=pmin)
      tmin[(sum(rmin[1:(i-1),1])+1):sum(rmin[1:i,1])]<-as.vector(forecast(mod,h=rmin[i,1])$mean)
    }
    else{}
  }
  ###### TMAX ####
  inds <- seq(as.Date(inicio), as.Date("2018-12-31"), by = "day")
  tmax<-datos[,colnames(datos)=="TMAX"]
  rmax<-data.frame(frec=rle(datos[,colnames(datos)=="TMAX"])$lengths,tmax=rle(datos[,colnames(datos)=="TMAX"])$values)
  for(i in 1:nrow(rmax)){
    if(rmax[i,2]==-99.9){
      ts1<-ts(tmax[1:sum(rmax[1:(i-1),1])], 
             start = c(datos$Año[1], as.numeric(format(inds[1], "%j")),calendar=TRUE),
             frequency = 365)
      mod<-Arima(ts1,order=pmax)
      tmax[(sum(rmax[1:(i-1),1])+1):sum(rmax[1:i,1])]<-as.vector(forecast(mod,h=rmax[i,1])$mean)
    }
    else{}
  }
  datos[,colnames(datos)=="TMAX"]<-round(tmax,1)
  datos[,colnames(datos)=="TMIN"]<-tmin
  return(datos)

}






