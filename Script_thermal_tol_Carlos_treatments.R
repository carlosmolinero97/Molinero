##############################################################################################################
# Script and functions to run:
#' * Thermal tolerance treatments birds/mammals/whatever 
#'
#'  Carlos Molinero Sampedro / Ignacio Morales-Castilla
#'  started may 2019
##############################################################################################################
# Script 2/x

#### 2. Extraccion de datos de parásitos #### 

## to start
rm(list=ls())
options(stringsAsFactors=FALSE)

## load packages
packs.to.extract<-list('raster','ncdf4','maptools','sp','foreach',
                       'doParallel','abind','stringr','rgdal','foreign')
lapply(packs.to.extract,require, character.only=T)
install.packages("SDMTools")
install.packages("letsR")
install.packages("MLmetrics")
library(SDMTools)
library(letsR)
library(MLmetrics)


load("~/MEGA/Work_UAH_postdoc/Teaching2018/TFG CARLOS MOLINERO/R/CMolineroTFG.RData")



#### Tratamientos Tmax, Tmin loop ####

matrizaveseuropeasT=matrizaveseuropeas # definir matriz para extraer datos



matriz.tratamiento15day=array(NA,dim=c(nrow(matrizaveseuropeas.tempmax),nsps))
matriz.tratamiento30day=array(NA,dim=c(nrow(matrizaveseuropeas.tempmax),nsps))
matriz.tratamiento60day=array(NA,dim=c(nrow(matrizaveseuropeas.tempmax),nsps))
matriz.tratamiento90day=array(NA,dim=c(nrow(matrizaveseuropeas.tempmax),nsps))


evaluation.AUC.alltreat = array(NA,dim=c(12,12,4,length(listspeurope)))
length(listspeurope)

for(i in 1:25){#i=4
  
    
  print(listspeurope[i])
  
  specie.i=listspeurope[i]
  
  tol.especie=subset(tolerancias,Binomial2==specie.i)
  
  tol.min=tol.especie$tmin
  tol.max=tol.especie$Tmax
  
  for(numdays in c(15,30,60,90,120,150)){#numdays=15
  
  for(tmaxlim in seq(0.05,0.6,0.05)){ #tmaxlim=0.5
    for(tminlim in seq(0.05,0.6,0.05)){ #tminlim=0.5
      
      print(paste(i,numdays,tmaxlim,tminlim))
      
      tol.maxS=tol.max + tmaxlim*tol.max
      tol.minS=tol.min - tminlim*tol.min
      
      prueba3=apply(matrizaveseuropeas.tempmax[,120:(120+numdays)],1,
                    function(x){ifelse(sum(x>tol.maxS)>30 |
                    sum(x<tol.minS)>30,0,1)})
      prueba3[is.na(prueba3)]=0
      # matriz.tratamientoT3[,i]=prueba3
      #matrizaveseuropeasT3$Presence_and_Absence_Matrix[,specie.i]=prueba3
      
      pos.tmin=which(seq(0.05,0.6,0.05)==tminlim)
      pos.tmax=which(seq(0.05,0.6,0.05)==tmaxlim)
      pos.days=which(c(15,30,60,90,120,150)==numdays)
      
      evaluation.AUC.alltreat[pos.tmin,pos.tmax,pos.days,i]=Accuracy(matrizpresabs[,i], prueba3, 0.5)$AUC
      
      
    }
  }
  }
  
}


#### plotting ####

par(mfrow=c(4,3))

plot(matrizaveseuropeas,specie.i)

for(i in 1:6){
  
  print(max(evaluation.AUC.alltreat[,,i,4],na.rm=T))

  
}

image(apply(t(t(as.matrix(evaluation.AUC.Eu[,,1,4]))),1,rev))

plot()



evaluation.AUC.Eu <- evaluation.AUC.alltreat[,,,]

par(mfrow=c(2,2))
for(i in 1:4){
image(apply(t(t(as.matrix(evaluation.AUC.Eu[,,i,16]))),1,rev))
}
listspeurope[4]

# America
matrizaveamericanasT=matrizavesamericanas # definir matriz para extraer datos

matriz.tratamiento15day2=array(NA,dim=c(nrow(matrizavesamericanas.tempmax),nsps))
matriz.tratamiento30day2=array(NA,dim=c(nrow(matrizavesamericanas.tempmax),nsps))
matriz.tratamiento60day2=array(NA,dim=c(nrow(matrizavesamericanas.tempmax),nsps))
matriz.tratamiento90day2=array(NA,dim=c(nrow(matrizavesamericanas.tempmax),nsps))


evaluation.AUC.alltreat2 = array(NA,dim=c(6,6,4,length(listspamerica)))
length(listspamerica)

for(i in 1:66){#i=1
  
  
  print(listspamerica[i])
  
  specie.i=listspamerica[i]
  
  tol.especie=subset(tolerancias,Binomial2==specie.i)
  
  tol.min=tol.especie$tmin
  tol.max=tol.especie$Tmax
  
  for(numdays in c(15,30,60,90)){#numdays=15
    
    for(tmaxlim in seq(0.05,0.3,0.05)){ #tmaxlim=0.1
      for(tminlim in seq(0.05,0.3,0.05)){ #tminlim=0.1
        
        print(paste(i,numdays,tmaxlim,tminlim))
        
        tol.maxS=tol.max + tmaxlim*tol.max
        tol.minS=tol.min - tminlim*tol.min
        
        prueba4=apply(matrizavesamericanas.tempmax[,120:(120+numdays)],1,
                      function(x){ifelse(sum(x>tol.maxS)>30 |
                                           sum(x<tol.minS)>30,0,1)})
        prueba4[is.na(prueba3)]=0
        # matriz.tratamientoT3[,i]=prueba3
        #matrizaveseuropeasT3$Presence_and_Absence_Matrix[,specie.i]=prueba3
        
        pos.tmin=which(seq(0.05,0.3,0.05)==tminlim)
        pos.tmax=which(seq(0.05,0.3,0.05)==tmaxlim)
        pos.days=which(c(15,30,60,90)==numdays)
        
        evaluation.AUC.alltreat2[pos.tmin,pos.tmax,pos.days,i]=accuracy(matrizpresabs2[,i], prueba4,0.5)$AUC
        
        
      }
    }
  }
  
}



evaluation.AUC.Am <- evaluation.AUC.alltreat2[,,,]





#accuracy(observados, matriztramiento, 0,5 es un umbral que marcamos) AUC y Prop.correct
#librarySDMTools 
?accuracy

#preparando datos
matrizpresabs=matrizaveseuropeas$Presence_and_Absence_Matrix[,3:27]
matrizpresabsT1=matrizaveseuropeasT1$Presence_and_Absence_Matrix[,3:27]

#calculando AUC Eur
accuracy(matrizpresabs[,1], matrizpresabsT1[,1],0.5)
sum(matrizpresabsT1[,1],na.rm=T)

tabla.evaluacion.EUR=array(NA,dim=c(25,6))
row.names(tabla.evaluacion.EUR)=matrizaveseuropeas$Species_name
colnames(tabla.evaluacion.EUR)=c("T1.AUC","T2.AUC",
                                 "T3.AUC","T4.AUC",
                                 "T5.AUC","T6.AUC")

#### 4.8 Matrices Presencia-Tratamientos
matricestratslist=list()
matricestratslist[[1]]=matrizaveseuropeasT1$Presence_and_Absence_Matrix[,3:27]
matricestratslist[[2]]=matrizaveseuropeasT2$Presence_and_Absence_Matrix[,3:27]
matricestratslist[[3]]=matrizaveseuropeasT3$Presence_and_Absence_Matrix[,3:27]
matricestratslist[[4]]=matrizaveseuropeasT4$Presence_and_Absence_Matrix[,3:27]
matricestratslist[[5]]=matrizaveseuropeasT5$Presence_and_Absence_Matrix[,3:27]
matricestratslist[[6]]=matrizaveseuropeasT6$Presence_and_Absence_Matrix[,3:27]

...

for (j in 1:6){
  
  matrizTratam.j=matricestratslist[[j]]
  
  for(i in 1:25){
    
    
    evalu.result=accuracy(matrizpresabs[,i], matrizTratam.j[,i],0.5)
    
    
    tabla.evaluacion.EUR[i,(j)]=evalu.result$AUC
    
  }
  
}



















###Europa
#### 4. Tratamientos ####
#### 4.1 Tratamiento 1: Tmax-Tmin y 25% días (30 días) ####
matriz.tratamientoT1=array(NA,dim=c(nrow(matrizaveseuropeas.tempmax),nsps))
matrizaveseuropeasT1=matrizaveseuropeas

for(i in 1:nsps){#i=1
  print(listspeurope[i])
  specie.i=listspeurope[i]
  
  tol.especie=subset(tolerancias,Binomial2==specie.i)
  
  tol.min=tol.especie$tmin
  tol.max=tol.especie$Tmax
  
  prueba=apply(matrizaveseuropeas.tempmax[,120:241],1,function(x){ifelse(sum(x>tol.max)>30 |
                                                                           sum(x<tol.min)>30,0,1)})
  matriz.tratamientoT1[,i]=prueba
  matrizaveseuropeasT1$Presence_and_Absence_Matrix[,specie.i]=prueba
  plot(matrizaveseuropeas.tempmin[,"Longitude(x)"],
       matrizaveseuropeas.tempmin[,"Latitude(y)"],
       col=matriz.tratamientoT1[,i]+1, main=i,
       cex.main=2, sub="T1", ylab="Latitud",xlab="Longitud",cex.lab=1.4
  )
}

sum(is.na(matriz.tratamientoT1))

#Comparamos las dimensiones
matrizaveseuropeasT1
matrizaveseuropeas.tempmin
length(matrizaveseuropeas.tempmin)
## Representamos:
##plot(matrizaveseuropeas.tempmin[,"Longitude(x)"],
matrizaveseuropeas.tempmin[,"Latitude(y)"],
col=matriz.tratamientoT1[,specie.i]+1
)

#write.csv(matriz.tratamientoT1, "matriz.tratamiento1.csv")

#### 4.2 Tratamiento 2: Tmax-Tmin y 75% días (90 días) #####
matriz.tratamientoT2=array(NA,dim=c(nrow(matrizaveseuropeas.tempmax),nsps))
matrizaveseuropeasT2=matrizaveseuropeas

for(i in 1:nsps){#i=1
  print(listspeurope[i])
  
  specie.i=listspeurope[i]
  
  tol.especie=subset(tolerancias,Binomial2==specie.i)
  
  tol.min=tol.especie$tmin
  tol.max=tol.especie$Tmax
  
  prueba2=apply(matrizaveseuropeas.tempmax[,120:241],1,function(x){ifelse(sum(x>tol.max)>90 |
                                                                            sum(x<tol.min)>90,0,1)})
  matriz.tratamientoT2[,i]=prueba2
  matrizaveseuropeasT2$Presence_and_Absence_Matrix[,specie.i]=prueba2
  plot(matrizaveseuropeas.tempmin[,"Longitude(x)"],
       matrizaveseuropeas.tempmin[,"Latitude(y)"],
       col=matriz.tratamientoT2[,i]+1, main=i,
       cex.main=2, sub="T2", ylab="Latitud",xlab="Longitud",cex.lab=1.4
  )
}

sum(is.na(matriz.tratamientoT2))

#### 4.3 Tratamiento 3: +10% de Tmax- -20% de Tmin y 25% días (30 días) ####
matriz.tratamientoT3=array(NA,dim=c(nrow(matrizaveseuropeas.tempmax),nsps))
matrizaveseuropeasT3=matrizaveseuropeas

for(i in 1:nsps){#i=1
  print(listspeurope[i])
  
  specie.i=listspeurope[i]
  
  tol.especie=subset(tolerancias,Binomial2==specie.i)
  
  tol.min=tol.especie$tmin
  tol.max=tol.especie$Tmax
  
  tol.maxS=tol.max + 0.1*tol.max
  tol.minS=tol.min - 0.2*tol.min
  
  prueba3=apply(matrizaveseuropeas.tempmax[,120:241],1,function(x){ifelse(sum(x>tol.maxS)>30 |
                                                                            sum(x<tol.minS)>30,0,1)})
  matriz.tratamientoT3[,i]=prueba3
  matrizaveseuropeasT3$Presence_and_Absence_Matrix[,specie.i]=prueba3
  plot(matrizaveseuropeas.tempmin[,"Longitude(x)"],
       matrizaveseuropeas.tempmin[,"Latitude(y)"],
       col=matriz.tratamientoT3[,i]+1, main=i,
       cex.main=2, sub="T3", ylab="Latitud",xlab="Longitud",cex.lab=1.4
  )
}

sum(is.na(matriz.tratamientoT3))

)

#### 4.4 Tratamiento 4: +10% de Tmax- -20% de Tmin y 75% días (90 días) #####
matriz.tratamientoT4A=array(NA,dim=c(nrow(matrizavesamericanas.tempmax),nsps))
matrizavesamericanasT4=matrizavesamericanas

for(i in 1:nsps){#i=1
  print(listspamerica[i])
  
  specie.i=listspamerica[i]
  
  
  tol.especie=subset(tolerancias,Binomial2==specie.i)
  
  
  tol.min=tol.especie$tmin
  tol.max=tol.especie$Tmax
  
  tol.maxS=tol.max + 0.1*tol.max
  tol.minS=tol.min - 0.2*tol.min
  
  prueba4=apply(matrizavesamericanas.tempmax[,120:241],1,function(x){ifelse(sum(x>tol.maxS)>90 |
                                                                              sum(x<tol.minS)>90,0,1)})
  
  matriz.tratamientoT4A[,i]=prueba4
  matrizavesamericanasT4$Presence_and_Absence_Matrix[,specie.i]=prueba4
  plot(matrizavesamericanas.tempmin[,"Longitude(x)"],
       matrizavesamericanas.tempmin[,"Latitude(y)"],
       col=matriz.tratamientoT4A[,i]+1, main=i,
       cex.main=2, sub="T4", ylab="Latitud",xlab="Longitud",cex.lab=1.4
  )
}

sum(is.na(matriz.tratamientoT4))

#### 4.5 Tratamiento 5: +25% de Tmax- 50% de Tmin y 25% días (30 días) #####
matriz.tratamientoT5A=array(NA,dim=c(nrow(matrizavesamericanas.tempmax),nsps))
matrizavesamericanasT5=matrizavesamericanas

for(i in 1:nsps){#i=1
  print(listspamerica[i])
  
  specie.i=listspamerica[i]
  
  
  tol.especie=subset(tolerancias,Binomial2==specie.i)
  
  
  tol.min=tol.especie$tmin
  tol.max=tol.especie$Tmax
  
  tol.maxSS=tol.max + 0.25*tol.max
  tol.minSS=tol.min - 0.5*tol.min
  
  prueba5=apply(matrizavesamericanas.tempmax[,120:241],1,function(x){ifelse(sum(x>tol.maxSS)>30 |
                                                                              sum(x<tol.minSS)>30,0,1)})
  
  matriz.tratamientoT5A[,i]=prueba5
  matrizavesamericanasT5$Presence_and_Absence_Matrix[,specie.i]=prueba5
  plot(matrizavesamericanas.tempmin[,"Longitude(x)"],
       matrizavesamericanas.tempmin[,"Latitude(y)"],
       col=matriz.tratamientoT5A[,i]+1, main=i,
       cex.main=2, sub="T5", ylab="Latitud",xlab="Longitud",cex.lab=1.4
  )
}

sum(is.na(matriz.tratamientoT5))

#### 4.6 Tratamiento 6: +25% de Tmax- 50% de Tmin y 75% días (90 días) #####
matriz.tratamientoT6A=array(NA,dim=c(nrow(matrizavesamericanas.tempmax),nsps))
matrizavesamericanasT6=matrizavesamericanas


for(i in 1:nsps){#i=1
  print(listspamerica[i])
  
  specie.i=listspamerica[i]
  
  
  tol.especie=subset(tolerancias,Binomial2==specie.i)
  
  
  tol.min=tol.especie$tmin
  tol.max=tol.especie$Tmax
  
  tol.maxSS=tol.max + 0.25*tol.max
  tol.minSS=tol.min - 0.5*tol.min
  
  prueba6=apply(matrizavesamericanas.tempmax[,120:241],1,function(x){ifelse(sum(x>tol.maxSS)>90 |
                                                                              sum(x<tol.minSS)>90,0,1)})
  
  matriz.tratamientoT6A[,i]=prueba6
  matrizavesamericanasT6$Presence_and_Absence_Matrix[,specie.i]=prueba6
  plot(matrizavesamericanas.tempmin[,"Longitude(x)"],
       matrizavesamericanas.tempmin[,"Latitude(y)"],
       col=matriz.tratamientoT6A[,i]+1, main=i,
       cex.main=2, sub="T6", ylab="Latitud",xlab="Longitud",cex.lab=1.4
  )
}

sum(is.na(matriz.tratamientoT6))

###N.América

library(letsR)

listspamerica=colnames(matrizavesamericanas.tempmin)[3:68]
nsps=length(listspamerica)
## Tratamiento 1: Tmax-Tmin y 25% días (30 días)
matriz.tratamientoT1A=array(NA,dim=c(nrow(matrizavesamericanas.tempmax),nsps))
matrizavesamericanasT1=matrizavesamericanas


for(i in 1:nsps){#i=1
  print(listspamerica[i])
  
  specie.i=listspamerica[i]
  
  
  tol.especie=subset(tolerancias,Binomial2==specie.i)
  
  tol.min=tol.especie$tmin
  tol.max=tol.especie$Tmax
  
  
  pruebaA=apply(matrizavesamericanas.tempmax[,120:241],1,function(x){ifelse(sum(x>tol.max)>30 |
                                                                              sum(x<tol.min)>30,0,1)})
  
  matriz.tratamientoT1A[,i]=pruebaA
  matrizavesamericanasT1$Presence_and_Absence_Matrix[,specie.i]=pruebaA
  plot(matrizavesamericanas.tempmin[,"Longitude(x)"],
       matrizavesamericanas.tempmin[,"Latitude(y)"],
       col=matriz.tratamientoT1A[,i]+1, main=i,
       cex.main=2, sub="T1", ylab="Latitud",xlab="Longitud",cex.lab=1.4
  )
}

sum(is.na(matriz.tratamientoT1))

#Comparamos las dimensiones
matrizaveseuropeasT1
matrizaveseuropeas.tempmin
length(matrizaveseuropeas.tempmin)
## Representamos:
##plot(matrizaveseuropeas.tempmin[,"Longitude(x)"],
matrizaveseuropeas.tempmin[,"Latitude(y)"],
col=matriz.tratamientoT1[,specie.i]+1
)

#write.csv(matriz.tratamientoT1, "matriz.tratamiento1.csv")


# Tratamiento 2: Tmax-Tmin y 75% días (90 días) #
matriz.tratamientoT2A=array(NA,dim=c(nrow(matrizavesamericanas.tempmax),nsps))
matrizavesamericanasT2=matrizavesamericanas

for(i in 1:nsps){#i=1
  print(listspamerica[i])
  
  specie.i=listspamerica[i]
  
  
  tol.especie=subset(tolerancias,Binomial2==specie.i)
  
  tol.min=tol.especie$tmin
  tol.max=tol.especie$Tmax
  
  
  prueba2=apply(matrizavesamericanas.tempmax[,120:241],1,function(x){ifelse(sum(x>tol.max)>90 |
                                                                              sum(x<tol.min)>90,0,1)})
  
  matriz.tratamientoT2A[,i]=prueba2
  matrizavesamericanasT2$Presence_and_Absence_Matrix[,specie.i]=prueba2
  plot(matrizavesamericanas.tempmin[,"Longitude(x)"],
       matrizavesamericanas.tempmin[,"Latitude(y)"],
       col=matriz.tratamientoT2A[,i]+1, main=i,
       cex.main=2, sub="T2", ylab="Latitud",xlab="Longitud",cex.lab=1.4
  )
}

sum(is.na(matriz.tratamientoT2))

# Tratamiento 3: +10% de Tmax- -20% de Tmin y 25% días (30 días) #
matriz.tratamientoT3A=array(NA,dim=c(nrow(matrizavesamericanas.tempmax),nsps))
matrizavesamericanasT3=matrizavesamericanas

for(i in 1:nsps){#i=1
  print(listspamerica[i])
  
  specie.i=listspamerica[i]
  
  
  tol.especie=subset(tolerancias,Binomial2==specie.i)
  
  
  tol.min=tol.especie$tmin
  tol.max=tol.especie$Tmax
  
  tol.maxS=tol.max + 0.1*tol.max
  tol.minS=tol.min - 0.2*tol.min
  
  prueba3=apply(matrizavesamericanas.tempmax[,120:241],1,function(x){ifelse(sum(x>tol.maxS)>30 |
                                                                              sum(x<tol.minS)>30,0,1)})
  
  matriz.tratamientoT3A[,i]=prueba3
  matrizavesamericanasT3$Presence_and_Absence_Matrix[,specie.i]=prueba3
  plot(matrizavesamericanas.tempmin[,"Longitude(x)"],
       matrizavesamericanas.tempmin[,"Latitude(y)"],
       col=matriz.tratamientoT3A[,i]+1, main=i,
       cex.main=2, sub="T3", ylab="Latitud",xlab="Longitud",cex.lab=1.4
  )
}

sum(is.na(matriz.tratamientoT3))


#### 4.7 Calculo AUC ####

 




