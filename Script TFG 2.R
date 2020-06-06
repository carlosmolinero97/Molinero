##############################################################################################################
# Script and functions to run:
#' * Parasite diversity maps for birds 
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

# set wd
setwd()

##### 2.1 Extraccion de datos ####
# cargar datos de distribución aves
load("matrizaveseuropeas.RData")
load("matrizavesamericanas.RData")

#cargar datos de parasitismo
datbirdparasite=read.csv("Aveparasitedata.csv", sep = ";")
str(datbirdparasite)

par.eur.names=unique(datbirdparasite$ParasiteSp[which(datbirdparasite$Continent=="Europe")])
num.parasitos.eur=length(par.eur.names)

par.nam.names=unique(datbirdparasite$ParasiteSp[which(datbirdparasite$Continent=="North America")])
num.parasitos.nam=length(par.nam.names)

par.names=unique(datbirdparasite$ParasiteSp)
num.parasitos=length(par.names)

#### 2.2 Conocer qué ave tiene más parasitos ####
#Sacar qué ave tiene más parásitos:
num.par.sturnusv=unique(datbirdparasite$ParasiteSp[which(datbirdparasite$HostSp=="Sturnus vulgaris")])
num.par.sturnusvulgaris=length(num.par.sturnusv)

num.par.passerd=unique(datbirdparasite$ParasiteSp[which(datbirdparasite$HostSp=="Passer domesticus")])
num.par.passerdomesticus=length(num.par.passerd)

num.par.zenaidam=unique(datbirdparasite$ParasiteSp[which(datbirdparasite$HostSp=="Zenaida macroura")])
num.par.Zenaidamacroura=length(num.par.zenaidam)

# crear una matriz vacía para almacenar distribución de parásitos
parasitepresabs.eur=array(NA,dim=c(nrow(matrizaveseuropeas$Presence_and_Absence_Matrix),
                                   num.parasitos.eur+2))
parasitepresabs.nam=array(NA,dim=c(nrow(matrizavesamericanas$Presence_and_Absence_Matrix),
                                   num.parasitos.nam+2))

parasitepresabs.eur[,1:2]=matrizaveseuropeas$Presence_and_Absence_Matrix[,1:2]
parasitepresabs.nam[,1:2]=matrizavesamericanas$Presence_and_Absence_Matrix[,1:2]

colnames(parasitepresabs.eur[,3:(num.parasitos.eur+2)])=par.eur.names
colnames(parasitepresabs.nam[,3:(num.parasitos.nam+2)])=par.nam.names

#### 2.4 Creacion de matrices para P/A parásitos ####
# bucle para asignar a cada columna de las matrices anteriores las presencias y ausencias de cada parásito

#datbirdparasite=subset(datbirdparasite,HostSpecies!="Branta bernicla")
datbirdparasite.eur=subset(datbirdparasite,Continent=="Europe")
datbirdparasite.nam=subset(datbirdparasite,Continent=="North America")
datbirdparasite.nam[which(datbirdparasite.nam$HostSp=="Antrostomus vociferous"),"HostSp"]="Antrostomus vociferus"
datbirdparasite.nam[which(datbirdparasite.nam$HostSp=="Pica nutallii"),"HostSp"]="Pica nutalli"

# chequeando correspondencia entre nombres de distintas bases de datos
datbirdparasite.nam$HostSp[which(!datbirdparasite.nam$HostSp%in%colnames(matrizavesamericanas$Presence_and_Absence_Matrix))]
colnames(matrizavesamericanas$Presence_and_Absence_Matrix)[which(!colnames(matrizavesamericanas$Presence_and_Absence_Matrix)%in%datbirdparasite.nam$HostSp)]

## bucleando
for(i in 1:num.parasitos.eur){#i=1
  print(i)
  
  parasite.i=par.eur.names[i]
  
  sub.data.i=subset(datbirdparasite.eur,ParasiteSp==parasite.i)
  
  hosts=unique(sub.data.i$HostSp)
  num.hosts=length(hosts)
  
  if(num.hosts==1){
    sumhosts=matrizaveseuropeas$Presence_and_Absence_Matrix[,hosts]
  } else {
    sumhosts=rowSums(matrizaveseuropeas$Presence_and_Absence_Matrix[,hosts])
    sumhosts=ifelse(sumhosts>0,1,0)
  }
  
  parasitepresabs.eur[,i+2]=sumhosts
  
}

#parasitepresabs.eur[1:5,1:10]
#hist(colSums(parasitepresabs.eur[,3:420]))

for(i in 1:num.parasitos.nam){#i=1
  print(i)
  
  parasite.i=par.nam.names[i]
  
  sub.data.i=subset(datbirdparasite.nam,ParasiteSp==parasite.i)
  
  hosts=unique(sub.data.i$HostSp)
  num.hosts=length(hosts)
  
  if(num.hosts==1){
    sumhosts=matrizavesamericanas$Presence_and_Absence_Matrix[,hosts]
  } else {
    sumhosts=rowSums(matrizavesamericanas$Presence_and_Absence_Matrix[,hosts])
    sumhosts=ifelse(sumhosts>0,1,0)
  }
  
  parasitepresabs.nam[,i+2]=sumhosts
  
}

#hist(parasitepresabs.eur)
#View(parasitepresabs.eur)
#### 2.5 Riqueza de parasitos ####
#Riqueza:
RiqPotParEu= rowSums(parasitepresabs.eur[,3:420], na.rm = T)
RiqPotParAme= rowSums(parasitepresabs.nam[,3:603], na.rm = T)

richparEu=matrizaveseuropeas$Richness_Raster
richparEu[values(richparEu)!=0]=RiqPotParEu
plot(richparEu)
writeRaster(richparEu,"RichnessParasiteEur.tif",format="GTiff")

richparAm=matrizavesamericanas$Richness_Raster
richparAm[values(richparAm)!=0]=RiqPotParAme
plot(richparAm)
writeRaster(richparAm,"RichnessParasiteAme.tif",format="GTiff")

#### 2.6 Busqueda de diferencias entre las aves migradoras y no-migradoras ####
datbirdparasite2=read.csv("Aveparasitedata2.csv", sep = ";")
str(datbirdparasite2)

par.migrant.names=unique(datbirdparasite2$ParasiteSp[which(datbirdparasite2$HostSp=="Migratoria")])
num.parasitos.migrant=length(par.migrant.names)
num.parasitos.migrant/26 #(Numero de especies migrantes)

par.nomigrant.names=unique(datbirdparasite2$ParasiteSp[which(datbirdparasite2$HostSp=="NoMigratoria")])
num.parasitos.nomigrant=length(par.nomigrant.names)
num.parasitos.nomigrant/34 #(Numero de especies no-migrantes)

# carga el archivo AA con todos los rangos de todas las aves
load("birds.allsps.shape.RData")

#tolerancias=read.csv("GlobalTherm Actualizado 2.csv")
tolerancias=read.table("GlobalTherm 4.csv",header = T,sep=";")
head(tolerancias)

# explorar datos (cuantas especies hay, lista de nombres, etc.)
head(AA)
length(unique(AA@data$SCINAME))

AA@data

# comprobar qué especies están en ambas bases de datos

listaavesmapas=AA@data$SCINAME
avessintolerancia=which(listaavesmapas %in% tolerancias$Binomial2)
especiescontol=unique(listaavesmapas[avessintolerancia])
especiessinonimos=tolerancias$Binomial2[which(! tolerancias$Binomial2%in% especiescontol)]
#as.character()
## salvar listas de especies a csv
write.csv(especiessinonimos, "lista.sps.not.in.maps.csv")
write.csv(unique(listaavesmapas), "lista.all.sps.in.maps.csv")

write.table(especiessinonimos, "lista.sps.not.in.maps.txt",col.names = T,sep="\t")
write.table(unique(listaavesmapas), "lista.all.sps.in.maps.txt",col.names = T,sep="\t")

# hacer un subset de datos, para quedarnos solo con aquellas especies que nos interesan 
# (para las cuales tenemos datos de tolerancia termica)

avestoleranciamaps = AA[avessintolerancia,]
AA = AA[avessintolerancia,]
#plot(avestoleranciamaps)

AA = AA[which(AA@data$PRESENCE<4),]
AA = AA[which(AA@data$ORIGIN<4),]
avestoleranciamap = AA[which(AA@data$SEASONAL<3),]
avestoleranciamap.migrant = AA[which(AA@data$SEASONAL==3),]
# extraer matriz presencias y ausencias
library(letsR)

#matrizaves= lets.presab(avestoleranciamaps,resol=20)
#show(matrizaves)
matrizaveseuropeas= lets.presab(avestoleranciamap, xmn = -28, xmx = 38, ymn = 32, ymx = 80, resol = 0.5)
matrizaveseuropeasmigrant= lets.presab(avestoleranciamap.migrant, xmn = -28, xmx = 50, ymn = 32, ymx = 80, resol = 0.5)

matrizavesamericanas= lets.presab(avestoleranciamap, xmn = -180, xmx = -48, ymn = 5, ymx = 80, resol = 0.5)
matrizavesamericanasmigrant= lets.presab(avestoleranciamap.migrant, xmn = -180, xmx = -48, ymn = 5, ymx = 80, resol = 0.5)

## guardar matrices
save(matrizaveseuropeas,file = "matrizaveseuropeas.RData")
save(matrizavesamericanas,file = "matrizavesamericanas.RData")

#### 2.7 Representacion de mapas de tolerancias ####
install.packages("viridis")
library(viridis)
plot(matrizavesamericanasmigrant)
plot(matrizavesamericanas, col= (viridis))
plot(matrizaveseuropeas, col= (viridis))
plot(matrizaveseuropeasmigrant, col= (viridis))
# Observar qué especies hay en cada matriz:
show(matrizaveseuropeas$Species_name)
show(matrizaveseuropeasmigrant$Species_name)
show(matrizavesamericanas$Species_name)
show(matrizavesamericanasmigrant$Species_name)


## comprobar qué especies están en ambas bases de datos (Base de datos de T, distribución y Parásitos)
parasites=read.table("ParasiteGlobal.csv",header = T,sep=";")

avesstoleranciainparasitos=especiescontol[which(especiescontol %in% as.character(unique(parasites$HostSpecies)))]

#Salvar un csv e las especies en ambas bases de T, distribución y parásitos. 
#write.csv(avesstoleranciainparasitos, "lista.sps.parasites.temperature.csv")
#### 3. Extraccion de los datos climáticos ####
# Extraer los datos de BEST
require('maptools')
require('ncdf4')
require('abind')
require('raster')
require('maptools')
require('sp')
require('foreach')
require('data.table')

####### Getting data to be matched from stations (observed data)
#setwd("C:/.../GitHub/vin/climatefuture/models/analyses/inputs")

# open BEST climatology as brick 
daily.temp.BEST.min <- brick("BEST.mean.clim_min.nc")
daily.temp.BEST.max <- brick("BEST.mean.clim_max.nc")

plot(daily.temp.BEST.min[[160]])
writeRaster(daily.temp.BEST.min[[160]],"XXX.tif",format="GTiff")
writeRaster(daily.temp.BEST.max[[160]],"YYY.tif",format="GTiff")
library(letsR)

matrizavesamericanas.tempmin=lets.addvar(matrizavesamericanas,daily.temp.BEST.min)
matrizavesamericanas.tempmax=lets.addvar(matrizavesamericanas,daily.temp.BEST.max)

hist(matrizavesamericanas.tempmin)
hist(matrizavesamericanas.tempmax)

matrizaveseuropeas.tempmin=lets.addvar(matrizaveseuropeas,daily.temp.BEST.min)
matrizaveseuropeas.tempmax=lets.addvar(matrizaveseuropeas,daily.temp.BEST.max)

hist(matrizaveseuropeas.tempmin)
hist(matrizaveseuropeas.tempmax)

media.mins.eur=colMeans(matrizaveseuropeas.tempmin[,28:392],na.rm=T)
media.maxs.eur=colMeans(matrizaveseuropeas.tempmax[,28:392],na.rm=T)
media.sdsmins.eur=apply(matrizaveseuropeas.tempmin[,28:392],2,sd,na.rm=T)
media.sdsmaxs.eur=apply(matrizaveseuropeas.tempmax[,28:392],2,sd,na.rm=T)


#Aves Europeas:
min.mins.eur=apply(matrizaveseuropeas.tempmin[,28:392],2,min,na.rm=T)
max.maxs.eur=apply(matrizaveseuropeas.tempmax[,28:392],2,max,na.rm=T)
#### 3.1 Representación de las tolerancias y los datos climaticos ####
# Dibujando especies
listspeurope=colnames(matrizaveseuropeas.tempmin)[3:27]
nsps=length(listspeurope)
nums=seq(100,200,3.6)
plot(1:365,max.maxs.eur,ylim=c(-30,50),col="red",type="l", lwd=3, main="Aves Europeas",
     cex.main=2, ylab="Temperaturas Medias Diarias (ºC)",xlab="Días del Año",cex.lab=1.4)
lines(1:365,min.mins.eur,col="blue", lwd=3)
legend("topleft", col=c("red","blue"),legend =c("Tmax","Tmin"), lwd = 3, bty="n")
text(98,25,"1") 
text(101.4,10,"2") 
text(105,32.7,"3")
text(108.8,22.2,"4")
text(117.2,28,"5") 
text(116,5,"6")
text(119.6,30,"7")
text(123,6,"8") 
text(131.9,7,"9")
text(128.4,29,"10")
text(132,20,"11") 
text(135.6,9,"12")
text(139.2,-3.25,"13")
text(142.8,-7,"14")
text(146.4,27.6,"15")
text(150,21.5,"16")
text(157.6,13,"17")
text(164.7,21,"18")
text(160.8,9,"19")
text(164.4,14,"20")
text(168,4.5,"21")
text(171.6,16,"22")
text(175.2,29.5,"23")
text(178.8,22.5,"24")
text(182.4,15,"25")
text(186,2,"26")


for(i in 1:nsps){#i=1
  print(listspeurope[i])
  sps=listspeurope[i]
  tols.sps=subset(tolerancias,Binomial2==sps)
  points(nums[i],tols.sps$Tmax,col="darkred",cex=1.5,pch=20)
  points(nums[i],tols.sps$tmin,col="darkblue",cex=1.5,pch=20)
  lines(c(nums[i],nums[i]),c(tols.sps$tmin-5,tols.sps$Tmax),col="blue", lwd=1)
  lines(c(nums[i],nums[i]),c(tols.sps$tmin,tols.sps$Tmax+5), col="red", lwd=1)
  lines(c(nums[i],nums[i]),c(tols.sps$tmin,tols.sps$Tmax),lwd=3)
}

#Aves Americanas:
min.mins.ame=apply(matrizavesamericanas.tempmin[,71:435],2,min,na.rm=T)
max.maxs.ame=apply(matrizavesamericanas.tempmax[,71:435],2,max,na.rm=T)

listspamerica=colnames(matrizavesamericanas.tempmin)[3:70]
nsps1=length(listspamerica)
nums1=seq(65,300,3.5)
plot(1:365,max.maxs.ame,ylim=c(-30,50),col="red",type="l", lwd=3, main="Aves Norteamericanas",
     cex.main=2, ylab="Temperaturas Medias Diarias (ºC)",xlab="Días del Año",cex.lab=1.4)
lines(1:365,min.mins.ame,col="blue", lwd=3)
legend("topleft", col=c("red","blue"),legend =c("Tmax","Tmin"), lwd = 3, bty="n")

text(98,25,"1")
text(102.94,18,"27") 
text(103.8,10,"2")
text(105.2,40,"28")
text(109.76,22.2,"4") 
text(114.7,44.5,"5")
text(115.4,5,"6")
text(120.58,38.5,"29") 
text(120.5,25,"30")
text(123.7,12.4,"31")
text(129.3,44.3,"32") 
text(130,28,"33")
text(132.9,25,"34")
text(134.2,23,"35")
text(139,23,"36")
text(142,20.5,"37")
text(145,6,"8")
text(147,20.3,"38")
text(150,27.5,"39")
text(153,27,"40")
text(156,29,"41")
text(159.6,30,"42")
text(162.2,7,"9")
text(164.8,27,"43")
text(167.2,30,"44")
text(170,11.5,"45")

for(i in 1:nsps1){#i=1
  print(listspamerica[i])
  spsA=listspamerica[i]
  tols.spsA=subset(tolerancias,Binomial2==spsA)
  points(nums1[i],tols.spsA$Tmax,col="darkred",cex=1.5,pch=20)
  points(nums1[i],tols.spsA$tmin,col="darkblue",cex=1.5,pch=20)
  lines(c(nums1[i],nums1[i]),c(tols.spsA$tmin-5,tols.spsA$Tmax),col="blue", lwd=1)
  lines(c(nums1[i],nums1[i]),c(tols.spsA$tmin,tols.spsA$Tmax+5), col="red", lwd=1)
  lines(c(nums1[i],nums1[i]),c(tols.spsA$tmin,tols.spsA$Tmax),lwd=3)
}



#if / else: permite decidir si ejecutar o no un fragmento de código en función de una condición.

#### 3.2 Preparacion de mapas térmicos #### 
## Ejemplo mapa rango térmico de una especie de ave
especie8=colnames(matrizaveseuropeas.tempmin)[10]

## Observamos el área de distribución de la especie. 
plot(matrizaveseuropeas.tempmin[which(matrizaveseuropeas.tempmin[,especie18]==1),1],
     matrizaveseuropeas.tempmin[which(matrizaveseuropeas.tempmin[,especie18]==1),2])

plot(matrizaveseuropeas,especie18)

## Observamos la tolerancia de la especie.
tol.especie18=subset(tolerancias,Binomial2==especie18)

## Lo mecanizamos:
tol.min8=tol.especie8$tmin
tol.max8=tol.especie8$Tmax

tol.max8s=tol.max8 + 0.1*tol.max8
tol.min8s=tol.min8 - 0.1*tol.min8

## Buscamos celdas con Temperaturas entre la Tmax y Tmin de la especie. 
## El 1 significa la fila, y [29 a 393 son los dias del año]

prueba=apply(matrizaveseuropeas.tempmax[,28:392],1,function(x){ifelse(sum(x>tol.max)>90 &
                                                                        sum(x<tol.min)>90,0,1)})

#loop para sacar mapas todas las especies
listspeurope=colnames(matrizaveseuropeas.tempmin)[3:27]
nsps=length(listspeurope)

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
#instalar el paquete: SDMTools.
library(SDMTools)
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



boxplot(tabla.evaluacion.EUR, col ="darkslateblue", main = "Aves Europeas", ylim=c(0.5,1)) 
#write.csv(tabla.evaluacion.EUR, "tabla.evaluacion.EUR.csv")

##AUC America
matrizpresabs2=matrizavesamericanas$Presence_and_Absence_Matrix[,3:68]
matrizpresabsT1A=matrizaveseuropeasT1$Presence_and_Absence_Matrix[,3:68]

tabla.evaluacion.AME=array(NA,dim=c(66,6))
row.names(tabla.evaluacion.AME)=matrizavesamericanas$Species_name
colnames(tabla.evaluacion.AME)=c("T1.AUC","T2.AUC",
                                 "T3.AUC","T4.AUC",
                                 "T5.AUC","T6.AUC")


matricestratslist2=list()
matricestratslist2[[1]]=matrizavesamericanasT1$Presence_and_Absence_Matrix[,3:68]
matricestratslist2[[2]]=matrizavesamericanasT2$Presence_and_Absence_Matrix[,3:68]
matricestratslist2[[3]]=matrizavesamericanasT3$Presence_and_Absence_Matrix[,3:68]
matricestratslist2[[4]]=matrizavesamericanasT4$Presence_and_Absence_Matrix[,3:68]
matricestratslist2[[5]]=matrizavesamericanasT5$Presence_and_Absence_Matrix[,3:68]
matricestratslist2[[6]]=matrizavesamericanasT6$Presence_and_Absence_Matrix[,3:68]


...

for (j in 1:6){
  
  matrizTratam.j=matricestratslist2[[j]]
  
  for(i in 1:66){
    
    
    evalu.result=accuracy(matrizpresabs2[,i], matrizTratam.j[,i],0.5)
    
    
    tabla.evaluacion.AME[i,(j)]=evalu.result$AUC
    
    }
  
}

boxplot(tabla.evaluacion.AME, main = "Aves Americanas", col= "darkslateblue", ylim=c(0.5,1)) 
#write.csv(tabla.evaluacion.AME, "tabla.evaluacion.Ame.csv")

dim(tabla.evaluacion.AME)
apply(tabla.evaluacion.AME[,c(1,3,5,7,9,11)],1,function(x){
  which.max(x)
})

#### 5. Riqueza potencial ####
#RiquezaPotencial
RiqPotT4EU= rowSums(matrizaveseuropeasT4$Presence_and_Absence_Matrix[,3:27], na.rm = T)
RiqPotT6EU= rowSums(matrizaveseuropeasT6$Presence_and_Absence_Matrix[,3:27], na.rm = T)
plot(matrizaveseuropeasT4$Presence_and_Absence_Matrix[,1],
     matrizaveseuropeasT4$Presence_and_Absence_Matrix[,2],
     col=RiqPotT4EU)

RiqPotT4AM= rowSums(matrizavesamericanasT4$Presence_and_Absence_Matrix[,3:68], na.rm = T)
RiqPotT6AM= rowSums(matrizavesamericanasT6$Presence_and_Absence_Matrix[,3:68], na.rm = T)

DiffT4EU= matrizaveseuropeas$Presence_and_Absence_Matrix - RiqPotT4EU
DiffT6EU= matrizaveseuropeas$Presence_and_Absence_Matrix - RiqPotT6EU

DiffT4AM= matrizavesamericanas$Presence_and_Absence_Matrix - RiqPotT4AM
DiffT6AM= matrizavesamericanas$Presence_and_Absence_Matrix - RiqPotT6AM
plot(DiffT6AM)

#### 5.1 Representacion de riquezas ####
##Europa
richi=matrizaveseuropeasT4$Richness_Raster
richi4=matrizaveseuropeasT4$Richness_Raster
richi4[values(richi4)!=0]=RiqPotT4EU
richiEu4=(richi-richi4)
plot(richiEu4)
writeRaster(richiEu4,"PotentialRichnessEu4.tif",format="GTiff")

richis=matrizaveseuropeasT6$Richness_Raster
richi6=matrizaveseuropeasT6$Richness_Raster
richi6[values(richi6)!=0]=RiqPotT6EU
plot(richis-richi6)
#America
richiA=matrizavesamericanasT4$Richness_Raster
richiA4=matrizavesamericanasT4$Richness_Raster
richiA4[values(richiA4)!=0]=RiqPotT4AM
plot(richiA-richiA4)
richiAm4=(richiA-richiA4)
plot(richiAm4)
writeRaster(richiAm4,"PotentialRichnessAm4.tif",format="GTiff")

richisA=matrizavesamericanasT6$Richness_Raster
richiA6=matrizavesamericanasT6$Richness_Raster
richiA6[values(richiA6)!=0]=RiqPotT6AM
plot(richisA-richiA6)

#### Otras cosas por revisar ####
##Para la base de datos necesitamos sacar los centroides de algunos estados, por eso
#utilizamos el siguiente paquete:
install.packages("rworldxtra")
install.packages("rgeos")
library(rgeos)
library(rworldxtra)

#Obtener un world map
wmap <- getMap(resolution="high")
head(wmap@data)

centroids <- gCentroid(wmap, byid=TRUE)
plot(centroids)
df <- as.data.frame(centroids)
head(df)
gCentroid(centroids, byid = F, id = )
View(centroids)
##Para los estados de USA:
install.packages("USAboundaries")
install.packages("namespace")
library(USAboundaries)
library(datasets)
States = us_boundaries(states = )
States = us_boundaries()

# crear vectores de valores para las regresiones (Riqueza de parásitos = Explicativo. Riqueza de aves Vector 2)
#vector1=values(richparEu)[which(values(richparEu)>0)]
#Europa
vector2=values(matrizaveseuropeas$Richness_Raster)[which(values(matrizaveseuropeas$Richness_Raster)>0)]
vector1=values(richparEu)[which(values(matrizaveseuropeas$Richness_Raster)>0)]
#N.America
vector4=values(matrizavesamericanas$Richness_Raster)[which(values(matrizavesamericanas$Richness_Raster)>0)]
vector3=values(richparAm)[which(values(matrizavesamericanas$Richness_Raster)>0)]

#Europa
vector6=values(Euraster)[which(values(Euraster)!=0)]
vector5=values(richparEu)[which(values(Euraster)!=0)]
#N.America
vector8=values(Amraster)[which(values(Amraster)!=0)]
vector7=values(richparAm)[which(values(Amraster)!=0)]

vecpru=which(values(Amraster)!=0)
points(coordinates(richparAm)[which(values(Amraster)!=0),])
points(coordinates(richparAm)[vecpru[negvals],],col="red")

which(vector7[negvals])
negvals

length(which(is.na(vector8)))

plot(richparAm)

##Regresión Riqueza parásitos~riqueza de aves
#Europa
modeloreg1=lm(vector2~vector1)
summary(modeloreg1)

plot(vector1,vector2,main= "Europa", cex.main=1.4, ylab="Riqueza de Aves",xlab="Riqueza de Parásitos",cex.lab=1.4)
abline(modeloreg1,col="red", lwd=2)

plot(vector2,vector1,main= "Europa", cex.main=1.4, ylab="Riqueza de Parásitos",xlab="Riqueza de Aves",cex.lab=1.4, pch= 19)
abline(modeloreg1,col="red", lwd=2)
##lines(vector2,vector1,col="blue")

#N.América
modeloreg2=lm(vector4~vector3)
summary(modeloreg2)

plot(vector3,vector4, main= "Norte América", cex.main=1.4, ylab="Riqueza de Aves",xlab="Riqueza de Parásitos",cex.lab=1.4)
abline(modeloreg2,col="red", lwd=2)

plot(vector4,vector3,main= "Norte América", cex.main=1.4, ylab="Riqueza de Parásitos",xlab="Riqueza de Aves",cex.lab=1.4, pch= 19)
abline(modeloreg2,col="red", lwd=2)
##Regresión Riqueza parásitos~Nicho potencial
#Europa
negvals=which(vector6<0)
posvals=which(vector6>0)

modeloreg3=lm(vector5~vector6)
modeloreg3neg=lm(vector5[negvals]~vector6[negvals])
modeloreg3pos=lm(vector5[posvals]~vector6[posvals])

plot(vector6[negvals],vector5[negvals], main= "Europa", cex.main=1.4, ylab="Riqueza de Parásitos",
     xlab="",cex.lab=1.4, pch= 19)
abline(modeloreg3neg,col="red", lwd=2)

plot(vector6[posvals],vector5[posvals], main= "Europa", cex.main=1.4, ylab="Riqueza de Parásitos",
     xlab="Riqueza de aves",cex.lab=1.4, pch= 19)
abline(modeloreg3pos,col="red", lwd=2)

modeloreg3=lm(vector6~vector5)
summary(modeloreg3)

plot(vector5,vector6, main= "Europa", cex.main=1.4, ylab="Riqueza de Aves",xlab="Riqueza de Parásitos",cex.lab=1.4, pch= 19)
abline(modeloreg3,col="red", lwd=2)

plot(vector6,vector5, main= "Europa", cex.main=1.4, ylab="Riqueza de Parásitos",xlab="Riqueza de Aves",cex.lab=1.4, pch= 19)
abline(modeloreg3,col="red", lwd=2)

summary(modeloreg3neg)
summary(modeloreg3pos)

#N.América
negvals=which(vector8<0)
posvals=which(vector8>0)

modeloreg4=lm(vector7~vector8)
modeloreg4neg=lm(vector7[negvals]~vector8[negvals])
modeloreg4pos=lm(vector7[posvals]~vector8[posvals])

summary(modeloreg4)

plot(vector7,vector8, main= "Norte América", cex.main=1.4, ylab="Riqueza de Aves",xlab="Riqueza de Parásitos",cex.lab=1.4, pch= 19)
abline(modeloreg4,col="red", lwd=2)

plot(vector8,vector7, main= "Norte América", cex.main=1.4, ylab="Riqueza de Parásitos",
     xlab="Riqueza de aves",cex.lab=1.4, pch= 19)

abline(modeloreg4neg,col="red", lwd=2)
abline(modeloreg4pos,col="red", lwd=2)

plot(vector8[negvals],vector7[negvals], main= "Norte América", cex.main=1.4, ylab="Riqueza de Parásitos",
     xlab="Riqueza de aves",cex.lab=1.4, pch= 19)
plot(vector8[posvals],vector7[posvals], main= "Norte América", cex.main=1.4, ylab="Riqueza de Parásitos",
     xlab="Riqueza de aves",cex.lab=1.4, pch= 19)
modeloreg4neg=lm(vector7[negvals]~vector8[negvals])
summary(modeloreg4neg)
summary(modeloreg4pos)

plot(Amraster)

#¿Existe una relación entre AUC y el tamaño de rango?

## Vamos a quitar los píxeles oceánicos en Nam
# cargar capa shp de océanos
ocean=shapefile("C:/Users/User/Desktop/Figuras TFG/ne_10m_ocean/ne_10m_ocean.shp")
plot(ocean)

rasprueba=matrizavesamericanasT6$Richness_Raster
plot(rasprueba)

OceanNam=crop(ocean,extent(rasprueba))
oceanras=rasterize(ocean)

save.image(file = "CMolineroTFG.RData")


