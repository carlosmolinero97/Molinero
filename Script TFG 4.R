##############################################################################################################
# Script and functions to run:
#' * Plots TFG 
#'
#'  Carlos Molinero Sampedro / Ignacio Morales-Castilla
#'  started may 2019
##############################################################################################################
# Script 4/x

## to start
rm(list=ls())
options(stringsAsFactors=FALSE)

## load packages
packs.to.extract<-list('raster','ncdf4','maptools','sp','foreach',
                       'doParallel','abind','stringr','rgdal','foreign')
lapply(packs.to.extract,require, character.only=T)


# set wd
setwd("~/MEGA/Work_UAH_postdoc/Teaching2018/TFG CARLOS MOLINERO/R/")


# cargar datos
# cargar datos desde environment previo de Carlos
load("CMolineroTFG.RData")


##########################################################
## limpiando datos y definiendo vectores

## remove NAs
plot(Euraster)
values(richparEu)[is.na(values(Euraster))]=NA
plot(richparEu)
Eubirdrich=matrizaveseuropeas$Richness_Raster
values(Eubirdrich)[is.na(values(Euraster))]=NA
plot(Eubirdrich)


plot(Amraster)
values(richparAm)[is.na(values(Amraster))]=NA
plot(richparAm)
Ambirdrich=matrizavesamericanas$Richness_Raster
values(Ambirdrich)[is.na(values(Amraster))]=NA
plot(Ambirdrich)


## vectors with data
#Europa
Eufilling=values(Euraster)
Euparasite=values(richparEu)
Eulats=coordinates(Euraster)[,2]
Eurichbirds=values(Eubirdrich)

#N.America
Amfilling=values(Amraster)
Amparasite=values(richparAm)
Amlats=coordinates(Amraster)[,2]
Amrichbirds=values(Ambirdrich)


##########################################################
## figura definitiva riquezas aves vs riqueza parásitos

# run models
modeloreg1=lm(vector1~vector2)
summary(modeloreg1)
modeloreg2=lm(vector3~vector4)
summary(modeloreg2)

# plot figures
par(mfrow=c(2,2),mar=c(4,4,1.5,1.5))

plot(vector4,vector3, cex.main=1.2,ylim=c(0,500), 
     ylab="Riqueza de Parásitos",xlab="Riqueza de Aves"
     ,cex.lab=1.2, pch= 19,col=adjustcolor("black",0.3))
abline(modeloreg2,col="darkgrey", lwd=3)
text(1.75,480,"A",cex=1.5)

plot(vector2,vector1,cex.main=1.2,ylim=c(0,500), 
     ylab="Riqueza de Parásitos",xlab="Riqueza de Aves",
     cex.lab=1.2, pch= 19,col=adjustcolor("black",0.3))
abline(modeloreg1,col="darkgrey", lwd=3)
text(1.5,480,"B",cex=1.5)


#####################################################
# explore and add latitudinal models 
# 
#par(mfrow=c(1,2))
plot(Amlats,Amfilling,ylim=c(-45,20), cex.main=1.2,
     ylab="Riqueza de Parásitos",xlab="Latitud",
     cex.lab=1.2, pch= 19,col=adjustcolor("black",0.3))
mAm=lm(Amfilling~poly(Amlats,2,raw=TRUE))
lines(Amlats[which(!is.na(Amfilling))],predict.lm(mAm),
      col="darkgrey",lwd=2.5)
summary(mAm)
text(12,480,"C",cex=1.5)


plot(Eulats,Euparasite,ylim=c(0,500), cex.main=1.2,
     ylab="Riqueza de Parásitos",xlab="Latitud",
     cex.lab=1.2, pch= 19,col=adjustcolor("black",0.3))
#meu=lm(Euparasite~poly(Eulats,2,raw=TRUE))
meu=lm(Euparasite~Eulats+I(Eulats^2))
lines(Eulats[which(!is.na(Euparasite))],predict.lm(meu),
      col="darkgrey",lwd=2.5)
summary(meu)
text(34,480,"D",cex=1.5)



###########################################
# Explorando y quitando  outliers en NAmerica

dev.off()
namout=which(Amlats<30 & Amfilling>(-5))
plot(Amraster)
points(coordinates(Amraster)[namout,])
Amfilling[namout]=NA


namrare=which(Amfilling<(-4) & Amfilling>(-25) &
                Amparasite>5 & Amparasite<320)

plot(Amraster)
points(coordinates(Amraster)[namrare,])
Amfilling[namrare]=NA




## explorando relaciones latitud vs. llenado de nicho térmico
par(mfrow=c(1,2))
plot(Amlats,Amfilling)
plot(Eulats,Eufilling)


##########################################################
## figura definitiva llenado térmico vs riqueza parásitos


#North America underfilling-overfilling separate
dev.off()
library(segmented)
par(mfrow=c(2,2),mar=c(4,4,1.5,1.5))
Amnegvals=which(Amfilling<(-4))
Amposvals=which(Amfilling>0)
modeloregAm=lm(Amparasite[Amnegvals]~Amfilling[Amnegvals])
summary(modeloregAm)
plot(Amfilling,Amparasite)
plot(Eufilling,Euparasite)

plot(Amfilling[Amnegvals],Amparasite[Amnegvals],cex.main=1.2,cex.lab=1.2,
     xlab="Infrallenado de nicho térmico",
     ylab="Riqueza de parásitos",ylim=c(0,500),
     pch= 19,col=adjustcolor("black",0.3))
text(-8,50,"A",cex=1.5)
#mAm=lm(Amparasite[Amnegvals]~poly(Amfilling[Amnegvals],2,raw=TRUE))
#summary(mAm)
#lines(Amfilling[Amnegvals],predict.lm(mAm),
#      col="darkgrey",lwd=2.5)

x=Amfilling[Amnegvals]
y=Amparasite[Amnegvals]
lin.mod <- lm(y~x)
segmented.mod <- segmented(lin.mod, 
                           seg.Z = ~x, psi=-40)
plot(segmented.mod,add=T,col="darkgrey",lwd=2.5)
summary(segmented.mod)
#plot(Amfilling,Amrichbirds)




#Europa all models
#

Eunegvals=which(Eufilling<(0))
Euposvals=which(Eufilling>(-1))
Euposvalsl=which(Eufilling>1)

modeloregEu=lm(Euparasite[Eunegvals]~Eufilling[Eunegvals])
summary(modeloregEu)
plot(Eufilling[Eunegvals],Euparasite[Eunegvals],cex.main=1.2,cex.lab=1.2,
     xlab="Infrallenado de nicho térmico",
     ylab="Riqueza de parásitos",ylim=c(0,500),
     pch= 19,col=adjustcolor("black",0.3))
text(-2,50,"B",cex=1.5)
#mEu=lm(Euparasite[Eunegvals]~poly(Eufilling[Eunegvals],2,raw=TRUE))
#summary(mEu)
#lines(Eufilling[Eunegvals],predict.lm(mEu),
#      col="darkgrey",lwd=2.5)

x=Eufilling[Eunegvals]
y=Euparasite[Eunegvals]
lin.mod <- lm(y~x)
segmented.mod <- segmented(lin.mod, 
                           seg.Z = ~x, psi=-4)
plot(segmented.mod,add=T,col="darkgrey",lwd=2.5)
summary(segmented.mod)


plot(Amfilling[Amposvals],Amparasite[Amposvals],cex.main=1.2,cex.lab=1.2,
     xlab="Sobrellenado de nicho térmico",
     ylab="Riqueza de parásitos",ylim=c(0,500),
     pch= 19,col=adjustcolor("black",0.3))
text(14,50,"C",cex=1.5)
x=Amfilling[Amposvals]
y=Amparasite[Amposvals]
lin.mod2 <- lm(y~x)
abline(lin.mod2,col="darkgrey",lwd=2.5)
summary(lin.mod2)

plot(Eufilling[Euposvals],Euparasite[Euposvals],cex.main=1.2,cex.lab=1.2,
     xlab="Sobrellenado de nicho térmico",
     ylab="Riqueza de parásitos",ylim=c(0,500),
     pch= 19,col=adjustcolor("black",0.3))
text(12,50,"D",cex=1.5)
x=Eufilling[Euposvalsl]
y=Euparasite[Euposvalsl]

lin.mod <- lm(y~x)
summary(lin.mod)
abline(lin.mod,col="darkgrey",lwd=2.5)

### INTERPRETACIÓN DE LA FIGURA
### Las especies de aves que se encuentran en lugares que en teoría
### no les serían propicios, tienden a tener un mayor número de
### parásitos que las que se encuentran dentro de su rango térmico
### (nicho fundamental) - paneles C-D: Esto puede deberse a
### - las especies que están más lejos de su ZTN son más vulnerables
### a los parásitos y se infectan más
### - las especies que están más lejos de su ZTN son más cosmopolitas
### y capaces de muestrear una mayor porción del territorio, y por tanto,
### tienen en promedio una mayor carga de parásitos.
### 
### En aquellos lugares donde hay menos especies de las que
### cabría esperar en base a las tolerancias térmicas de las 
### especies (coincide con las latitudes más bajas o tropicales)
### encontramos relaciones no lineales (A-B) con una drástica
### disminución de parásitos hacia las zonas más propicias (valores 
### más negativos), y a partir del punto de inflexión, una disminución
### ligera de parásitos a medida que nos acercamos a regiones con
### condiciones climáticas dentro de la ZTN de las especies.
### 
### 
### 

plot(Amlats,Amfilling,ylim=c(-45,20), cex.main=1.2,
     ylab="Llenado del Nicho Térmico",xlab="Latitud",
     cex.lab=1.2, pch= 19,col=adjustcolor("black",0.3))
mAm=lm(Amfilling~poly(Amlats,2,raw=TRUE))

plot(Eulats,Eufilling,ylim=c(-15,15), cex.main=1.2,
     ylab="Llenado del Nicho Térmico",xlab="Latitud",
     cex.lab=1.2, pch= 19,col=adjustcolor("black",0.3))
mAm=lm(Eufilling~poly(Eulats,2,raw=TRUE))

