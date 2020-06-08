
##############################################################################################################
# Script and functions to run:
#' * Extract bird species for which we have thermal tolerance data 
#'
#'  Carlos Molinero Sampedro / Ignacio Morales-Castilla
#'  started 22 Jan 2019
##############################################################################################################
# SCRIPT 1/x

#### 1. Extraer las especies de aves para las que tenemos tolerancias termicas ####

## to start
rm(list=ls())
options(stringsAsFactors=FALSE)

## load packages
packs.to.extract<-list('raster','ncdf4','maptools','sp','foreach',
                       'doParallel','abind','stringr','rgdal','foreign')
lapply(packs.to.extract,require, character.only=T)

# set wd
setwd()

# cargar datos
load("birds.allsps.shape.RData")
# carga el archivo AA con todos los rangos de todas las aves

#### 1.1 Explorar los datos de tolerancias térmicas ####
#tolerancias=read.csv("GlobalTherm Actualizado 2.csv")
tolerancias=read.table("GlobalTherm 4.csv",header = T,sep=";")
head(tolerancias)

# explorar datos (cuantas especies hay, lista de nombres, etc.)
head(AA)
length(unique(AA@data$SCINAME))

#### 1.2 Comprobación de qué especies tienen datos de tolerancias y distribucion ####

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
#plot(avestoleranciamaps)

#### 1.3 Extraccion de matriz presencias y ausencias ####
library(letsR)

matrizaves= lets.presab(avestoleranciamaps,resol=20)
show(matrizaves)
matrizaveseuropeas= lets.presab(avestoleranciamaps, xmn = -28, xmx = 38, ymn = 32, ymx = 72, resol = 0.5)
