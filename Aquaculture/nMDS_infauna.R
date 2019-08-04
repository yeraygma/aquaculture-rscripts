#SRCIPT DE NON PARAMETRIC MDS Ó NMDS PARA PROYECTO DE MUSEO ATLANTICO DE LANZAROTE
#Calcula la similaridad de comunidades. Análisis multivariado


rm(list=ls()) #clear workspace
graphics.off() 
#libraries


#SET WORKING DIRECTORY AND LOAD DATA FILE
setwd ("C:\\Users\\Administrator\\Desktop\\informe\\nMDS")

#GET DATA
#data2<-read.table("C:\\Users\\administrator\\Documents\\R\\MDS_poliquetos\\poliquetosmds.txt",dec = ",", header=T)

#el archivo tiene que estar guardado en excel en formato "text MS2" con extensión txt cuando se guarda
data<-read.table("C:\\Users\\Administrator\\Desktop\\informe\\nMDS\\data\\poliquetos_nMDS.txt", dec = ",", header=T)

#data <- lapply(data, function(x) as.numeric(as.character(x)))

#data<-data.frame(data, row.names=data$name) Si se activa esta línea se quitan los nombres de las estaciones
#data$especies<-NULL


# RUN NMDS and plot it
#hay que instalar packages permute y lattice
library(vegan)

data_NMDS<-metaMDS(data,k=2,trymax=1000)

#ordiplot(data_NMDS,type="n")
orditorp(data_NMDS,display="sites",cex=0.50,air=0.01)

# CREA POLIGONOS, INCLUYE LAS ETIQUETAS DE LOS SITES Y LES DA COLOR

treat=c(rep("Site 1",6),rep("Site 2",12))
ordiplot(data_NMDS,type="n")
ordihull(data_NMDS,groups=treat,draw="polygon",col="grey70",label=F)
orditorp(data_NMDS, display = "sites", air=0.01, cex=0.80, col = c(rep("black",6),rep("red",12)))
                                         
#Calcula el stress del plot
t<-stressplot(data_NMDS)
#representa el stress del plot
t


