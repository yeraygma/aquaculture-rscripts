#T-TEST TWO GROUPS para analizar diferencias entre dos grupos de muestras, materia orgánica o fracción fina de sedimentos entre control e impacto

rm(list=ls()) #clear workspace
graphics.off()
setwd ("D:\\R")

#red.csv2 lee los csv separados por punto y coma.desde excel crear el archivo como csv separado por comas aunque lo genere separado por puntos y comas
dat=read.csv2("D:\\R\\R_redox\\data\\adsa.csv")

control<-data.matrix(dat$control,rownames.force = NA)
impacto<-data.matrix(dat$impacto,rownames.force = NA)

control<-as.numeric(control)
impacto<-as.numeric(impacto)



#TESTS DE NORMALIDAD 
#------------------------------------------------------------------------------------------------
#TESTS PARA REQUISITOS DEL T-TEST
  #TEST NORMALIDAD
    #Test estadístico de Shapiro-Wilk. Valores de p-value >0,05 significa que es noesdistinto de la normalidad,es decir distribución normal
    shapiro.test(dat$control)
    shapiro.test(dat$impacto)

    #Métodos visuales de normalidad
        #1)test histograma
              hist(dat$control)
              hist(dat$impacto)
        #2)test qqplot
              qqnorm(dat$control)
              qqline(dat$control)
    
              qqnorm(dat$impacto)
              qqline(dat$impacto)
             
#--------------------------------------------------------------------------------------------------------------------------------------
#BOXPLOT
                    
  #con<-dat[dat$control>0,]
  #imp<-dat[dat$impacto>0,]
  #rm(dat)

  #BOX PLOT 1
  #control<-dat$control
  #impacto<-dat$impacto
  n<-c("control","impacto")
  dataList <- lapply(n, get, envir=environment())
  names(dataList)<-n
  boxplot(dataList,col="grey", main="Wilcox test REDOX ADSA", ylab="potencial REDOX (mV)")

#mtext("***", side=3, line=-13, at=2, cex=1.2,col="red")
#mtext("t = 7.4272, df = 231.135, p-value = 2.124e-12",side=3,line=-13, at=1, cex=0.8, col="black")


#calculo de la media de la zona de impacto para sulfuros y REDOX
#nu<- mean(dat$impacto)


#PAIRED TESTS
   #REALIZA EL T-TEST Student para muestras con igual tamaño muestral que presentan normalidad y homocedasticidad(con misma varianza)
   t.test(control,impacto, var.equal = TRUE, paired = TRUE)
   mtext("t = -5.2588, df = 5, p-value = 0.003302 ",side=1,line=-5, at=1, cex=1, col="black")
   #mtext("media zona impacto= -358 mV",side=1,line=-5, at=1, cex=1, col="black")
   mtext("**", side=1, line=-4, at=1, cex=1.2,col="black") # 1 estrella si es < 0,05, dos estrellas si p < 0,01 y 3 estrellas si p < 0,001.

   #si hace falta borrar el plot            
   graphics.off()                               

   #REALIZA EL T-TEST de Welch para muestras con igual o distinto tamaño muestral que presentan normalidad y no homocedasticidad(con varianza distinta)
   #t.test(control,impacto, paired= TRUE)
   #mtext("W = 21.5, p-value = 2.943e-07",side=1,line=-19, at=1, cex=1, col="black")

   #REALIZA EL WILCOXON SIGNED-RANK TEST O (TAMBIEN LLAMADO MANN-WHITNEY) EQUIVALENTE NO PARAMÉTRICO (NO DISTRIBUCIÓN NORMAL) A T-TEST PARA COMPARAR MEDIAS DE DOS GRUPOS
   wilcox.test(control,impacto, paired=TRUE)
   mtext("V = 45, p-value = 0.003906",side=1,line=-4, at=1, cex=1, col="red")
   #mtext("media zona impacto= 241 mV",side=1,line=-9, at=1, cex=1, col="black")
   mtext("*", side=1, line=-3, at=1, cex=1.2,col="red")

#si hace falta borrar el plot
graphics.off()
