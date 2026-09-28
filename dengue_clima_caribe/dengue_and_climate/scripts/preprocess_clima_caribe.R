#==================================================================
#
# DATA SET - CLIMATE VARIABLES
# Preprocesamiento desde data set de sally
# 17 de enero de 2025
#
#----------------------------------------------
# Parte 0: limpieza y depuración de datos
#---------------------------------------------
#
#==================================================================


rm(list=ls())

# Cargar las librerías necesarias
library(dplyr)
library(lubridate)
library(readr)
library(ggplot2)
library(tidyr)
library(gridExtra)
library(patchwork)



#Base de datos
DatosdengueclimaIDEAM <- read_csv("data/DatosdengueclimaIDEAM.csv")
Datosdengueclima = DatosdengueclimaIDEAM


#Calidad de los datos
#summary(Datosdengueclima) #Hay variables con valores que no son plausibles

#Corregir valores erróneos
Datosdengueclima$TM_Barranquilla<-ifelse(Datosdengueclima$TM_Barranquilla<20,NA,Datosdengueclima$TM_Barranquilla)   
Datosdengueclima$TM_SantaMarta<-ifelse(Datosdengueclima$TM_SantaMarta<20,NA,Datosdengueclima$TM_SantaMarta)       
Datosdengueclima$TM_Cartagena<-ifelse(Datosdengueclima$TM_Cartagena<20,NA,Datosdengueclima$TM_Cartagena)     

Datosdengueclima$HR_Barranquilla <-ifelse(Datosdengueclima$HR_Barranquilla<55,NA,Datosdengueclima$HR_Barranquilla)     
Datosdengueclima$HR_SantaMarta   <-ifelse(Datosdengueclima$HR_SantaMarta<55,NA,Datosdengueclima$HR_SantaMarta)      
Datosdengueclima$HR_Cartagena    <-ifelse(Datosdengueclima$HR_Cartagena<55,NA,Datosdengueclima$HR_Cartagena)     


Datosdengueclima$TM_Barranquilla_Flores  <-ifelse(Datosdengueclima$TM_Barranquilla<20,NA,Datosdengueclima$TM_Barranquilla)   
Datosdengueclima$TM_Cartagena_AerRafaelN <-ifelse(Datosdengueclima$TM_Barranquilla<20,NA,Datosdengueclima$TM_Barranquilla)   
Datosdengueclima$TM_SantaMarta_AerSimonB  <-ifelse(Datosdengueclima$TM_Barranquilla<20,NA,Datosdengueclima$TM_Barranquilla)   

Datosdengueclima$PT_Cartagena_AerRafaelN <-ifelse(Datosdengueclima$PT_Cartagena_AerRafaelN>300,NA,Datosdengueclima$PT_Cartagena_AerRafaelN)   

Datosdengueclima$HR_Barranquilla_Flores   <-ifelse(Datosdengueclima$HR_Barranquilla_Flores  <55,NA,Datosdengueclima$HR_Barranquilla_Flores  )     
Datosdengueclima$HR_SantaMarta_AerSimonB   <-ifelse(Datosdengueclima$HR_SantaMarta_AerSimonB<55,NA,Datosdengueclima$HR_SantaMarta_AerSimonB)      
Datosdengueclima$HR_Cartagena_AerRafaelN    <-ifelse(Datosdengueclima$HR_Cartagena_AerRafaelN<55,NA,Datosdengueclima$HR_Cartagena_AerRafaelN)     
