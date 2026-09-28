#==================================================================
#
# DATA SET - CLIMATE VARIABLES
# Preprocesamiento desde data set de sally
# 22 noviembre 2024
#
#----------------------------------------------
# Parte 3: Análisis estadístico-
# GRÁFICA TRIDIMENSIONAL
#---------------------------------------------
#
#==================================================================

library(dlnm)
library(gnm)
library(data.table)
library(splines)
library(readr)



################################################################################
# ANALYSIS
################################################################################

##################
# Base de datos
##################
Datosdengueclimav <- read_csv("data/caribe_data/Datosdengueclimav.csv")
datos_dlnm=Datosdengueclimav
datos_dlnm <- datos_dlnm[datos_dlnm$Año >= 2009 & datos_dlnm$Año <= 2023, ]
datos_dlnm$Fecha <- as.Date(paste(datos_dlnm$Año, datos_dlnm$Semana_Epidemiologica, 1, sep = "-"), format = "%Y-%U-%u")


#######################
#Gráficos de la serie
#######################

#Barranquilla
graphics.off()  # Cierra todos los dispositivos gráficos abiertos
par(mar=c(4,4,1,1), oma=c(0,0,4,0))
layout(matrix(1:5, nrow=5, byrow=TRUE), heights=c(1.5,1.3,1.3,1.3,1.3))
plot(Casos_Barranquilla~Fecha, data=datos_dlnm, type="l", lty=1, bty="l", col=1, xlab="",
     ylab="Dengue \ncases")
plot(BAQ_avg_temperature~Fecha, data=datos_dlnm, type="l", lty=1, bty="l", col=2, xlab="",
     ylab="Temperature avr")
plot(BAQ_avg_specific_humidity~Fecha, data=datos_dlnm, type="l", lty=1, bty="l", col=4, xlab="Date",
     ylab="specific \nhumidity")
plot(BAQ_avg_relative_humidity~Fecha, data=datos_dlnm, type="l", lty=1, bty="l", col=3, xlab="Date",
     ylab="relative \nhumidity")
plot(BAQ_total_precipitation~Fecha, data=datos_dlnm, type="l", lty=1, bty="l", col=6, xlab="Date",
     ylab="total \nprecipitation")

mtext("Casos y variables clima de Barranquilla", outer=TRUE, cex=1.2, line=2)  # Agrega el título general
par(mar=c(5,4,4,1)+0.1)



#Santa Marta
graphics.off()  # Cierra todos los dispositivos gráficos abiertos
par(mar=c(4,4,1,1), oma=c(0,0,4,0))
layout(matrix(1:5, nrow=5, byrow=TRUE), heights=c(1.5,1.3,1.3,1.3,1.3))
plot(Casos_SantaMarta~Fecha, data=datos_dlnm, type="l", lty=1, bty="l", col=1, xlab="",
     ylab="Dengue \ncases")
plot(SMR_avg_temperature~Fecha, data=datos_dlnm, type="l", lty=1, bty="l", col=2, xlab="",
     ylab="Temperature avr")
plot(SMR_avg_specific_humidity~Fecha, data=datos_dlnm, type="l", lty=1, bty="l", col=4, xlab="Date",
     ylab="specific \nhumidity")
plot(SMR_avg_relative_humidity~Fecha, data=datos_dlnm, type="l", lty=1, bty="l", col=3, xlab="Date",
     ylab="relative \nhumidity")
plot(SMR_total_precipitation~Fecha, data=datos_dlnm, type="l", lty=1, bty="l", col=6, xlab="Date",
     ylab="total \nprecipitation")

mtext("Casos y variables clima de Santa Marta", outer=TRUE, cex=1.2, line=2)  # Agrega el título general



#Cartagena
graphics.off()  # Cierra todos los dispositivos gráficos abiertos
par(mar=c(4,4,1,1), oma=c(0,0,4,0))
layout(matrix(1:5, nrow=5, byrow=TRUE), heights=c(1.5,1.3,1.3,1.3,1.3))
plot(Casos_Cartagena~Fecha, data=datos_dlnm, type="l", lty=1, bty="l", col=1, xlab="",
     ylab="Dengue \ncases")
plot(CTG_avg_temperature~Fecha, data=datos_dlnm, type="l", lty=1, bty="l", col=2, xlab="",
     ylab="Temperature avr")
plot(BAQ_avg_specific_humidity~Fecha, data=datos_dlnm, type="l", lty=1, bty="l", col=4, xlab="Date",
     ylab="specific \nhumidity")
plot(CTG_avg_relative_humidity~Fecha, data=datos_dlnm, type="l", lty=1, bty="l", col=3, xlab="Date",
     ylab="relative \nhumidity")
plot(CTG_total_precipitation~Fecha, data=datos_dlnm, type="l", lty=1, bty="l", col=6, xlab="Date",
     ylab="total \nprecipitation")

mtext("Casos y variables clima de Cartagena", outer=TRUE, cex=1.2, line=2)  # Agrega el título general




#################################
#Creación de las bases cruzadas
#################################

dftrend <- round(as.numeric(diff(range(data$date))/365.25 * 8))
btrend <- ns(data$date, knots=equalknots(data$date, dftrend-1))
data$stratum <- with(data, factor(paste(id, year, month, sep="-")))


#Base temperatura media
cbpoll <- crossbasis(data$pollen, lag=3, argvar=list(knots=c(40,100)),
                     arglag=list(knots=1), group=data$id)
#Base temperatura mínima
cbpm <- crossbasis(data$pm, lag=3, arglag=list("integer"), group=data$id)

#Base temperatura máxima
cbtmean <- crossbasis(data$tmean, lag=3, argvar=list(knots=1:2*10),
                      arglag=list(knots=1), group=data$id)

#Base humedad relativa
cbtmean <- crossbasis(data$tmean, lag=3, argvar=list(knots=1:2*10),
                      arglag=list(knots=1), group=data$id)
#Base humedad específica
cbtmean <- crossbasis(data$tmean, lag=3, argvar=list(knots=1:2*10),
                      arglag=list(knots=1), group=data$id)

#Base precipitación total
cbtmean <- crossbasis(data$tmean, lag=3, argvar=list(knots=1:2*10),
                      arglag=list(knots=1), group=data$id)


#################################
#Ajuste del modelo
#################################

mod <- gnm(y ~ cbpoll + cbpm + cbtmean + btrend + dow, eliminate=stratum, data=data,
           family=binomial)


#################################################################
#Asociaciones con los coeficientes de las variables climáticas
#################################################################

cppoll <- crosspred(cbpoll, mod, at=0:20*10, cen=0)
cppm <- crosspred(cbpm, mod, at=0:20*5, cen=0)
cptmean <- crosspred(cbtmean, mod, cen=15, by=1.5)


layout(t(1:2))
plot(cppoll, "overall", xlab="Pollen", ylab="OR", col=2,
     main="Pollen: overall cumulative exposure-response", ylim=c(0.5,3))
par(mar=c(1,1,2,1))
plot(cppoll, xlab="Pollen", zlab="OR", main="Pollen: exposure-lag-response",
     cex.axis=0.8, col=2)
layout(1)
par(mar=c(5,4,4,1)+0.1)


layout(t(1:2))
plot(cppm, var=10, "overall", xlab="PM2.5", ylab="OR", col=4,
     main="PM2.5: overall cumulative exposure-response", ylim=c(0.95,1.20))
plot(cppm, var=10, ci="b", type="p", ylab="OR", col=4, pch=19, cex=1.7,
     xlab="Lag", main="PM2.5: lag-response", lab=c(3,5,7), ylim=c(0.995,1.015))
layout(1)


layout(t(1:2))
plot(cptmean, "overall", xlab="Temperature", ylab="OR", col=3,
     main="Temperature: overall cumulative exposure-response", ylim=c(0.5,3))
par(mar=c(1,1,2,1))
plot(cptmean, xlab="Temperature", zlab="OR", ltheta=240, lphi=60, cex.axis=0.8, 
     main="Temperature: exposure-lag-response", col=3)
layout(1)
par(mar=c(5,4,4,1)+0.1)