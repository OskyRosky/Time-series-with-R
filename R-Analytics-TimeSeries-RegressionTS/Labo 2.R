########################################
#                                      #
#            Laboratorio 2             #
#                                      #
########################################

setwd("E:/Cursos/UCR Estadística/UCR Series Cronológicas/Laboratorios/Labo2")


############################
#                          #
#        Libreras          #
#                          #
############################

library(readxl)
library(tseries)
library(timeSeries)
library(forecast)
library(TTR)
library(lubridate)
library(plotly)
library(plyr)
library(ggthemes)
library(gridExtra)
library(scales)
library(ggplot2)
library(tidyr)
library(dplyr)
library(smooth)
library(astsa)
library(fpp)
library(fpp2)
library(VGAMdata)
library(GGally)

###################################################
#                                                 #
#         La tendencia como Predictor             #
#                                                 #
###################################################

library(fpp)
ausbeer # producción de cerveza por trimestre

#Visualización de la serie

serie.1 <- window(ausbeer, start=1992)
autoplot(serie.1) + xlab("Año") + ylab("Miles de litros") +
   ggtitle("Producción trimestral de cerveza en Australia de 1956 al 2008")

# Estimación de la serie utilizando únicamente la tendencia como predictor

serie.1.tendencia <- tslm(serie.1 ~ trend)
summary(serie.1.tendencia)

# Visualización de los valores observador y ajustados

autoplot(serie.1, series="Valor observados") +
  autolayer(fitted(serie.1.tendencia), series="Valores ajustados") +
  xlab("Año") + ylab("Miles de litros") +
  ggtitle("Producción trimestral de cerveza")


# Valoración de los valores ajustados por el modelo con tendencia

cbind(Data=serie.1, Fitted=fitted(serie.1.tendencia)) %>%
  as.data.frame() %>%
  ggplot(aes(x = Data, y = Fitted,
             colour = as.factor(cycle(serie.1)))) +
  geom_point() +
  ylab("Ajustados") + xlab("Valores actuales") +
  ggtitle("Producción trimestral de cerveza") +
  scale_colour_brewer(palette="Dark2", name="Trimestre") +
  geom_abline(intercept=0, slope=1)


###################################################
#                                                 #
#     La estacionalidad como predictor            #
#                                                 #
###################################################

# Estimación de la serie utilizando tanto la tendencia como la estacionalidad como predictor

serie.2.estacionalidad <- tslm(serie.1 ~ trend+ season)
summary(serie.2.estacionalidad)

# Visualización de los valores observador y ajustados

autoplot(serie.1, series="Valor observados") +
  autolayer(fitted(serie.2.estacionalidad), series="Valores ajustados") +
  xlab("Año") + ylab("Miles de litros") +
  ggtitle("Producción trimestral de cerveza")

# Valoración de los valores ajustados por el modelo con tendencia + estacionalidad

cbind(Data=serie.1, Fitted=fitted(serie.2.estacionalidad)) %>%
  as.data.frame() %>%
  ggplot(aes(x = Data, y = Fitted,
             colour = as.factor(cycle(serie.1)))) +
  geom_point() +
  ylab("Ajustados") + xlab("Valores actuales") +
  ggtitle("Producción trimestral de cerveza") +
  scale_colour_brewer(palette="Dark2", name="Trimestre") +
  geom_abline(intercept=0, slope=1)


###################################################
#                                                 #
#          Medidas de rendimiento                 #
#                                                 #
###################################################

#Rendimiento según método clásico 

accuracy(serie.2.estacionalidad)

CV(serie.2.estacionalidad)


###################################################
#                                                 #
#                El pronóstico                    #
#                                                 #
###################################################


serie.1 <- window(ausbeer, start=1992)
serie.2.estacionalidad <- tslm(serie.1 ~ trend + season)
pronostico <- forecast(serie.2.estacionalidad, h=12)  #3 años
autoplot(pronostico) +
  ggtitle("Pronóstico de la producción de cerza utilizando la regresión") +
  xlab("Año") + ylab("Miles de litros")

###################################################
#                                                 #
#       Relaciones no lineales                    #
#                                                 #
###################################################

#Vamos a utilizar el archivo de datos marathon

h <- 12

#Estimación de los modelos

fit.lin <- tslm(serie.1 ~ trend)
fcasts.lin <- forecast(fit.lin, h = h)
fit.exp <- tslm(serie.1 ~ trend, lambda = 0)
fcasts.exp <- forecast(fit.exp, h = h)

t <- time(serie.1)
t.break1 <- 1994
t.break2 <- 2002
tb1 <- ts(pmax(0, t - t.break1), start = 1992)
tb2 <- ts(pmax(0, t - t.break2), start = 2000)

fit.pw <- tslm(serie.1 ~ t + tb1 + tb2)
t.new <- t[length(t)] + seq(h)
tb1.new <- tb1[length(tb1)] + seq(h)
tb2.new <- tb2[length(tb2)] + seq(h)

newdata <- cbind(t=t.new, tb1=tb1.new, tb2=tb2.new) %>%
  as.data.frame()
fcasts.pw <- forecast(fit.pw, newdata = newdata)

fit.spline <- tslm(serie.1 ~ t + I(t^2) + I(t^3) +
                     I(tb1^3) + I(tb2^3))
fcasts.spl <- forecast(fit.spline, newdata = newdata)

#Pronósticos de los modelos

autoplot(serie.1) +
  autolayer(fitted(fit.lin), series = "Lineal") +
  autolayer(fitted(fit.exp), series = "Exponencial") +
  autolayer(fitted(fit.pw), series = "Cambio de nivel") +
  autolayer(fitted(fit.spline), series = "Spline cúbico") +
  autolayer(fcasts.pw, series="Cambio de nivel") +
  autolayer(fcasts.lin, series="Lineal", PI=FALSE) +
  autolayer(fcasts.exp, series="Exponencial", PI=FALSE) +
  autolayer(fcasts.spl, series="Spline cúbico", PI=FALSE) +
  xlab("Año") + ylab("Miles de litro") +
  ggtitle("Pronóstico de la producción de cerza utilizando la regresiónn") +
  guides(colour = guide_legend(title = " "))

###################################################
#                                                 #
#  Regresión múltiple en las series temporales    #
#                                                 #
###################################################

library(fpp2)

#Utilicemos el archivo uschange

#View(uschange)
head(uschange,5)


#Veamos dos variables temporales

autoplot(uschange[,c("Consumption","Income")]) +
  ylab("% cambio") + xlab("año")


#Relación entre el consumo y el ingreso

uschange %>%
  as.data.frame() %>%
  ggplot(aes(x=Income, y=Consumption)) +
  ylab("Consumo (trimestre % cambio)") +
  xlab("Income (trimestre % cambio)") +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)

#Regresión entre el consumo y el ingreso

regre.multi.1 <-tslm(Consumption ~ Income, data=uschange)
summary(regre.multi.1)     #Parece que el ingreso es un buen predictor del ingreso

# Sería pertinete utilizar las otras variables: Ingreso, producción, ahorros y desempleo ?

uschange %>%
  as.data.frame() %>%
  GGally::ggpairs()

# Estimación de la regresión múltiple 

regre.multi.2 <- tslm( Consumption ~ Income + Production + Unemployment + Savings, data=uschange)
summary(regre.multi.2)

# Evaluación de los valores ajustados optenidos

autoplot(uschange[,'Consumption'], series="Data") +
  autolayer(fitted(regre.multi.2), series="Fitted") +
  xlab("Year") + ylab("") +
  ggtitle("Percent change in US consumption expenditure") +
  guides(colour=guide_legend(title=" "))

cbind(Data = uschange[,"Consumption"],
      Fitted = fitted(regre.multi.2)) %>%
  as.data.frame() %>%
  ggplot(aes(x=Data, y=Fitted)) +
  geom_point() +
  xlab("Valores ajustados") +
  ylab("Valores Observados") +
  ggtitle("Porcentaje de cambio en US del consumo") +
  geom_abline(intercept=0, slope=1)

# Estadístcas de bondad y ajuste

checkresiduals(regre.multi.2)

cbind(Fitted = fitted(regre.multi.2),
      Residuals=residuals(regre.multi.2)) %>%
  as.data.frame() %>%
  ggplot(aes(x=Fitted, y=Residuals)) + geom_point()


#Pronóstico ---> pero no me funciona....  dice 
# Error in eval(predvars, data, env) : object 'Income' not found




serie.2 <- window(uschange, start=1970)
regre.multi.2 <- tslm(Consumption ~ Income + Production + Unemployment + Savings, data=uschange)

pronostico <- forecast(serie.2.estacionalidad, h=12, )  #3 años
autoplot(pronostico) +
  ggtitle("Pronóstico de la producción de cerza utilizando la regresión") +
  xlab("Año") + ylab("Miles de litros")
