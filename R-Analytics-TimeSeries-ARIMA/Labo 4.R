########################################
#                                      #
#            Laboratorio 4             #
#                                      #
########################################

# setwd("E:/Cursos/UCR Estadística/UCR Series Cronológicas/Laboratorios/Labo4")

############################
#                          #
#        Libreras          #
#                          #
############################

suppressMessages(library(readxl))
suppressMessages(library(tseries))
suppressMessages(library(timeSeries))
suppressMessages(library(forecast))
suppressMessages(library(TTR))
suppressMessages(library(lubridate))
suppressMessages(library(plotly))
suppressMessages(library(plyr))
suppressMessages(library(ggthemes))
suppressMessages(library(gridExtra))
suppressMessages(library(scales))
suppressMessages(library(ggplot2))
suppressMessages(library(tidyr))
suppressMessages(library(dplyr))
suppressMessages(library(smooth))
suppressMessages(library(astsa))
suppressMessages(library(fpp))
suppressMessages(library(fpp2))
suppressMessages(library(VGAMdata))
suppressMessages(library(GGally))
suppressMessages(library(seasonal))
suppressMessages(library(fpp2))
suppressMessages(library(urca))
suppressMessages(library(astsa))
suppressMessages(library(formattable))

###################################################
#                                                 #
#    Estacionaridad  y raíces unitarias           #
#                                                 #
###################################################

#######################
#   Estacionaridad    #
#######################

# Veamos la siguiente serie sobre las Ventas de medicamentos.  ¿Por qué no es estacionaria?

autoplot(a10) +  ylab("Ventas de medicamentos antidiabeticos en Australia") + xlab("Año")

#Serie haciendo primero la variancia constante

autoplot(log(a10)) +  ylab("Ventas de medicamentos antidiabeticos en Australia") + xlab("Año")

#Serie quitando el efecto de la tendencia. ¿Es estacionaria?

diff.1 <- diff(log(a10))

autoplot(diff.1) +  ylab("Ventas de medicamentos antidiabeticos en Australia") + xlab("Año")

#Serie quitando el efecto de la tendencia y la estacionalidad. ¿Es estacionaria?

diff.2 <- diff(diff.1,lag=12)

autoplot(diff.2) +  ylab("Ventas de medicamentos antidiabeticos en Australia") + xlab("Año")


#Visualización de los pasos llevados a cabo para obtener la estacionaridad del proceso

cbind("Serie normal" = a10,
      "Serie con log" = log(a10),
      "Serie con log y una diferenciación" = diff.1) %>%
  autoplot(facets=TRUE) +
  xlab("Year") + ylab("") +
  ggtitle("Ventas de antibioticos para diabéticos") 

autoplot(diff.2) +  ylab("Ventas de medicamentos antidiabeticos en Australia") + xlab("Año")


#Se puede hacer doble diferenciación haciando diff(diff())

cbind("Serie normal" = usmelec,
      "Serie con log" = log(usmelec),
      "Serie con log y diff - 12" = diff(log(usmelec),12),
      "Serie con log y doble diff - 12" =  diff(diff(log(usmelec),12),1)) %>%
  autoplot(facets=TRUE) +
  xlab("Año") + ylab("") +
  ggtitle("Monthly US net electricity generation")


#######################
#   Raíces unitarias  #
#######################

#Raíz unitaria

#Datos de google


autoplot(goog) +  ylab("Datos de las acciones de Google en el NASDAQ ") + 
                xlab("Del 23 Feb 2013 - 13 Febrero 2014")


ndiffs(goog)

#Datos de la demanda electrica


autoplot(usmelec) +  ylab("Billones de electricidad en Kb ") + 
                      xlab("Años") +
                      ggtitle("Monthly US net electricity generation")


usmelec %>% log() %>% nsdiffs()

usmelec %>% log() %>% diff(lag=12) %>% ndiffs()




#Datos sobre ventas de medicamentos para diabéticos

autoplot(a10) +  ylab("Ventas de medicamentos antidiabeticos en Australia") + xlab("Año")


a<- a10
b<- log(a10)
c<- diff(log(a10),1)
d<-diff(log(a10),1,12)

a10 %>% log() %>% nsdiffs()


a %>% log() %>% nsdiffs()
b %>% log() %>% nsdiffs()
c %>% log() %>% nsdiffs()
d %>% log() %>% nsdiffs()



###################################################
#                                                 #
#   Identificación y estimación del proceso       #
#            ARIMA (p,d,q)                        #
#                                                 #
###################################################


# Simulate 100 observations from an AR(2) Process 

ar.sim<-arima.sim(model=list(ar=c(.9,-.2)),n=100) 
ggtsdisplay(ar.sim)

# Simulate 100 observations from an MA(2) Process

ma.sim<-arima.sim(model=list(ma=c(.8,.4)),n=100)
ggtsdisplay(ma.sim)


#Simulate 100 observations from an ARMA(2,2) Process 

arma.sim<-arima.sim(model=list(ar=c(.9,-.2),ma=c(-.7,.1)),n=100) 
ggtsdisplay(arma.sim)



#Simulemos unos proceso más simples, y veamos que no se debería tomar al dedo los
#intervalos de confianza


x1 <- arima.sim(model = list(order = c(1, 0, 0), ar = .9), n = 100) 
acf2(x1)

x2 <- arima.sim(model = list(order = c(1, 0, 0), ar = .9), n = 200) 
acf2(x2)

x3 <- arima.sim(model = list(order = c(1, 0, 0), ar = .9), n = 400) 
acf2(x3)


###################################################
#                                                 #
#    Estimación del proceso ARIMA (p,d,q)         #
#                                                 #
###################################################

###########
#  AR(1)  #
###########

x <- arima.sim(model = list(order = c(1, 0, 0), ar = .9), n = 100) 

autoplot(x) +  ylab("") +  xlab("Rezagos") + 
            ggtitle("Proceso AR(1) con n=100") 
acf2(x)

#Estimación del proceso AR(1)

ar.1 <- sarima(x,p=1,d=0,q=0)
ar.1
names(ar.1)

ar.1$fit
ar.1$ttable


###########
#  MA(1)  #
###########

y <- arima.sim(model = list(order = c(0, 0, 1), ma = -.8), n = 100)

autoplot(y) +  ylab("") +  xlab("Rezagos") + 
  ggtitle("Proceso MA(1) con n=100") 
acf2(y)


#Estimación del proceso MA (1)

ma.1 <- sarima(y,p=0,d=0,q=1)
ma.1


ma.1$fit
ma.1$ttable

##############
#  ARMA(2,1) #
##############

z <- arima.sim(model = list(order = c(2, 0, 1), ar = c(0.8, -.9), ma = .8), n = 250)

autoplot(z) +  ylab("") +  xlab("Rezagos") + 
  ggtitle("Proceso ARMA(2,1) con n=100") 
acf2(z)


#Estimación del proceso MA (1)

arma.2.1 <- sarima(z,p=2,d=0,q=1)
arma.2.1


arma.2.1$fit
arma.2.1$ttable


###################################################
#                                                 #
#   Sobre parametrización de un ARIMA (p,d,q)     #
#                                                 #
###################################################

z <- arima.sim(model = list(order = c(2, 0, 1), ar = c(0.8, -.9), ma = .8), n = 250)
autoplot(z) +  ylab("") +  xlab("Rezagos") + 
  ggtitle("Proceso ARMA(2,1) con n=100") 
acf2(z)



mod.1 <- sarima(z,p=1,d=0,q=0)
mod.1 $ttable

mod.2 <- sarima(z,p=0,d=0,q=1)
mod.2 $ttable

mod.3 <-sarima(z,p=1,d=0,q=1)
mod.3 $ttable

mod.4 <-sarima(z,p=2,d=0,q=1)
mod.4 $ttable  

mod.5 <-sarima(z,p=2,d=0,q=2)
mod.5 $ttable

mod.6 <-sarima(z,p=3,d=0,q=2)
mod.6 $ttable  




###################################################
#                                                 #
#  El término d en el  ARIMA (p,d,q) integrated   #
#                                                 #
###################################################


# Modelo diferenciado

y <- arima.sim(model = list(order = c(1, 1, 0), ar = .9), n = 200)

autoplot(y)
acf2(y)

x <- diff(y)

autoplot(x)
acf2(x)

sarima(y,p=1,d=1,q=0)


# Estimación del archivo

autoplot(a10) +  ylab("Ventas de medicamentos antidiabeticos en Australia") + xlab("Año")
acf2(a10)

diferencia_1 <- diff(log(a10),1)
diferencia_1_12 <-  diff(diferencia_1,lag=12)


autoplot(diferencia_1_12) +  ylab("") +  xlab("Rezagos") + 
  ggtitle("Diferencia del valor del barril de petroleo") 

acf2(diferencia_1_12)

sarima(a10,0,1,1)


###################################################
#                                                 #
#  Estimación del proceso ARIMA (p,d,q)(P,D,Q)s   #
#                                                 #
###################################################

##########
# unemp  #
##########

autoplot((unemp)) +  ylab("Desempleo en USA") + xlab("Año")
acf2(unemp)

d_unemp <- diff(unemp,1)
dd_unemp <- diff(d_unemp, lag = 12) 

autoplot(dd_unemp) +  ylab("Desempleo en USA") + xlab("Año")

acf2(dd_unemp,max.lag=60)


mod1.1 <- Arima(unemp, order=c(2,1,0), seasonal=c(0,1,1))
summary(mod1.1)
#mod1 <-sarima(unemp,p=2,d=1,q=0,P=0,D=1,Q=1,S=12)


autoplot(unemp, series="Algo") +
  autolayer(fitted(mod1.1), series="ARIMA (2,1,0)(0,1,1)")+
  ggtitle("Desempleo en USA") +
  guides(colour=guide_legend(title="Estimación"))

##########
# birth  #
##########


autoplot(birth) +  ylab("Nacimiento en USA luego de la SGM") + xlab("Año")

d_birth <- diff(birth)
dd_birth <- diff(d_birth, lag = 12)

autoplot(dd_birth) +  ylab("Nacimiento en USA luego de la SGM") + xlab("Año")

acf2(dd_birth,max.lag=60)

mod1 <- Arima(birth, order=c(0,1,1), seasonal=c(0,1,1))
#mod1 <- sarima(birth,p=0,d=1,q=1,P=0,D=1,Q=1,S=12)

mod2 <- Arima(birth, order=c(1,1,1), seasonal=c(0,1,1))
#mod2 <- sarima(birth,p=1,d=1,q=1,P=0,D=1,Q=1,S=12)

autoplot(birth, series="Observados") + ylab("Nacimiento en USA luego de la SGM") +
  autolayer(fitted(mod2), series="ARIMA (1,1,1)(0,1,1)")+
  ggtitle("Nacimiento en USA luego de la SGM") +
  guides(colour=guide_legend(title="Estimación"))


##########
#  a10   #
##########

autoplot(a10) +  ggtitle("Ventas de medicamentos para diabéticos en Australia")  + xlab("Año")

diferencia_1 <- diff(log(a10),1)
diferencia_1_12 <-  diff(diferencia_1,lag=12)

autoplot(diferencia_1_12) +  ggtitle("Ventas de medicamentos para diabéticos en Australia")  + xlab("Año")

acf2(diferencia_1_12)

mod3 <- Arima(a10, order=c(1,1,1), seasonal=c(0,1,1))
#mod1 <- sarima(a10,p=1,d=1,q=1,P=0,D=1,Q=1,S=12)

autoplot(a10, series="Observados")  +
  autolayer(fitted(mod3 ), series="ARIMA (1,1,1)(0,1,1)")+
  ggtitle("Ventas de medicamentos para diabéticos en Australia") +
  guides(colour=guide_legend(title="Estimación"))


###################################################
#                                                 #
#    Diagnósticos de los mdelos estimads          #
#                                                 #
###################################################


##########
# unemp  #
##########

mod1 <-sarima(unemp,p=2,d=1,q=0,P=0,D=1,Q=1,S=12)

##########
# birth  #
##########

mod2 <- sarima(birth,p=1,d=1,q=1,P=0,D=1,Q=1,S=12)

##########
#  a10   #
##########

mod1 <- sarima(a10,p=1,d=1,q=1,P=0,D=1,Q=1,S=12)


###################################################
#                                                 #
#    Pronósticos  de los modelos estimads         #
#                                                 #
###################################################

##########
# unemp  #
##########

mod1.1 <- Arima(unemp, order=c(2,1,0), seasonal=c(0,1,1))


mod1.1 %>% forecast(h=24) %>%
  autoplot() +
  autolayer(fitted(mod1.1), series="Ajustados") +
  ylab("") + xlab("Año") + 
  ggtitle("Desempleo en USA") +
  guides(colour=guide_legend(title="Estimación"))


##########
# birth  #
##########

mod2 <- Arima(birth, order=c(1,1,1), seasonal=c(0,1,1))

mod2 %>% forecast(h=24) %>%
  autoplot() +
  autolayer(fitted(mod2), series="Ajustados") +
  ylab("") + xlab("Año") + 
  ggtitle("Nacimiento en USA luego de la SGM") +
  guides(colour=guide_legend(title="Estimación"))


##########
#  a10   #
##########

mod3 <- Arima(a10, order=c(1,1,1), seasonal=c(0,1,1))

mod3 %>% forecast(h=24) %>%
  autoplot() +
  autolayer(fitted(mod3 ), series="Ajustados")+
  ggtitle("Ventas de medicamentos para diabéticos en Australia") +
  guides(colour=guide_legend(title="Estimación"))


###################################################
#                                                 #
#    Estimación por la función auto.arima         #
#                                                 #
###################################################

##########
# unemp  #
##########

mod1.1 <- Arima(unemp, order=c(2,1,0), seasonal=c(0,1,1))
mod1.2 <- auto.arima(unemp)

summary(mod1.1)
summary(mod1.2)

a<- round(accuracy(mod1.1),2)
b<- round(accuracy(mod1.2),2)

c<-rbind(a,b)
rownames(c)  <- c("ARIMA", "auto.arima") 
c

mod1.2 %>% forecast(h=24) %>%
  autoplot() +
  autolayer(fitted(mod1.2), series="Ajustados auto.arima") +
  ylab("") + xlab("Año") + 
  ggtitle("Desempleo en USA") +
  guides(colour=guide_legend(title="Estimación"))

##########
# birth  #
##########

mod2.1 <- Arima(birth, order=c(1,1,1), seasonal=c(0,1,1))
mod2.2 <- auto.arima(birth)

summary(mod2.1)
summary(mod2.1)

a<- round(accuracy(mod2.1),2)
b<- round(accuracy(mod2.2),2)

c<-rbind(a,b)
rownames(c)  <- c("ARIMA", "auto.arima") 
c

mod2.2 %>% forecast(h=24) %>%
  autoplot() +
  autolayer(fitted(mod2.2), series="Ajustados auto.arima") +
  ylab("") + xlab("Año") + 
  ggtitle("Nacimiento en USA luego de la SGM") +
  guides(colour=guide_legend(title="Estimación"))

##########
#  a10   #
##########

mod3.1 <- Arima(a10, order=c(1,1,1), seasonal=c(0,1,1))
mod3.2 <-  auto.arima(a10)

summary(mod3.1)
summary(mod3.2)

a<- round(accuracy(mod3.1),2)
b<- round(accuracy(mod3.2),2)

c<-rbind(a,b)
rownames(c)  <- c("ARIMA", "auto.arima") 
c

mod3.2 %>% forecast(h=24) %>%
  autoplot() +
  autolayer(fitted(mod3.2), series="Ajustados auto.arima")+
  ggtitle("Ventas de medicamentos para diabéticos en Australia") +
  guides(colour=guide_legend(title="Estimación"))






