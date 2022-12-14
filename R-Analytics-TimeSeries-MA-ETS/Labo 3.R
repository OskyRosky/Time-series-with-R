########################################
#                                      #
#            Laboratorio 3             #
#                                      #
########################################

# setwd("E:/Cursos/UCR Estad?stica/UCR Series Cronol?gicas/Laboratorios/Labo3")

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

###################################################
#                                                 #
#              La media m?vil                     #
#                                                 #
###################################################

autoplot(elecsales) + xlab("A?o") + ylab("Unidad de medida GWh") +
  ggtitle("Ventas anuales de electricidad en Australaia del sur")

ma(elecsales, 5)

autoplot(elecsales, series="Data") +
  autolayer(ma(elecsales,5), series="5-MA") +
  xlab("A?or") + ylab("Unidad de medida GWh") +
  ggtitle("Ventas anuales de electricidad en Australaia del sur") +
  scale_colour_manual(values=c("Data"="grey50","5-MA"="red"),
                      breaks=c("Data","5-MA"))

par(mfrow = c(2,2))
plot(elecsales, col="gray", main = "3 a?os de MA")
lines(ma(elecsales, order = 3), col = "red", lwd=3)
plot(elecsales, col="gray", main = "5 a?os de MA")
lines(ma(elecsales, order = 5), col = "blue", lwd=3)
plot(elecsales, col="gray", main = "8 a?os de MA")
lines(ma(elecsales, order = 8), col = "green", lwd=3)
plot(elecsales, col="gray", main = "12 a?os de MA")
lines(ma(elecsales, order = 12), col = "yellow4", lwd=3)


# Medias m?viles sobre medias m?viles

beer2 <- window(ausbeer,start=1980)
ma4 <- ma(beer2, order=4, centre=FALSE)
ma2x4 <- ma(beer2, order=4, centre=TRUE)

par(mfrow = c(1,2))

plot(beer2, col="gray", main = "4 a?os de MA no-centrada")
lines(ma(beer2, order=4, centre=FALSE), col = "red", lwd=3)
plot(beer2, col="gray", main = "4 a?os de MA centrada")
lines(ma(beer2, order=4, centre=TRUE), col = "blue", lwd=3)

#Otro ejemplo de un MA centrada: equipo el?ctrico manufacturado

autoplot(elecequip, series="Data") +
  autolayer(ma(elecequip, 12, centre=TRUE), series="12-MA") +
  xlab("A?o") + ylab("Indice de nuevas ?rdenes") +
  ggtitle("Equipo el?ctrico manufacturado (Euro area)") +
  scale_colour_manual(values=c("Data"="grey","12-MA"="blue"),
                      breaks=c("Data","12-MA"))

#Ejemplo: ?Por qu? ac? es util utilizar un MA?

autoplot(elecequip, series="Data") +
  autolayer(ma(elecequip, 12), series="12-MA") +
  xlab("Year") + ylab("New orders index") +
  ggtitle("Electrical equipment manufacturing (Euro area)") +
  scale_colour_manual(values=c("Data"="grey","12-MA"="red"),
                      breaks=c("Data","12-MA"))

###################################################
#                                                 #
#             Descomposici?n                      #
#                                                 #
###################################################

################
#   Cl?sico    #
################

elecequip %>%
  autoplot() + xlab("A?o") + ggtitle("?ndice de equipamiento el?ctrico")

elecequip %>% decompose(type="additive") %>%
  autoplot() + xlab("A?o") +
  ggtitle("Descomposici?n aditiva cl?sica del ?ndice de equipamiento el?ctrico")

elecequip %>% decompose(type="multiplicative") %>%
  autoplot() + xlab("A?o") +
  ggtitle("Descomposici?n multiplicativa cl?sica del ?ndice de equipamiento el?ctrico")

#Podr?amos solo estimar la tendencia y la estacionalidad ? S?
#Utilicemos las funciones trendcycle() y seasadj()


################
#   X-11       #
################


elecequip %>% seas(x11="") -> fit    #utilizo seas(x11="") para invocar el x11
autoplot(fit) + ggtitle("Descomposici?n X11 del ?ndice de equipamiento el?ctrico")

#Otra forma de analizar la estacionalidad por mes....

fit %>% seasonal() %>% ggsubseriesplot() + ylab("Estacionalidad") + xlab("Estacionalidad")

################
#   SEATS      #
################

elecequip %>% seas() %>%  #utilizamos la funci?n seas()
  autoplot() +
  ggtitle("SEATS: descomposici?n del ?ndice de equipamiento el?ctrico")

################
#   STL        #
################

#La estrucura del STL es la siguiente

#stl(x, s.window, s.degree = 0,
#    t.window = NULL, t.degree = 1,
#    l.window = nextodd(period), l.degree = t.degree,
#    s.jump = ceiling(s.window/10),
#    t.jump = ceiling(t.window/10),
#    l.jump = ceiling(l.window/10),
#    robust = FALSE,
#    inner = if(robust)  1 else 2,
#    outer = if(robust) 15 else 0,
#    na.action = na.fail)

elecequip %>%
  stl(t.window=13, s.window="periodic", robust=TRUE) %>%   
  autoplot() + xlab("A?o")

# para saber m?s sobre stl() 
# https://www.rdocumentation.org/packages/stats/versions/3.5.1/topics/stl

################
# Pron?stico   #
################

pronostico <- stl(elecequip, t.window=13, s.window="periodic",
              robust=TRUE)


pronostico %>% forecast(method="naive") %>%
         autoplot() + ggtitle("Naive forecasts of seasonally adjusted data") + 
         ylab("New orders index")  + ylab("A?os")


###################################################
#                                                 #
#   Los m?todos de suavizamiento exponencial      #
#                                                 #
###################################################


########################################
#  Suavizamiento exponencial simple    #  
########################################

#Vamos a utilizar sobre el petroleo

oildata <- window(oil, start=1996)
autoplot(oildata) +  ylab("Petr?leo (millones de toneladas)") + xlab("A?o")


# Estimaci?n del modelo SES

mod.1 <- ses(oildata)
summary(mod.1)


# Medidas de rendimiento

round(accuracy(mod.1),2)
#AIC  178.1430
#AICc 179.8573
#BIC  180.8141

# Pron?stico para h=5

pronostico.1 <- ses(oildata, h=5)
pronostico.1

autoplot(pronostico.1) +
  autolayer(fitted(pronostico.1), series="Ajustados") +
  ggtitle("Pron?stico del petroleo para el m?todo SES") + 
  ylab("Petr?leo (millones de toneladas)") + xlab("A?o")

#?Qu? pas?? 


########################################
#  M?todo de Holt de la tendencia      #  
########################################

#Vamos a utilizar los famos datos de air

air <- window(ausair, start=1990)
autoplot(ausair) +  ylab("Pasajeros (millones de personas)") + xlab("A?o")

#Estimaci?n del modelo de Holt de tendencia

mod.2 <- holt(air)
summary(mod.2)

# Medidas de rendimiento

round(accuracy(mod.2),2)

#AIC  141.1291
#AICc 143.9863
#BIC  147.6083

# Pron?sticos

pronostico.2 <- holt(air, h=15)
pronostico.2 

autoplot(pronostico.2) +
  autolayer(fitted(pronostico.2), series="Ajustados") +
  ylab("Pasajeros (millones de personas)") + xlab("A?o")

#Estimaci?n del modelo de Holt con la amortizaci?n

mod.2.2 <- holt(air, damped=TRUE, phi = 0.9)
summary(mod.2.2)

round(accuracy(mod.2.2),2) 

round(accuracy(mod.2),2)  
round(accuracy(mod.2.2),2)

#AIC  145.3965
#AICc 148.2536
#BIC  151.8757

#Estimaci?n de los m?todos de Holt sin y con la amortizaci?n (damped)

pronostico.2 <- holt(air, h=15)
pronostico.2.2 <- holt(air, damped=TRUE, phi = 0.9, h=15)
autoplot(air) +
  autolayer(pronostico.2, series="M?todo de Holt", PI=FALSE) +
  autolayer(pronostico.2.2, series="M?todo de Holt con amortizaci?n", PI=FALSE) +
  autolayer(fitted(pronostico.2), series="Ajustados") +
  autolayer(fitted(pronostico.2.2), series="Ajustados") +
  ggtitle("Pron?stico con el m?todo de Holt") + xlab("A?o") +
  ylab("Pasajeros (millones de personas)") +
  guides(colour=guide_legend(title="Pron?stico"))


########################################
#  Holt-Winters estacional             #  
########################################

#Vamos a utilizar los famos datos de air

aust <- window(austourists,start=2005)
autoplot(aust) +  ylab("Noches de visitantes (millones)") + xlab("A?o")

#?Efecto aditivo o multiplicativo?

#Estimaci?n del modelo Holt-Winters aditivo 

fit1 <- hw(aust,seasonal="additive")
summary(fit1)


#Estimaci?n del modelo Holt-Winters multiplicativo 

fit2 <- hw(aust,seasonal="multiplicative")
summary(fit2)


#Comparaci?n de los rendimientos entre modelos

round(accuracy(fit1),2)  #Aditivo
round(accuracy(fit2),2)  #Multiplicativo

#Pron?sticos de ambos modelos

fit1 <- hw(aust,seasonal="additive",h=8)
fit2 <- hw(aust,seasonal="multiplicative", h=8)

fit1;fit2

aust <- window(austourists,start=2005)
fit1 <- hw(aust,seasonal="additive")
fit2 <- hw(aust,seasonal="multiplicative")
autoplot(aust) +
  autolayer(fit1, series="Pron?stico HW aditivo", PI=FALSE) +
  autolayer(fit2, series="Pron?stico HW multiplicative",
            PI=FALSE) +
  autolayer(fitted(fit1), series="Ajustados") +
  autolayer(fitted(fit2), series="Ajustados") +
  xlab("A?o") +
  ylab("Noches de visitantes (millones)") +
  ggtitle("Las noches de visitantes internacionales en Australia") +
  guides(colour=guide_legend(title="Pron?stico"))


#Y nos falta la amortizaci?n...

#Estimaci?n de un Holt-Winters con amortizaci?n (damped)


#Estimaci?n del modelo Holt-Winters multiplicativo amortiguado (damped)

fit3 <- hw(aust,seasonal="multiplicative", damped =TRUE)
round(accuracy(fit3),2)

#Pron?sticos

fit3 <- hw(aust,seasonal="multiplicative", damped =TRUE, h=10) # OJO al par?metro damped=TRUE

autoplot(aust) +
  autolayer(fit3, series="HW multi damped", PI=FALSE)+
  autolayer(fitted(fit3), series="Ajustados") +
  ggtitle("Las noches de visitantes internacionales en Australia amortizado") +
  guides(colour=guide_legend(title="Estimaci?n"))


###################################################
#                                                 #
#           Los modelos   ETS                     #
#                                                 #
###################################################

########################################
#  Suavizamiento exponencial simple    #  
########################################

#Vamos a utilizar sobre el petroleo

oildata <- window(oil, start=1996)
autoplot(oildata) +  ylab("Petr?leo (millones de toneladas)") + xlab("A?o")


# Estimaci?n del modelo SES

mod.1.1 <- ses(oildata)
summary(mod.1.1)

mod.1.2 <- ets(oildata,model ="ANN" )
summary(mod.1.2)

mod.1.3 <- ets(oildata,model ="MNN" )
summary(mod.1.3)

# alpha = 0.8339    |  alpha = 0.8339 
#mod.1.3  Alpha= 0.7526


# Pron?stico para h=5 de los 3 modelos

pronostico.1.1 <- ses(oildata, h=5)
pronostico.1.1

autoplot(pronostico.1.1) +
  autolayer(fitted(pronostico.1.1), series="Ajustados") +
  ggtitle("Pron?stico del petroleo para el m?todo SES") + 
  ylab("Petr?leo (millones de toneladas)") + xlab("A?o")


mod.1.2 %>% forecast(h=5) %>%
  autoplot() +
  autolayer(fitted(mod.1.2), series="Ajustados") +
  ylab("Petr?leo (millones de toneladas)") + xlab("A?o")


mod.1.3 %>% forecast(h=5) %>%
  autoplot() +
  autolayer(fitted(mod.1.3), series="Ajustados") +
  ylab("Petr?leo (millones de toneladas)") + xlab("A?o")


########################################
#  Suavizamiento con tendencia de Holt #  
########################################


air <- window(ausair, start=1990)
autoplot(ausair) +  ylab("Pasajeros (millones de personas)") + xlab("A?o")

#Estimaci?n del modelo de Holt de tendencia holt()

mod.2.1 <- holt(air)
summary(mod.2.1)

#Estimaci?n del modelo de Holt de tendencia mediante el ets()

mod.2.2 <- ets(air,  model ="AAN")
summary(mod.2.2)

# alpha = 0.796    |  alpha = 0.7959 
# beta  = 1e-04    |  beta  = 1e-04

# Medidas de rendimiento

pronostico.2 <- holt(air, h=15)
pronostico.2 

autoplot(pronostico.2) +
  autolayer(fitted(pronostico.2), series="Ajustados") +
  ylab("Pasajeros (millones de personas)") + xlab("A?o")

mod.2.2 %>% forecast(h=15) %>%
  autoplot() +
  autolayer(fitted(mod.2.2), series="Ajustados") +
  ylab("Pasajeros (millones de personas)") + xlab("A?o")

#Modelo con el amortiguamiento 

mod.2.3 <- ets(air,  model ="AAN", damped = TRUE)
summary(mod.2.3)

#alpha = 0.8794 
#beta  = 2e-04 
#phi   = 0.98 

mod.2.3 %>% forecast(h=15) %>%
  autoplot() +                                               # OJO, para el efecto de amortiguamiento
  autolayer(fitted(mod.2.3), series="Ajustados") +           # en la funci?n de ets() se debe marcar 
  ylab("Pasajeros (millones de personas)") + xlab("A?o")     # damped = TRUE


########################################
#  Suavizamiento con el Holt-Winter    #  
########################################

#Ventas mensuales de medicamentos antidiab?ticos en Australia de 1991 a 2008

head(a10) ; tail(a10)

autoplot(a10) +  ylab("Ventas de medicamentos antidiabeticos en Australia") + xlab("A?o")

mod.3.1 <- hw(a10,seasonal="multiplicative", damped =TRUE)    
summary(mod.3.1)

mod.3.2 <- ets(a10,  model ="MAM", damped =TRUE)       # ATENCION A  damped =TRUE
summary(mod.3.2)

#alpha = 0.2553  |   alpha = 0.2524 
#beta  = 0.0302  |   beta  = 0.0379 
#gamma = 1e-04   |   gamma = 1e-04 
#phi   = 0.9794  |   phi   = 0.9643 

mod.3.2 %>% forecast(h=24) %>%
  autoplot() +                                              
  autolayer(fitted(mod.3.2), series="Ajustados") +          
  ylab("Ventas de medicamentos antidiabeticos en Australia") + xlab("A?o")    

