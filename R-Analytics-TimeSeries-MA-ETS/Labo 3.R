########################################
#                                      #
#            Laboratorio 3             #
#                                      #
########################################

# setwd("E:/Cursos/UCR Estadística/UCR Series Cronológicas/Laboratorios/Labo3")

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
#              La media móvil                     #
#                                                 #
###################################################

autoplot(elecsales) + xlab("Año") + ylab("Unidad de medida GWh") +
  ggtitle("Ventas anuales de electricidad en Australaia del sur")

ma(elecsales, 5)

autoplot(elecsales, series="Data") +
  autolayer(ma(elecsales,5), series="5-MA") +
  xlab("Añor") + ylab("Unidad de medida GWh") +
  ggtitle("Ventas anuales de electricidad en Australaia del sur") +
  scale_colour_manual(values=c("Data"="grey50","5-MA"="red"),
                      breaks=c("Data","5-MA"))

par(mfrow = c(2,2))
plot(elecsales, col="gray", main = "3 años de MA")
lines(ma(elecsales, order = 3), col = "red", lwd=3)
plot(elecsales, col="gray", main = "5 años de MA")
lines(ma(elecsales, order = 5), col = "blue", lwd=3)
plot(elecsales, col="gray", main = "8 años de MA")
lines(ma(elecsales, order = 8), col = "green", lwd=3)
plot(elecsales, col="gray", main = "12 años de MA")
lines(ma(elecsales, order = 12), col = "yellow4", lwd=3)


# Medias móviles sobre medias móviles

beer2 <- window(ausbeer,start=1980)
ma4 <- ma(beer2, order=4, centre=FALSE)
ma2x4 <- ma(beer2, order=4, centre=TRUE)

par(mfrow = c(1,2))

plot(beer2, col="gray", main = "4 años de MA no-centrada")
lines(ma(beer2, order=4, centre=FALSE), col = "red", lwd=3)
plot(beer2, col="gray", main = "4 años de MA centrada")
lines(ma(beer2, order=4, centre=TRUE), col = "blue", lwd=3)

#Otro ejemplo de un MA centrada: equipo eléctrico manufacturado

autoplot(elecequip, series="Data") +
  autolayer(ma(elecequip, 12, centre=TRUE), series="12-MA") +
  xlab("Año") + ylab("Indice de nuevas órdenes") +
  ggtitle("Equipo eléctrico manufacturado (Euro area)") +
  scale_colour_manual(values=c("Data"="grey","12-MA"="blue"),
                      breaks=c("Data","12-MA"))

#Ejemplo: ¿Por qué acá es util utilizar un MA?

autoplot(elecequip, series="Data") +
  autolayer(ma(elecequip, 12), series="12-MA") +
  xlab("Year") + ylab("New orders index") +
  ggtitle("Electrical equipment manufacturing (Euro area)") +
  scale_colour_manual(values=c("Data"="grey","12-MA"="red"),
                      breaks=c("Data","12-MA"))

###################################################
#                                                 #
#             Descomposición                      #
#                                                 #
###################################################

################
#   Clásico    #
################

elecequip %>%
  autoplot() + xlab("Año") + ggtitle("Índice de equipamiento eléctrico")

elecequip %>% decompose(type="additive") %>%
  autoplot() + xlab("Año") +
  ggtitle("Descomposición aditiva clásica del índice de equipamiento eléctrico")

elecequip %>% decompose(type="multiplicative") %>%
  autoplot() + xlab("Año") +
  ggtitle("Descomposición multiplicativa clásica del índice de equipamiento eléctrico")

#Podríamos solo estimar la tendencia y la estacionalidad ? Sí
#Utilicemos las funciones trendcycle() y seasadj()


################
#   X-11       #
################


elecequip %>% seas(x11="") -> fit    #utilizo seas(x11="") para invocar el x11
autoplot(fit) + ggtitle("Descomposición X11 del índice de equipamiento eléctrico")

#Otra forma de analizar la estacionalidad por mes....

fit %>% seasonal() %>% ggsubseriesplot() + ylab("Estacionalidad") + xlab("Estacionalidad")

################
#   SEATS      #
################

elecequip %>% seas() %>%  #utilizamos la función seas()
  autoplot() +
  ggtitle("SEATS: descomposición del índice de equipamiento eléctrico")

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
  autoplot() + xlab("Año")

# para saber más sobre stl() 
# https://www.rdocumentation.org/packages/stats/versions/3.5.1/topics/stl

################
# Pronóstico   #
################

pronostico <- stl(elecequip, t.window=13, s.window="periodic",
              robust=TRUE)


pronostico %>% forecast(method="naive") %>%
         autoplot() + ggtitle("Naive forecasts of seasonally adjusted data") + 
         ylab("New orders index")  + ylab("Años")


###################################################
#                                                 #
#   Los métodos de suavizamiento exponencial      #
#                                                 #
###################################################


########################################
#  Suavizamiento exponencial simple    #  
########################################

#Vamos a utilizar sobre el petroleo

oildata <- window(oil, start=1996)
autoplot(oildata) +  ylab("Petróleo (millones de toneladas)") + xlab("Año")


# Estimación del modelo SES

mod.1 <- ses(oildata)
summary(mod.1)


# Medidas de rendimiento

round(accuracy(mod.1),2)
#AIC  178.1430
#AICc 179.8573
#BIC  180.8141

# Pronóstico para h=5

pronostico.1 <- ses(oildata, h=5)
pronostico.1

autoplot(pronostico.1) +
  autolayer(fitted(pronostico.1), series="Ajustados") +
  ggtitle("Pronóstico del petroleo para el método SES") + 
  ylab("Petróleo (millones de toneladas)") + xlab("Año")

#¿Qué pasó? 


########################################
#  Método de Holt de la tendencia      #  
########################################

#Vamos a utilizar los famos datos de air

air <- window(ausair, start=1990)
autoplot(ausair) +  ylab("Pasajeros (millones de personas)") + xlab("Año")

#Estimación del modelo de Holt de tendencia

mod.2 <- holt(air)
summary(mod.2)

# Medidas de rendimiento

round(accuracy(mod.2),2)

#AIC  141.1291
#AICc 143.9863
#BIC  147.6083

# Pronósticos

pronostico.2 <- holt(air, h=15)
pronostico.2 

autoplot(pronostico.2) +
  autolayer(fitted(pronostico.2), series="Ajustados") +
  ylab("Pasajeros (millones de personas)") + xlab("Año")

#Estimación del modelo de Holt con la amortización

mod.2.2 <- holt(air, damped=TRUE, phi = 0.9)
summary(mod.2.2)

round(accuracy(mod.2.2),2) 

round(accuracy(mod.2),2)  
round(accuracy(mod.2.2),2)

#AIC  145.3965
#AICc 148.2536
#BIC  151.8757

#Estimación de los métodos de Holt sin y con la amortización (damped)

pronostico.2 <- holt(air, h=15)
pronostico.2.2 <- holt(air, damped=TRUE, phi = 0.9, h=15)
autoplot(air) +
  autolayer(pronostico.2, series="Método de Holt", PI=FALSE) +
  autolayer(pronostico.2.2, series="Método de Holt con amortización", PI=FALSE) +
  autolayer(fitted(pronostico.2), series="Ajustados") +
  autolayer(fitted(pronostico.2.2), series="Ajustados") +
  ggtitle("Pronóstico con el método de Holt") + xlab("Año") +
  ylab("Pasajeros (millones de personas)") +
  guides(colour=guide_legend(title="Pronóstico"))


########################################
#  Holt-Winters estacional             #  
########################################

#Vamos a utilizar los famos datos de air

aust <- window(austourists,start=2005)
autoplot(aust) +  ylab("Noches de visitantes (millones)") + xlab("Año")

#¿Efecto aditivo o multiplicativo?

#Estimación del modelo Holt-Winters aditivo 

fit1 <- hw(aust,seasonal="additive")
summary(fit1)


#Estimación del modelo Holt-Winters multiplicativo 

fit2 <- hw(aust,seasonal="multiplicative")
summary(fit2)


#Comparación de los rendimientos entre modelos

round(accuracy(fit1),2)  #Aditivo
round(accuracy(fit2),2)  #Multiplicativo

#Pronósticos de ambos modelos

fit1 <- hw(aust,seasonal="additive",h=8)
fit2 <- hw(aust,seasonal="multiplicative", h=8)

fit1;fit2

aust <- window(austourists,start=2005)
fit1 <- hw(aust,seasonal="additive")
fit2 <- hw(aust,seasonal="multiplicative")
autoplot(aust) +
  autolayer(fit1, series="Pronóstico HW aditivo", PI=FALSE) +
  autolayer(fit2, series="Pronóstico HW multiplicative",
            PI=FALSE) +
  autolayer(fitted(fit1), series="Ajustados") +
  autolayer(fitted(fit2), series="Ajustados") +
  xlab("Año") +
  ylab("Noches de visitantes (millones)") +
  ggtitle("Las noches de visitantes internacionales en Australia") +
  guides(colour=guide_legend(title="Pronóstico"))


#Y nos falta la amortización...

#Estimación de un Holt-Winters con amortización (damped)


#Estimación del modelo Holt-Winters multiplicativo amortiguado (damped)

fit3 <- hw(aust,seasonal="multiplicative", damped =TRUE)
round(accuracy(fit3),2)

#Pronósticos

fit3 <- hw(aust,seasonal="multiplicative", damped =TRUE, h=10) # OJO al parámetro damped=TRUE

autoplot(aust) +
  autolayer(fit3, series="HW multi damped", PI=FALSE)+
  autolayer(fitted(fit3), series="Ajustados") +
  ggtitle("Las noches de visitantes internacionales en Australia amortizado") +
  guides(colour=guide_legend(title="Estimación"))


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
autoplot(oildata) +  ylab("Petróleo (millones de toneladas)") + xlab("Año")


# Estimación del modelo SES

mod.1.1 <- ses(oildata)
summary(mod.1.1)

mod.1.2 <- ets(oildata,model ="ANN" )
summary(mod.1.2)

mod.1.3 <- ets(oildata,model ="MNN" )
summary(mod.1.3)

# alpha = 0.8339    |  alpha = 0.8339 
#mod.1.3  Alpha= 0.7526


# Pronóstico para h=5 de los 3 modelos

pronostico.1.1 <- ses(oildata, h=5)
pronostico.1.1

autoplot(pronostico.1.1) +
  autolayer(fitted(pronostico.1.1), series="Ajustados") +
  ggtitle("Pronóstico del petroleo para el método SES") + 
  ylab("Petróleo (millones de toneladas)") + xlab("Año")


mod.1.2 %>% forecast(h=5) %>%
  autoplot() +
  autolayer(fitted(mod.1.2), series="Ajustados") +
  ylab("Petróleo (millones de toneladas)") + xlab("Año")


mod.1.3 %>% forecast(h=5) %>%
  autoplot() +
  autolayer(fitted(mod.1.3), series="Ajustados") +
  ylab("Petróleo (millones de toneladas)") + xlab("Año")


########################################
#  Suavizamiento con tendencia de Holt #  
########################################


air <- window(ausair, start=1990)
autoplot(ausair) +  ylab("Pasajeros (millones de personas)") + xlab("Año")

#Estimación del modelo de Holt de tendencia holt()

mod.2.1 <- holt(air)
summary(mod.2.1)

#Estimación del modelo de Holt de tendencia mediante el ets()

mod.2.2 <- ets(air,  model ="AAN")
summary(mod.2.2)

# alpha = 0.796    |  alpha = 0.7959 
# beta  = 1e-04    |  beta  = 1e-04

# Medidas de rendimiento

pronostico.2 <- holt(air, h=15)
pronostico.2 

autoplot(pronostico.2) +
  autolayer(fitted(pronostico.2), series="Ajustados") +
  ylab("Pasajeros (millones de personas)") + xlab("Año")

mod.2.2 %>% forecast(h=15) %>%
  autoplot() +
  autolayer(fitted(mod.2.2), series="Ajustados") +
  ylab("Pasajeros (millones de personas)") + xlab("Año")

#Modelo con el amortiguamiento 

mod.2.3 <- ets(air,  model ="AAN", damped = TRUE)
summary(mod.2.3)

#alpha = 0.8794 
#beta  = 2e-04 
#phi   = 0.98 

mod.2.3 %>% forecast(h=15) %>%
  autoplot() +                                               # OJO, para el efecto de amortiguamiento
  autolayer(fitted(mod.2.3), series="Ajustados") +           # en la función de ets() se debe marcar 
  ylab("Pasajeros (millones de personas)") + xlab("Año")     # damped = TRUE


########################################
#  Suavizamiento con el Holt-Winter    #  
########################################

#Ventas mensuales de medicamentos antidiabéticos en Australia de 1991 a 2008

head(a10) ; tail(a10)

autoplot(a10) +  ylab("Ventas de medicamentos antidiabeticos en Australia") + xlab("Año")

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
  ylab("Ventas de medicamentos antidiabeticos en Australia") + xlab("Año")    

