########################################
#                                      #
#            Laboratorio 5             #
#                                      #
########################################

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
suppressMessages(library(ggplot2))
suppressMessages(library(forecast))
suppressMessages(library(astsa))
suppressMessages(library(lmtest))
suppressMessages(library(fUnitRoots))
suppressMessages(library(FitARMA))
suppressMessages(library(strucchange))
suppressMessages(library(reshape))
suppressMessages(library(Rmisc))
suppressMessages(library(fBasics))

###################################################
#                                                 #
#         Una ?nica intervenci?n                  #
#                                                 #
###################################################

# https://datascienceplus.com/arima-models-and-intervention-analysis/

##########################################
#  Obtenci?n + estructura de los datos   #
##########################################

url <- "https://www.openintro.org/stat/data/arbuthnot.csv"
abhutondot <- read.csv(url, header=TRUE)
nrow(abhutondot)

head(abhutondot)

abhutondot_rs <- melt(abhutondot, id = c("year"))
head(abhutondot_rs)
tail(abhutondot_rs)


#######################################
#   Definiendo la serie como una TS   #
#######################################

#Taza de varianci?n de genero seg?n mujeres

excess_frac <- (abhutondot$boys - abhutondot$girls)/abhutondot$girls
excess_ts <- ts(excess_frac, frequency = 1, start = abhutondot$year[1])


#######################################
#  Algunas estad?sticas descriptivas  #
#######################################

basicStats(excess_frac)

#######################################
#   Definiendo la serie como una TS   #
#######################################

#Gr?fica de la serie

ggplot(data = abhutondot_rs, aes(x = year)) + geom_line(aes(y = value, colour = variable)) +
  scale_colour_manual(values = c("blue", "red"))

#Gr?fica de la varianci?n de genero seg?n mujeres

autoplot(excess_ts) +  ylab("Taza de varianci?n de las mujeres") + xlab("A?o")



#######################################
#    Identificaci?n de la serie       #
#######################################

# ?Por qu? podemos decir que nuestra serie es estacionaria?

# Realicemos el proceso de identificaci?n


acf2(excess_ts, nlag=60)


#######################################
#      Cambios estructurales          #
#######################################

#Normalmente, podemos observar un cambio o identificar la intervenci?n mediante conocimiento.
#Tambien es posible mediante 

(break_point <- breakpoints(excess_ts ~ 1)) 

plot(break_point)

summary(break_point)

#El valor m?nimo de BIC se alcanza cuando m = 1, por lo que solo se determina
#un punto de ruptura correspondiente al a?o 1670. Tracemos la serie temporal
#original contra su ruptura estructural y su intervalo de confianza.


plot(excess_ts)
lines(fitted(break_point, breaks = 1), col = 4)
lines(confint(break_point, breaks = 1))

fitted.ts <- fitted(break_point, breaks = 1)
autoplot(fitted.ts)


fitted(break_point)[1] ;  fitted(break_point)[42] ;  fitted(break_point)[43] ; fitted(break_point)[82] 

#N?tese que el valor resulta como la constante donde la media es estimada, a partir de la observaci?n 43
#vemos un cambio en el nivel de la serie.

###################################
#  Estimaci?n de 6 modelos ARIMA  #
###################################

#Se van a estimar los siguientes 6 modelos:
  
  # 1. non seasonal (1,1,1), as determined by auto.arima() within forecast package
  # 2. seasonal (1,0,0)(0,0,1)[10]
  # 3. seasonal (1,0,0)(1,0,0)[10]
  # 4. seasonal (0,0,0)(0,0,1)[10] with level shift regressor as intervention variable
  # 5. seasonal (1,0,0)(0,0,1)[10] with level shift regressor as intervention variable
  # 6. non seasonal (1,0,0) with level shift regressor as intervention variable  
  
  
###################################
#  ARIMA 1. non seasonal (1,1,1)  #
###################################

(model_1 <- auto.arima(excess_ts, stepwise = FALSE, trace = TRUE))
summary(model_1)

#######################################
#  ARIMA 2.  seasonal (1,0,0)(0,0,1)  #
#######################################

model_2 <- Arima(excess_ts, order = c(1,0,0), seasonal = list(order = c(0,0,1), period = 10), include.mean = TRUE)
summary(model_2)

#######################################
#  ARIMA 3. seasonal (1,0,0)(1,0,0)   #
#######################################

model_3 <- Arima(excess_ts, order = c(1,0,0), seasonal = list(order = c(1,0,0), period = 10), include.mean = TRUE)
summary(model_3)

########################################################
#  ARIMA 4.  seasonal (0,0,0)(0,0,1)                   #
# with level shift regressor as intervention variable  #
########################################################

#OJO!  as? es como definims la intervenci?n

level <- c(rep(0, break_point$breakpoints), rep(1, length(excess_ts) - break_point$breakpoints))
         # N?tese como introduje variables controles con la funci?n rep

model_4 <- Arima(excess_ts, order = c(0,0,0), seasonal = list(order = c(0,0,1), period = 10), xreg = level, include.mean = TRUE)
summary(model_4)

########################################################
#  ARIMA 5.  seasonal (1,0,0)(0,0,1)                   #
# with level shift regressor as intervention variable  #
########################################################

model_5 <- Arima(excess_ts, order = c(1,0,0), seasonal = list(order = c(0,0,1), period=10),  xreg = level, include.mean = TRUE)
summary(model_5)

########################################################
#  6. non seasonal (1,0,0)                             #
# with level shift regressor as intervention variable  #
########################################################

model_6 <- Arima(excess_ts, order = c(1,0,0), xreg = level, include.mean = TRUE)
summary(model_6)


##################################################
#    An?lisis de los residuos de cada uno de     #
#           los modelos estimados                #
##################################################

#El modelo n. ? 5 se retir? del an?lisis, por lo tanto,sus residuos no se verificar?n.

#############
#  Modelo 1 #
#############

checkresiduals(model_1)
LjungBoxTest(residuals(model_1), k = 2, lag.max = 20)
#sarima(excess_ts, p = 1, d = 1, q = 1)

#############
#  Modelo 2 #
#############

checkresiduals(model_2)
LjungBoxTest(residuals(model_2), k = 2, lag.max = 20)
#sarima(excess_ts, p = 1, d = 0, q = 0, P = 0, D = 0, Q = 1, S = 10)

#############
#  Modelo 3 #
#############

checkresiduals(model_3)
LjungBoxTest(residuals(model_3), k = 2, lag.max = 20)
#sarima(excess_ts, p = 1, d = 0, q = 0, P = 1, D = 0, Q = 0, S = 10)

#############
#  Modelo 4 #
#############

checkresiduals(model_4)
LjungBoxTest(residuals(model_4), k = 1, lag.max = 20)
#sarima(excess_ts, p = 0, d = 0, q = 0, P = 0, D = 0, Q = 1, S = 10, xreg = level)


#############
#  Modelo 6 #
#############

checkresiduals(model_6)
LjungBoxTest(residuals(model_6), k = 1, lag.max = 20)
#sarima(excess_ts, p = 1, d = 0, q = 0, xreg = level)

##################################################
#        Comparaci?n de modelos                  #
#                                                #
##################################################

# Finalmente, es hora de recopilar las m?tricas generales de AIC, AICc y BIC de nuestros
# cinco modelos candidatos (recuerde que el modelo n. ? 5 ha sido excluido del enjuiciamiento
# del an?lisis) y elegir el mejor modelo o el modelo final.

df <- data.frame(col_1_res = c(model_1$aic, model_2$aic, model_3$aic, model_4$aic, model_6$aic),
                 col_2_res = c(model_1$aicc, model_2$aicc, model_3$aicc, model_4$aicc, model_6$aicc),
                 col_3_res = c(model_1$bic, model_2$bic, model_3$bic, model_4$bic, model_6$bic))

colnames(df) <- c("AIC", "AICc", "BIC")
rownames(df) <- c("ARIMA(1,1,1)", 
                  "ARIMA(1,0,0)(0,0,1)[10]", 
                  "ARIMA(1,0,0)(1,0,0)[10]", 
                  "ARIMA(0,0,0)(0,0,1)[10] with level shift", 
                  "ARIMA(1,0,0) with level shift")

formattable(df)

#El modelo que proporciona las mejores m?tricas AIC, AICc y BIC al mismo tiempo
#es el modelo n. ? 4, ARIMA (0,0,0) (0,0,1) con cambio de nivel.

##################################################
#                 Pron?stico                     #
#                                                #
##################################################

 horizonte<- 15
# plot(forecast(model_4, h = horizonte, xreg = rep(1, horizonte)))

model_4 %>% forecast(h=24, xreg = rep(1, horizonte) ) %>%
  autoplot() +
  autolayer(fitted(model_4), series="Ajustados") +
  ylab("") + xlab("A?o") + 
  ggtitle("Tasa de variaci?n de genero seg?n mujeres") +
  guides(colour=guide_legend(title="Estimaci?n"))


# N?tese que el pron?stico no es el mejor, pero tambi?n hay que ver que estimamos un ruido blanco..... 
# nada excelente iba a salir de dicha serie.


###################################################
#                                                 #
#                                                 #
#         Diversas intervenciones                 #
#                                                 #
#                                                 #
###################################################

##################################################
#     Aplicnado diversas intervenciones          #
#                                                #
##################################################

#Gr?fica de la serie

ggplot(data = abhutondot_rs, aes(x = year)) + geom_line(aes(y = value, colour = variable)) +
  scale_colour_manual(values = c("blue", "red"))

#Total de personas: hombres + mujeres

abhutondot.ts <- ts(abhutondot$boys + abhutondot$girls, frequency = 1 , start = abhutondot$year[1])

#Gr?fica de la varianci?n de genero seg?n mujeres

autoplot(abhutondot.ts) +  ylab("Total de inviduos") + xlab("A?o")


summary(lm(abhutondot.ts ~ 1))


#######################################
#      Cambios estructurales          #
#######################################

(break_point <- breakpoints(abhutondot.ts ~ 1))
plot(break_point)
summary(break_point)

#El valor m?nimo de BIC se alcanza con m = 3. Tracemos la serie temporal original
#contra sus rupturas estructurales y sus intervalos de confianza.

plot(abhutondot.ts)
fitted.ts <- fitted(break_point, breaks = 3)
lines(fitted.ts, col = 4)
lines(confint(break_point, breaks = 3))

#Los niveles ajustados y las fechas de los puntos de interrupci?n son los siguientes.

unique(as.integer(fitted.ts))
breakdates(break_point, breaks = 3)

fitted.ts <- fitted(break_point, breaks = 3)
autoplot(fitted.ts)

fitted.ts

###############################################
#        Estimaci?n del modelo ARIMA          #
###############################################

#Variables de intersecci?n

x1 <-  x1<-rep(1,length(abhutondot.ts))
x2<-x1
x3<-x1
x4<-x1

#x1[1:33]<-0
#x2[1:15]<-0
#x3[1:52]<-0
#x4[1:82]<-0
  
#x<- cbind(x1,x2,x3,x4)
  
#modelo3.2<-Arima(datos.aprendi, order=c(0,1,1), include.drift = T, xreg = x)

coeftest(modelo3.2)


#checkresiduals(modelo3.2)
#g10<-autoplot(datos.aprendi, series="Observado")+
  #autolayer(fitted(modelo3.2), series="Ajustados")+ylab("Cantidad")+
  #xlab("Tiempo")+
  #theme_bw() +mithema2+theme(legend.position="bottom")+
  #ggtitle("Valores ajustados y observados por a?o") 
#g10
#source("funciones series.R")
#salida<-vis.resi(modelo3.1, retrasos = 60) 

#x1n<-rep(1,54)
#x2n<-rep(1,54)
#x3n<-rep(1,54)
#x4n<-rep(1,54)

#newdata <- cbind(x1=x1n,x2=x2n, x3=x3n,x4=x4n) %>% as.data.frame()
#f.modelo3.2<-forecast(modelo3.2, h=54, xreg=newdata)

length(abhutondot.ts)

level1 <- c(rep(1, 15), rep(0, 18), rep(2,19), rep(3,30))

abhutondot_xreg <- Arima(abhutondot.ts, 
                        order = c(0,1,1), 
                        xreg = level1, 
                        include.mean = TRUE)

summary(abhutondot_xreg)

###############################################
#        An?lisis de los residuos             #
###############################################

checkresiduals(abhutondot_xreg)
LjungBoxTest(residuals(abhutondot_xreg), k=1, lag.max=20)
#sarima(abhutondot.ts, p=0, d=1, q=1, xreg = fitted.ts)


###############################################
#        Pron?stico                           #
###############################################

level1 <- c(rep(1, 15), rep(2, 18), rep(3,19), rep(4,30))



abhutondot_xreg <- Arima(abhutondot.ts, 
                         order = c(0,1,1), 
                         xreg = level1, 
                         include.mean = TRUE)

horizonte <- 24

abhutondot_xreg %>% forecast(h=24, xreg = level1) %>%
  autoplot() +
  autolayer(fitted(abhutondot_xreg), series="Ajustados") +
  ylab("") + xlab("A?o") + 
  ggtitle("Total de personas") +
  guides(colour=guide_legend(title="Estimaci?n"))



###################################################
#                                                 #
#                                                 #
#            Valores faltantes                    #
#                                                 #
#                                                 #
###################################################

#Veamos la serie orinal de gold

autoplot(gold) + ylab("Taza de varianci?n de las mujeres") + xlab("A?o")


# Apliquemos la imputaci?n de los valores faltantes


gold2 <- na.interp(gold)

datos_gold <- data.frame(gold,gold2)

formattable(datos_gold)

# Veamos la serie con los valores imputados 

autoplot(gold2, series="Interpolated") +
  autolayer(gold, series="Original") +
  scale_colour_manual(
    values=c(`Interpolated`="red",`Original`="gray"))


###################################################
#                                                 #
#                                                 #
#             Outliers                            #
#                                                 #
#                                                 #
###################################################

# En la serie de gold vemos un valor extremo u outlier

autoplot(gold) + ylab("Taza de varianci?n de las mujeres") + xlab("A?o")

# Verifiquemos si el pico se detecta como un outlier

tsoutliers(gold)

gold[768:772]

# Limpiemos o modifiquemos dicho valor 

gold %>%
  tsclean() %>%
  ets(model ="ZZZ", damped = NULL) %>%
  forecast(h=50) %>%
  autoplot()


