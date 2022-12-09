########################################
#                                      #
#            Laboratorio 6             #
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
suppressMessages(library(MASS))
suppressMessages(library(sBIC))
suppressMessages(library(neuralnet))

#########################
#  Sitios por consultar #
#########################



###################################################
#                                                 #
#               Aplicación de NNA                 #
#                                                 #
###################################################

# https://www.r-bloggers.com/fitting-a-neural-network-in-r-neuralnet-package/

set.seed(500)
library(MASS)
data <- Boston

apply(data,2,function(x) sum(is.na(x)))


index <- sample(1:nrow(data),round(0.75*nrow(data)))
train <- data[index,]
test <- data[-index,]
lm.fit <- glm(medv~., data=train)
summary(lm.fit)
pr.lm <- predict(lm.fit,test)
MSE.lm <- sum((pr.lm - test$medv)^2)/nrow(test)


#Una NNA preparación

maxs <- apply(data, 2, max) 
mins <- apply(data, 2, min)

scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))

train_ <- scaled[index,]
test_ <- scaled[-index,]


#Estimación

library(neuralnet)
n <- names(train_)
f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))
nn <- neuralnet(f,data=train_,hidden=c(5,3),linear.output=T)

#Visualización

plot(nn)


#Predicción 

pr.nn <- compute(nn,test_[,1:13])

pr.nn_ <- pr.nn$net.result*(max(data$medv)-min(data$medv))+min(data$medv)
test.r <- (test_$medv)*(max(data$medv)-min(data$medv))+min(data$medv)

MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)

print(paste(MSE.lm,MSE.nn))


#Validación interna

par(mfrow=c(1,2))

plot(test$medv,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')

plot(test$medv,pr.lm,col='blue',main='Real vs predicted lm',pch=18, cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='LM',pch=18,col='blue', bty='n', cex=.95)


par(mfrow=c(1,1))

plot(test$medv,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
points(test$medv,pr.lm,col='blue',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend=c('NN','LM'),pch=18,col=c('red','blue'))


###################################################
#                                                 #
#               Aplicación de NNA   2             #
#                                                 #
###################################################

# https://rpubs.com/Spencer_Butt_RPubs/301131

library(tidyverse)
library(neuralnet)
library(GGally)

#################################################
#  Ejemplo de una regresión con una única capa  # 
#################################################

#Datos

Yacht_Data <- read_table(file = 'http://archive.ics.uci.edu/ml/machine-learning-databases/00243/yacht_hydrodynamics.data',
                         col_names = c('LongPos_COB', 'Prismatic_Coeff', 
                                       'Len_Disp_Ratio', 'Beam_Draut_Ratio', 
                                       'Length_Beam_Ratio','Froude_Num', 
                                       'Residuary_Resist')) %>%
  na.omit()

#Correlaciones 

ggpairs(Yacht_Data, title = "Scatterplot Matrix of the Features of the Yacht Data Set")


#Partición de datos

scale01 <- function(x){
  (x - min(x)) / (max(x) - min(x))
}
Yacht_Data <- Yacht_Data %>%
  mutate_all(scale01)

set.seed(12345)
Yacht_Data_Train <- sample_frac(tbl = Yacht_Data, replace = FALSE, size = 0.80)
Yacht_Data_Test <- anti_join(Yacht_Data, Yacht_Data_Train)


# Regresión con una sola capa oculta


set.seed(12321)
Yacht_NN1 <- neuralnet(Residuary_Resist ~ LongPos_COB + Prismatic_Coeff + 
                         Len_Disp_Ratio + Beam_Draut_Ratio + Length_Beam_Ratio +
                         Froude_Num, data = Yacht_Data_Train)


plot(Yacht_NN1, rep = 'best')

#Una medida de rendimiento

NN1_Train_SSE <- sum((Yacht_NN1$net.result - Yacht_Data_Train[, 7])^2)/2
paste("SSE: ", round(NN1_Train_SSE, 4))

#Validación

Test_NN1_Output <- compute(Yacht_NN1, Yacht_Data_Test[, 1:6])$net.result
NN1_Test_SSE <- sum((Test_NN1_Output - Yacht_Data_Test[, 7])^2)/2
NN1_Test_SSE


####################################################
#  Ejemplo de una regresión con hiper parámetros:  #
#  diversas capas                                  #
####################################################

# 2-Hidden Layers, Layer-1 4-neurons, Layer-2, 1-neuron, logistic activation
# function
set.seed(12321)
Yacht_NN2 <- neuralnet(Residuary_Resist ~ LongPos_COB + Prismatic_Coeff + Len_Disp_Ratio + 
                         Beam_Draut_Ratio + Length_Beam_Ratio + Froude_Num, data = Yacht_Data_Train, 
                       hidden = c(4, 1), act.fct = "logistic")
## Training Error
NN2_Train_SSE <- sum((Yacht_NN2$net.result - Yacht_Data_Train[, 7])^2)/2
## Test Error
Test_NN2_Output <- compute(Yacht_NN2, Yacht_Data_Test[, 1:6])$net.result
NN2_Test_SSE <- sum((Test_NN2_Output - Yacht_Data_Test[, 7])^2)/2

# Rescale for tanh activation function
scale11 <- function(x) {
  (2 * ((x - min(x))/(max(x) - min(x)))) - 1
}
Yacht_Data_Train <- Yacht_Data_Train %>% mutate_all(scale11)
Yacht_Data_Test <- Yacht_Data_Test %>% mutate_all(scale11)

# 2-Hidden Layers, Layer-1 4-neurons, Layer-2, 1-neuron, tanh activation
# function
set.seed(12321)
Yacht_NN3 <- neuralnet(Residuary_Resist ~ LongPos_COB + Prismatic_Coeff + Len_Disp_Ratio + 
                         Beam_Draut_Ratio + Length_Beam_Ratio + Froude_Num, data = Yacht_Data_Train, 
                       hidden = c(4, 1), act.fct = "tanh")
## Training Error
NN3_Train_SSE <- sum((Yacht_NN3$net.result - Yacht_Data_Train[, 7])^2)/2
## Test Error
Test_NN3_Output <- compute(Yacht_NN3, Yacht_Data_Test[, 1:6])$net.result
NN3_Test_SSE <- sum((Test_NN3_Output - Yacht_Data_Test[, 7])^2)/2

# 1-Hidden Layer, 1-neuron, tanh activation function
set.seed(12321)
Yacht_NN4 <- neuralnet(Residuary_Resist ~ LongPos_COB + Prismatic_Coeff + Len_Disp_Ratio + 
                         Beam_Draut_Ratio + Length_Beam_Ratio + Froude_Num, data = Yacht_Data_Train, 
                       act.fct = "tanh")
## Training Error
NN4_Train_SSE <- sum((Yacht_NN4$net.result - Yacht_Data_Train[, 7])^2)/2
## Test Error
Test_NN4_Output <- compute(Yacht_NN4, Yacht_Data_Test[, 1:6])$net.result
NN4_Test_SSE <- sum((Test_NN4_Output - Yacht_Data_Test[, 7])^2)/2

# Bar plot of results
Regression_NN_Errors <- tibble(Network = rep(c("NN1", "NN2", "NN3", "NN4"), 
                                             each = 2), DataSet = rep(c("Train", "Test"), time = 4), SSE = c(NN1_Train_SSE, 
                                                                                                             NN1_Test_SSE, NN2_Train_SSE, NN2_Test_SSE, NN3_Train_SSE, NN3_Test_SSE, 
                                                                                                             NN4_Train_SSE, NN4_Test_SSE))
Regression_NN_Errors %>% ggplot(aes(Network, SSE, fill = DataSet)) + geom_col(position = "dodge") + 
  ggtitle("Regression ANN's SSE")


#Veamos la red con la función de impulso y sus pesos 

plot(Yacht_NN2, rep = "best")

#Veamos la red con la función de impuls

set.seed(12321)
Yacht_NN2 <- neuralnet(Residuary_Resist ~ LongPos_COB + Prismatic_Coeff + Len_Disp_Ratio + 
                         Beam_Draut_Ratio + Length_Beam_Ratio + Froude_Num, data = Yacht_Data_Train, 
                       hidden = c(4, 1), act.fct = "tanh", rep = 10)
plot(Yacht_NN2, rep = "best")


###################################################
#                                                 #
#       NNA en las series temporales              #
#                                                 #
###################################################

################
#  sunspotarea #
################



#Pronóstico sin intervalos de confianza

fit <- nnetar(sunspotarea, lambda=0)
summary(fit)

autoplot(forecast(fit,h=30))

#Pronóstico con intervalos de confianza

fcast <- forecast(fit, PI=TRUE, h=30)
autoplot(fcast)


##########
#  oil   #
##########

#Vamos a utilizar sobre el petroleo

oildata <- window(oil, start=1996)
autoplot(oildata) +  ylab("Petróleo (millones de toneladas)") + xlab("Año")

#Estimación del modelo por NNA

mod.1 <- nnetar(oildata, lambda=0)

# Visualización de los valores observador y ajustados

autoplot(oildata, series="Valor observados") +
  autolayer(fitted(mod.1), series="Valores ajustados") +
  ylab("Petróleo (millones de toneladas)") + xlab("Año") +
  ggtitle("Venta de Petróleo ")

#Estadísticas de rendimiento 

round(accuracy(mod.1),2)

# Valores pronosticados  con intervalo de confianza

fcast.1 <- forecast(mod.1, PI=TRUE, h=10)

autoplot(fcast.1)

# Valores pronosticados  sin intervalo de confianza

mod.1 %>% forecast(h=5) %>%
  autoplot() +
  autolayer(oildata, series="Observados", PI=TRUE) +
  autolayer(fitted(mod.1), series="Ajustados") +
  ylab("Petróleo (millones de toneladas)") + xlab("Año")
  ggtitle("Pronóstico de la venta de petroleo") 


##########
# unemp  #
##########

#Veamos el desempleo en USA
  
autoplot(unemp) +  ylab("Desempleo en USA") + xlab("Año")
  
#Estimemos un auto.arima y un RNA 
  
mod2.1 <- auto.arima(unemp)
mod2.2 <- nnetar(unemp)



summary(mod2.1)
summary(mod2.2)

# Visualización de los valores observador y ajustados

autoplot(unemp, series="Valor observados") +
  autolayer(fitted(mod2.2), series="Valores ajustados") +
  ylab("Cantidad de personas en miles") + xlab("Año") +
  ggtitle("Desempleo en USA")

#Estadísticas de rendimiento 

a<- round(accuracy(mod2.1),2)
b<- round(accuracy(mod2.2),2)

c<-rbind(a,b)
rownames(c)  <- c("auto.arima", "Redes Neuronales") 
c

# Valores pronosticados  con intervalo de confianza

fcast.2 <- forecast(mod2.2, PI=TRUE, h=24)

autoplot(fcast.2)


# Valores pronosticados  sin intervalo de confianza

mod2.2 %>% forecast(h=24) %>%
  autoplot() +
  autolayer(fitted(mod2.2), series="Ajustados Redes Neuronales") +
  ylab("") + xlab("Año") + 
  ggtitle("Desempleo en USA") +
  guides(colour=guide_legend(title="Estimación"))


##########
# birth  #
##########

# Analicemos los nacimientos. Veamos la serie

autoplot(birth) +  ylab("Miles de nacimientos") + xlab("Año")

#Estimemos un auto.arima y un RNA 

mod3.1 <- auto.arima(birth)
mod3.2 <- nnetar(birth, lambda = 0)

summary(mod3.1)
summary(mod3.2)

# Visualización de los valores observador y ajustados

autoplot(birth, series="Valor observados") +
  autolayer(fitted(mod3.2), series="Valores ajustados") +
  ylab("Cantidad de personas en miles") + xlab("Año") +
  ggtitle("Nacimientos en USA / Australia")


#Veamos el ajuste interno

a<- round(accuracy(mod3.1),2)
b<- round(accuracy(mod3.2),2)

c<-rbind(a,b)
rownames(c)  <- c("Auto.ARIMA", "Redes Neuronales Artificiales") 
c

# Valores pronosticados  con intervalo de confianza

fcast.3 <- forecast(mod3.2, PI=TRUE, h=24)

autoplot(fcast.3)


# Valores pronosticados  sin intervalo de confianza

mod3.2 %>% forecast(h=24) %>%
  autoplot() +
  autolayer(fitted(mod3.2), series="Ajustados RNA") +
  ylab("") + xlab("Año") + 
  ggtitle("Nacimiento en USA luego de la SGM") +
  guides(colour=guide_legend(title="Estimación"))


##########
#  a10   #
##########

# Analicemos la venta de medicamento para diabéticos en Australia

autoplot(a10) +  ylab("Venta en miles") + xlab("Año") +
  ggtitle("Venta de medicamento para diabéticos en Australia")


#Estimemos un auto.arima y un RNA 

mod4.1 <- auto.arima(a10)
mod4.2 <- nnetar(a10)

summary(mod4.1)
summary(mod4.2)


# Visualización de los valores observador y ajustados

autoplot(a10, series="Valor observados") +
  autolayer(fitted(mod4.2), series="Valores ajustados de la RNA") +
  ylab("Venta en miles") + xlab("Año") +
  ggtitle("Venta de medicamento para diabéticos en Australia")


#Veamos el ajuste interno


a<- round(accuracy(mod4.1),2)
b<- round(accuracy(mod4.2),2)

c<-rbind(a,b)
rownames(c)  <- c("auto.arima", "Red Neuronal Artificial") 
c

# Valores pronosticados  con intervalo de confianza

fcast.4 <- forecast(mod4.2, PI=TRUE, h=24)

autoplot(fcast.4)


# Valores pronosticados  sin intervalo de confianza

mod4.2 %>% forecast(h=24) %>%
  autoplot() +
  autolayer(fitted(mod4.2), series="Ajustados auto.arima")+
  ggtitle("Ventas de medicamentos para diabéticos en Australia") +
  guides(colour=guide_legend(title="Estimación"))

