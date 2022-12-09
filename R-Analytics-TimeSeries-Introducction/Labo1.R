########################################
#                                      #
#            Laboratorio 1             #
#                                      #
########################################

 setwd("E:/Cursos/UCR Estadística/UCR Series Cronológicas/Laboratorios/Labo1")
getwd()

############################
#                          #
#        Libreras          #
#                          #
############################

library(readxl)
library(xts)
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
library(plotly)
library(tidyr)
library(dplyr)
library(smooth)
library(astsa)
library(dygraphs)         
library(tidyverse)  # Para qué sirve este paquete... ?
library(readr)
library(DT)

###################################################
#                                                 #
#   Convertir y extraer archivos en formato TS    #
#                                                 #
###################################################

###########################################
#           Importar datos                #
###########################################

datos <- read_excel("E:/Cursos/UCR Estadística/UCR Series Cronológicas/Laboratorios/Labo1/serie.xlsx")
#View(datos)

attach(datos)
names(datos)
class(Monto)  # OJO debe pasar a tener el valor de 'ts'

################################################################
#                Asignando la clase de 'ts'                    #
################################################################

serie <-datos$Monto 
periodicidad <- 4
inicio <- c(1959,4)

serie <- ts(serie, frequency=periodicidad, start=inicio)

class(serie)  # ahora estamos bien!
head(serie) ; tail(serie)

#Se puede también utilizar la función xts
#https://cran.r-project.org/web/packages/xts/xts.pdf

################################################################
#          Asignar la clase 'ts' desde la importación          #
################################################################

#Se puede asignar una variable ts desde la importancia.
#Se hace utilizando la función as.xts de la siguiente manera

# as.xts(read.table("file"))           o
# as.xts(read.zoo("file"))

# https://cran.r-project.org/web/packages/xts/xts.pdf   
# Página 12:    as.xts         Convert Object To And From Class xts

###################################################
#                                                 #
#   Etapas de análisis de una serie cronológica   #
#                                                 #
###################################################

#################################
# 1. Descripción de la serie    #
#################################

plot(serie, main="Labo 1 de series de tiempo")

#Descomposición de la serie

desco <- decompose(serie)
desco

plot(desco)

#Se podrían agregar otras estadísticas descriptivas como el máximo, mínimo
#y cualquier medida que ayude a entender la serie.
# Más adelante se muestran otras formas de descomponer y describir una serie.

#################################
# 2. Partición de la serie      #
#################################

periodicidad <- 4
entrenamiento <- c(1959,4)
validacion <- c(1987,1)
serie <- ts(datos$Monto , frequency=periodicidad, start=inicio)  #de forma general

#Particiones en dos: el set de entrenamiento y el set de validación

train.serie <-datos$Monto [-((length(datos$Monto ) - 34):length(datos$Monto ))]
test.serie <- datos$Monto [((length(datos$Monto ) - 34):length(datos$Monto ))]
length(train.serie)
length(test.serie)

ts.train.serie <- ts(train.serie, start = entrenamiento, frequency = periodicidad)
ts.test.serie  <- ts(test.serie, start =validacion , frequency = periodicidad)

#################################
# 3.  Estimación de los modelos #
#################################

auto.arima.ts.train.serie <- auto.arima(ts.train.serie)
summary(auto.arima.ts.train.serie)
  

#################################
# 4. Medidas de rendimiento (1) #
#################################

critererios.info <- c(auto.arima.ts.train.serie$aic,
                      auto.arima.ts.train.serie$aicc,
                      auto.arima.ts.train.serie$bic) 
critererios.info

accuracy(auto.arima.ts.train.serie)


#################################
# 5. Estimación set validación  # 
#################################

estimacion.train <- forecast(ts.train.serie,
                            h=35, 
                            model = auto.arima.ts.train.serie)

estimacion.train

#################################
# 6. Medidas de rendimiento (2) #
#################################

rendi.auto.arima <- accuracy(estimacion.train, ts.test.serie)
rendi.auto.arima

#################################
# 7. Selección del mejor modelo #
#################################

# Se selecciona según los diferentes criterios de
# todos los modelos estimados

#################################
# 8. Pronóstico                 # 
#################################

auto.arima.total <- auto.arima(serie)
summary(auto.arima.total)

pronostico <-forecast(serie,h=24, 
                 model = auto.arima.ts.train.serie)

plot(forecast(serie,h=24, 
              model = auto.arima.ts.train.serie))

# No olvidar que la interpretación, contextextualización y otros del
# pronóstico es la esencia del análisis de series temporales...

###################################################
#                                                 #
#       Visualisación de datos en                 #
#                                                 #
###################################################

#La forma más sencilla 

plot(serie, main="Labo 1 de series de tiempo")

# Otras visualizaciones 1
 
library(plotly)
today <- Sys.Date()
tm <- seq(0, 600, by = 10)
x <- today - tm
y <- rnorm(length(x))
p <- plot_ly(x = ~x, y = ~y, mode = 'lines', text = paste(tm, "days from today"))
p


# Para los datos en cuestión 

datos <-read_excel("E:/Cursos/UCR Estadística/UCR Series Cronológicas/Laboratorios/Labo1/serie.xlsx")

# Variable monto
serie <-datos$Monto 
periodicidad <- 4
inicio <- c(1959,4)
serie <- ts(serie, frequency=periodicidad, start=inicio)

# Variable temporal
datos$fecha01 <- as.Date(datos$fecha)
datos$Año01 <- format(datos$fecha01, "%Y")

attach(datos)

datos$fecha01 

grafico <- plot_ly(x = ~fecha01, y = ~serie, mode = 'lines', main="Hola")
grafico

################################################################
#                Grafico                                       #
################################################################


# Otras visualizaciones 2

library(plotly)
library(zoo)

# Trends Data
trends <- read.csv("https://cdn.rawgit.com/plotly/datasets/master/Move%20to%20Canada.csv", check.names = F, stringsAsFactors = F)
trends.zoo <- zoo(trends[,-1], order.by = as.Date(trends[,1], format = "%d/%m/%Y"))
trends.zoo <- aggregate(trends.zoo, as.yearmon, mean)

trends <- data.frame(Date = index(trends.zoo),
                     coredata(trends.zoo))

# Immigration Data
immi <- read.csv("https://cdn.rawgit.com/plotly/datasets/master/Canada%20Immigration.csv", stringsAsFactors = F)

labels <- format(as.yearmon(trends$Date), "%Y")
labels <- as.character(sapply(labels, function(x){
  unlist(strsplit(x, "20"))[2]
}))

test <- labels[1]
for(i in 2:length(labels)){
  if(labels[i] == test) {
    labels[i] <- ""
  }else{
    test <- labels[i]
  }
}
labels[1] <- "2004"
hovertext1 <- paste0("Date:<b>", trends$Date, "</b><br>",
                     "From US:<b>", trends$From.US, "</b><br>")

hovertext2 <- paste0("Date:<b>", trends$Date, "</b><br>",
                     "From Britain:<b>", trends$From.Britain, "</b><br>")


p <- plot_ly(data = trends, x = ~Date) %>%
  
  # Time series chart
  
  add_lines(y = ~From.US, line = list(color = "#00526d", width = 4),
            hoverinfo = "text", text = hovertext1, name = "From US") %>%
  
  add_lines(y = ~From.Britain, line = list(color = "#de6e6e", width = 4),
            hoverinfo = "text", text = hovertext2, name = "From Britain") %>%
  
  add_markers(x = c(as.yearmon("2004-11-01"), as.yearmon("2016-03-01")),
              y = c(24, 44),
              marker = list(size = 15, color = "#00526d"),
              showlegend = F) %>%
  
  add_markers(x = c(as.yearmon("2008-07-01"), as.yearmon("2016-07-01")),
              y = c(27, 45),
              marker = list(size = 15, color = "#de6e6e"),
              showlegend = F) %>%
  
  # Markers for legend
  add_markers(x = c(as.yearmon("2005-01-01"), as.yearmon("2005-01-01")),
              y = c(40, 33.33),
              marker = list(size = 15, color = "#00526d"),
              showlegend = F) %>%
  
  add_markers(x = c(as.yearmon("2005-01-01"), as.yearmon("2005-01-01")),
              y = c(36.67, 30),
              marker = list(size = 15, color = "#de6e6e"),
              showlegend = F) %>%
  
  add_text(x = c(as.yearmon("2004-11-01"), as.yearmon("2016-03-01")),
           y = c(24, 44),
           text = c("<b>1</b>", "<b>3</b>"),
           textfont = list(color = "white", size = 8),
           showlegend = F) %>%
  
  add_text(x = c(as.yearmon("2008-07-01"), as.yearmon("2016-07-01")),
           y = c(27, 45),
           text = c("<b>2</b>", "<b>4</b>"),
           textfont = list(color = "white", size = 8),
           showlegend = F) %>%
  
  # Text for legend
  add_text(x = c(as.yearmon("2005-01-01"), as.yearmon("2005-01-01"), as.yearmon("2005-01-01"), as.yearmon("2005-01-01")),
           y = c(40, 36.67, 33.33, 30),
           text = c("<b>1</b>", "<b>2</b>", "<b>3</b>", "<b>4</b>"),
           textfont = list(color = "white", size = 8),
           showlegend = F) %>%
  
  # Bar chart
  add_bars(data = immi, x = ~Year, y = ~USA, yaxis = "y2", xaxis = "x2", showlegend = F,
           marker = list(color = "#00526d"), name = "USA") %>%
  
  add_bars(data = immi, x = ~Year, y = ~UK, yaxis = "y2", xaxis = "x2", showlegend = F,
           marker = list(color = "#de6e6e"), name = "UK") %>%
  
  layout(legend = list(x = 0.8, y = 0.36, orientation = "h", font = list(size = 10),
                       bgcolor = "transparent"),
         
         yaxis = list(domain = c(0.4, 0.95), side = "right", title = "", ticklen = 0,
                      gridwidth = 2),
         
         xaxis = list(showgrid = F, ticklen = 4, nticks = 100,
                      ticks = "outside",
                      tickmode = "array",
                      tickvals = trends$Date,
                      ticktext = labels,
                      tickangle = 0,
                      title = ""),
         
         yaxis2 = list(domain = c(0, 0.3), gridwidth = 2, side = "right"),
         xaxis2 = list(anchor = "free", position = 0),
         
         # Annotations
         annotations = list(
           list(xref = "paper", yref = "paper", xanchor = "left", yanchor = "right",
                x = 0, y = 1, showarrow = F,
                text = "<b>Your home and native land?</b>",
                font = list(size = 18, family = "Balto")),
           
           list(xref = "paper", yref = "paper", xanchor = "left", yanchor = "right",
                x = 0, y = 0.95, showarrow = F,
                align = "left",
                text = "<b>Google search volume for <i>'Move to Canada'</i></b><br><sup>100 is peak volume<br><b>Note</b> that monthly averages are used</sup>",
                font = list(size = 13, family = "Arial")),
           
           list(xref = "plot", yref = "plot", xanchor = "left", yanchor = "right",
                x = as.yearmon("2005-03-01"), y = 40, showarrow = F,
                align = "left",
                text = "<b>George W. Bush is re-elected</b>",
                font = list(size = 12, family = "Arial"),
                bgcolor = "white"),
           
           list(xref = "plot", yref = "plot", xanchor = "left", yanchor = "right",
                x = as.yearmon("2005-03-01"), y = 36.67, showarrow = F,
                align = "left",
                text = "<b>Canadian minister visits Britain, ecourages skilled workers to move</b>",
                font = list(size = 12, family = "Arial"),
                bgcolor = "white"),
           
           list(xref = "plot", yref = "plot", xanchor = "left", yanchor = "right",
                x = as.yearmon("2005-03-01"), y = 33.33, showarrow = F,
                align = "left",
                text = "<b>Super tuesday: Donald Trump wins 7 out of 11 republican primaries</b>",
                font = list(size = 12, family = "Arial"),
                bgcolor = "white"),
           
           list(xref = "plot", yref = "plot", xanchor = "left", yanchor = "right",
                x = as.yearmon("2005-03-01"), y = 30, showarrow = F,
                align = "left",
                text = "<b>Britain votes 52-48% to leave the Europen Union</b>",
                font = list(size = 12, family = "Arial"),
                bgcolor = "white"),
           
           list(xref = "paper", yref = "paper", xanchor = "left", yanchor = "right",
                x = 0, y = 0.3, showarrow = F,
                align = "left",
                text = "<b>Annual immigration to Canada</b>",
                font = list(size = 12, family = "Arial")),
           
           list(xref = "paper", yref = "paper", xanchor = "left", yanchor = "right",
                x = 0, y = -0.07, showarrow = F,
                align = "left",
                text = "<b>Source:</b> Google trends and national statistics",
                font = list(size = 12, family = "Arial")),
           
           list(xref = "paper", yref = "paper", xanchor = "left", yanchor = "right",
                x = 0.85, y = 0.98, showarrow = F,
                align = "left",
                text = 'Inspired by <a href = "http://www.economist.com/blogs/graphicdetail/2016/07/daily-chart">The economist</a>',
                font = list(size = 12, family = "Arial"))),
         
         paper_bgcolor = "#f2f2f2",
         margin = list(l = 18, r = 30, t = 18),
         width = 1024,height = 600)

print(p)

###############################
# Otras visualizaciones 3
################################


#ver el siguiente enlace
# https://moderndata.plot.ly/time-series-charts-by-the-economist-in-r-using-plotly/ 

library(plotly)
library(zoo)
library(tidyr)
library(dplyr)

# Aids Data
df <- read.csv("https://cdn.rawgit.com/plotly/datasets/master/Aids%20Data.csv", stringsAsFactors = F)

# AIDS Related Deaths ####
plot.df <- df %>%
  filter(Indicator == "AIDS-related deaths") %>%
  filter(Subgroup %in% c("All ages estimate",
                         "All ages upper estimate",
                         "All ages lower estimate"))

# Munge
plot.df <- plot.df %>%
  select(Subgroup, Time.Period, Data.Value) %>%
  spread(Subgroup, Data.Value) %>%
  data.frame()

hovertxt <- paste0("<b>Year: </b>", plot.df$Time.Period, "<br>",
                   "<b>Est.: </b>", round(plot.df$All.ages.estimate/1e6,2),"M<br>",
                   "<b>Lower est.: </b>", round(plot.df$All.ages.lower.estimate/1e6,2),"M<br>",
                   "<b>Upper est.: </b>", round(plot.df$All.ages.upper.estimate/1e6,2), "M")

# Plot
p <- plot_ly(plot.df, x = ~Time.Period, showlegend = F) %>%
  add_lines(y = ~All.ages.estimate/1e6, line = list(width = 4, color = "#1fabdd"),
            hoverinfo = "text", text = hovertxt) %>%
  add_lines(y = ~All.ages.lower.estimate/1e6, line = list(color = "#93d2ef"),
            hoverinfo = "none") %>%
  add_lines(y = ~All.ages.upper.estimate/1e6, line = list(color = "#93d2ef"),
            fill = "tonexty",
            hoverinfo = "none")

# New HIV Infections ####
plot.df <- df %>%
  filter(Indicator == "New HIV Infections") %>%
  filter(Subgroup %in% c("All ages estimate",
                         "All ages upper estimate",
                         "All ages lower estimate"))

# Munge
plot.df <- plot.df %>%
  select(Subgroup, Time.Period, Data.Value) %>%
  spread(Subgroup, Data.Value) %>%
  data.frame()

hovertxt <- paste0("<b>Year: </b>", plot.df$Time.Period, "<br>",
                   "<b>Est.: </b>", round(plot.df$All.ages.estimate/1e6,2),"M<br>",
                   "<b>Lower est.: </b>", round(plot.df$All.ages.lower.estimate/1e6,2),"M<br>",
                   "<b>Upper est.: </b>", round(plot.df$All.ages.upper.estimate/1e6,2), "M")

# Add to current plot
p <- p %>%
  add_lines(data = plot.df, y = ~All.ages.estimate/1e6, line = list(width = 4, color = "#00587b"),
            hoverinfo = "text", text = hovertxt) %>%
  add_lines(data = plot.df, y = ~All.ages.lower.estimate/1e6, line = list(color = "#3d83a3"),
            hoverinfo = "none") %>%
  add_lines(data = plot.df, y = ~All.ages.upper.estimate/1e6, line = list(color = "#3d83a3"),
            fill = "tonexty",
            hoverinfo = "none")

# People receiving ART ####
x <- c(2010:2015)
y <- c(7501470, 9134270, 10935600, 12936500, 14977200, 17023200)

hovertxt <- paste0("<b>Year:</b>", x, "<br>",
                   "<b>Est.:</b> ", round(y/1e6,2), "M")

p <- p %>%
  add_lines(x = x, y = y/1e6, line = list(width = 5, color = "#e61a20"),
            yaxis = "y2",
            hoverinfo = "text", text = hovertxt)

# Layout
p <- p %>%
  layout(xaxis = list(title = "", showgrid = F, ticklen = 4, ticks = "inside",
                      domain = c(0, 0.9)),
         yaxis = list(title = "", gridwidth = 2, domain = c(0, 0.9), range = c(-0.01, 4)),
         yaxis2 = list(overlaying = "y", side = "right", showgrid = F, color = "#e61a20",
                       range = c(5,18)),
         
         annotations = list(
           list(xref = "paper", yref = "paper", xanchor = "left", yanchor = "right",
                x = 0, y = 1, showarrow = F, align = "left",
                text = "<b>Keeping the pressure up<br><sup>Worldwide, (in millions)</sup></b>",
                font = list(size = 18, family = "Arial")),
           
           list(xref = "paper", yref = "paper", xanchor = "left", yanchor = "right",
                x = 0, y = -0.07, showarrow = F, align = "left",
                text = "<b>Source: UNAIDS</b>",
                font = list(size = 10, family = "Arial", color = "#bfbfbf")),
           
           list(xref = "plot", yref = "plot", xanchor = "left", yanchor = "right",
                x = 1995, y = 3.92, showarrow = F, align = "left",
                text = "<b>New HIV Infections(per year)</b>",
                font = list(size = 12, family = "Arial", color = "#00587b")),
           
           list(xref = "plot", yref = "plot", xanchor = "left", yanchor = "right",
                x = 1999, y = 1, showarrow = F, align = "left",
                text = "<b>AIDS related deaths (per year)</b>",
                font = list(size = 12, family = "Arial", color = "#1fabdd")),
           
           list(xref = "plot", yref = "plot", xanchor = "left", yanchor = "right",
                x = 2010, y = 3, showarrow = F, align = "left",
                text = "<b>People receving Anti-<br>Retroviral Therapy (total)</b>",
                font = list(size = 12, family = "Arial", color = "#e61a20")),
           
           list(xref = "paper", yref = "paper", xanchor = "left", yanchor = "right",
                x = 0.85, y = 0.98, showarrow = F,
                align = "left",
                text = 'Inspired by <a href = "http://www.economist.com/blogs/graphicdetail/2016/05/daily-chart-23">The economist</a>',
                font = list(size = 12, family = "Arial")),
           
           list(xref = "paper", yref = "paper", xanchor = "left", yanchor = "middle",
                x = 0.375, y = 0.9, showarrow = F, align = "left",
                text = "<b>Lower bound</b>",
                font = list(size = 10, family = "Arial", color = "#8c8c8c")),
           
           list(xref = "paper", yref = "paper", xanchor = "left", yanchor = "middle",
                x = 0.375, y = 0.95, showarrow = F, align = "left",
                text = "<b>Higher bound</b>",
                font = list(size = 10, family = "Arial", color = "#8c8c8c")),
           
           list(xref = "paper", yref = "paper", xanchor = "left", yanchor = "middle",
                x = 0.485, y = 0.925, showarrow = F, align = "left",
                text = "<b>Estimate</b>",
                font = list(size = 10, family = "Arial", color = "#8c8c8c"))
         ),
         
         shapes = list(
           list(type = "rectangle",
                xref = "paper", yref = "paper",
                x0 = 0.45, x1 = 0.48, y0 = 0.9, y1 = 0.95,
                fillcolor = "#d9d9d9",
                line = list(width = 0)),
           
           list(type = "line",
                xref = "paper", yref = "paper",
                x0 = 0.45, x1 = 0.48, y0 = 0.9, y1 = 0.9,
                line = list(width = 2, color = "#8c8c8c")),
           
           list(type = "line",
                xref = "paper", yref = "paper",
                x0 = 0.45, x1 = 0.48, y0 = 0.95, y1 = 0.95,
                fillcolor = "#bfbfbf",
                line = list(width = 2, color = "#8c8c8c")),
           
           list(type = "line",
                xref = "paper", yref = "paper",
                x0 = 0.45, x1 = 0.48, y0 = 0.925, y1 = 0.925,
                fillcolor = "#bfbfbf",
                line = list(width = 2, color = "#404040"))),
         
         height = 600,width = 1024)

print(p)

#################################################
#######  Y el más impresionante de todos ########
#################################################

library(dygraphs)
library(xts)          # To make the convertion data-frame / xts format
library(tidyverse)
library(lubridate)
library(readr)

# Se puede descargar el archivo 
#https://python-graph-gallery.com/wp-content/uploads/bike.csv

bike <- read.table("https://python-graph-gallery.com/wp-content/uploads/bike.csv", header=T, sep=",") 

#View(bike)

str(bike)

# Since my time is currently a factor, I have to convert it to a date-time format!
bike$datetime = ymd_hms(bike$datetime)

# Then you can create the xts format, and thus use dygraph
don=xts(x = bike$count, order.by = bike$datetime)
dygraph(don) %>%
  dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#D8AE5A") %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
  dyRoller(rollPeriod = 1)


###################################################
#                                                 #
#   Otro tipos de descomposiciones o formas de    #
#         describir el fenómeno                   #
#                                                 #
###################################################

# https://datamarket.com/data/set/22xr/monthly-beer-production-in-australia-megalitres-includes-ale-and-stout-does-not-include-beverages-with-alcohol-percentage-less-than-115-jan-1956-aug-1995#!ds=22xr&display=line

library(fpp)
library(astsa)
library(DT)
library(dygraphs)
library(readxl)

beer <- read_excel("E:/Cursos/UCR Estadística/UCR Series Cronológicas/Laboratorios/Labo1/monthly-beer-production-in-austr.xlsx")
#View(monthly_beer_production_in_austr)


beer.ts <- ts(beer, frequency = 12, start = c(1956,1), end = c(1994,12))

#Se puede analizar para una periodicidad más pequeña para entender mejor el crecimiento

beer.ts.qtr <- aggregate(beer.ts, nfrequency=4)
beer.ts.yr <- aggregate(beer.ts, nfrequency=1)

# Produción en trismestres y años

plot.ts(beer.ts[,2], main = "Monthly Beer Production in Australia", xlab = "Year", ylab = "ML")
plot.ts(beer.ts.qtr[,2], main = "Quarterly Beer Production in Australia", xlab = "Year", ylab = "ML")

#Producción anual suavizamiento

plot.ts(beer.ts.yr[,2], main = "Yearly Beer Production in Australia", xlab = "Year", ylab = "ML")

#Comparación entre años

seasonplot(beer.ts[,2], year.labels = TRUE, year.labels.left=TRUE, col=1:40, pch=19, main = "Monthly Beer Production in Australia - seasonplot", xlab = "Month", ylab = "ML")


#El efecto promedio por año

monthplot(beer.ts[,2], main = "Monthly Beer Production in Australia - monthplot", xlab = "Month", ylab = "ML")

#Misma temática vista desde boxplots por mes

boxplot(beer.ts[,2] ~ cycle(beer.ts[,2]), xlab = "Month", ylab = "ML", main = "Monthly Beer Production in Australia - Boxplot")


#Efecto de la media movil para ventanas de tiempo de 1, 3, 5, 10 años

par(mfrow = c(2,2))
plot(beer.ts[,2], col="gray", main = "1 Year Moving Average Smoothing")
lines(ma(beer.ts[,2], order = 12), col = "red", lwd=3)
plot(beer.ts[,2], col="gray", main = "3 Year Moving Average Smoothing")
lines(ma(beer.ts[,2], order = 36), col = "blue", lwd=3)
plot(beer.ts[,2], col="gray", main = "5 Year Moving Average Smoothing")
lines(ma(beer.ts[,2], order = 60), col = "green", lwd=3)
plot(beer.ts[,2], col="gray", main = "10 Year Moving Average Smoothing")
lines(ma(beer.ts[,2], order = 120), col = "yellow4", lwd=3)


###################################################
#                                                 #
#       Un análisis de serie de tiempo            #
#                                                 #
###################################################

#=======================================================================================================
# ESTABLECIENDO AMBIENTE DE TRABAJO Y LEYENDO LOS DATOS
#=======================================================================================================
setwd("D:/Cursos/UCR Series Cronológicas/Laboratorios/Labo1")
base <- read.csv("Datos 18 a 64 años.csv", header = T)
colnames(base)
head(base)
or.db <- ts(base$X, start = c(2007,01), frequency = 12)
train.db <- base$X[-((length(base$X) - 11):length(base$X))]
test.db <- base$X[((length(base$X) - 11):length(base$X))]
length(train.db)
length(test.db)
ts.db <- ts(train.db, start = c(2007,01), frequency = 12)
ts.test <- ts(test.db, start = c(2016,01), frequency = 12)
#=======================================================================================================
# CARGANDO PAQUETES
#=======================================================================================================
library(ggplot2)
library(TSA)
library(smooth)
library(forecast)
library(astsa)
library(lmtest)
library(FitARMA)
library(strucchange)
library(Rmisc)
library(fBasics)
library(lattice)
library(latticeExtra)
library(tseries)
#=======================================================================================================
# ANALISIS DESCRIPTIVO
#=======================================================================================================
par(font.lab = 2)
stl.intox <- stl(ts.db, "periodic")
xyplot(stl.intox, xlab = "Años")
#=======================================================================================================
# Holt Winters Method with Additive Errors
#=======================================================================================================
es.db.A <- es(ts.db, h=12, holdout=TRUE, model = "AAA", intervals = TRUE) 
es.db.A
plot(es.db.A)
forecast.esA <- forecast(es.db.A, h = 12)
ac.HWA <- accuracy(forecast.esA, ts.test)

par(mfrow = c(1,1), mai = c(1,1,0.1,0.1))
plot(or.db, ylim = c(0, max(forecast.esA$upper)), xlab = "Tiempo", ylab = "Total de Intoxicaciones")
lines(forecast.esA$fitted, col = "green", lty = 2, lwd = 2)
lines(forecast.esA$forecast, col = "blue", lwd = 2)
lines(forecast.esA$lower, col = "darkgreen", lwd = 2)
lines(forecast.esA$upper, col = "darkgreen", lwd = 2)
lines(ts.test, col = "red", lwd = 2)
legend("bottomleft", c("Serie de Entrenamiento", "Estimados HWA sobre Entrenamiento", "Pronóstico HWA para validación", "Intervalos de Confianza del 95%", "Valores Observados"), col = c("black", "green", "blue", "darkgray", "red"), lty = c(1,2,1,1,1), cex = 0.8, lwd = 2, bty = "n")
#=======================================================================================================
# Holt Winters Method with Multiplicative Errors
#=======================================================================================================
es.db.M <- es(ts.db, h=12, holdout=TRUE, model = "MAM", interval = TRUE) 
es.db.M
plot(es.db.M)
forecast.esM <- forecast(es.db.M, h = 12)
ac.HWM <- accuracy(forecast.esM, ts.test)

par(mfrow = c(1,1), mai = c(1,1,0.1,0.1))
plot(or.db, ylim = c(0, max(forecast.esM$upper)), xlab = "Tiempo", ylab = "Total de Intoxicaciones")
lines(forecast.esM$fitted, col = "green", lty = 2, lwd = 2)
lines(forecast.esM$forecast, col = "blue", lwd = 2)
lines(forecast.esM$lower, col = "darkgreen", lwd = 2)
lines(forecast.esM$upper, col = "darkgreen", lwd = 2)
lines(ts.test, col = "red", lwd = 2)
legend("bottomleft", c("Serie de Entrenamiento", "Estimados HWM sobre Entrenamiento", "Pronósticos HWA para validación", "Intervalos de Confianza del 95%", "Valores Observados"), col = c("black", "green", "blue", "darkgreen", "red"), lty = c(1,2,1,1,1), lwd = 2, cex = 0.8, bty = "n")
#=======================================================================================================
# Creating Information Criteria Table
#=======================================================================================================
IC.table <- cbind(c("HW Aditivo", "HW Multiplicativo"),as.data.frame(rbind(ac.HWA, ac.HWM)))
colnames(IC.table)[1] <- "Modelo"
IC.table
write.csv(IC.table, file = "Results Holt Winters.csv")
#=======================================================================================================
# GENERANDO CORRELOGRAMA CON AC TOTALES Y PARCIALES DE LA SERIE ORIGINAL
#=======================================================================================================
par(mfcol = c(2,1), mai = c(1,1,0.1,0.1))
Acf(ts.db, main = NA, ylab = "AC Total")
Pacf(ts.db, main = NA, ylab = "AC Parcial")
#=======================================================================================================
# APLICANDO LA PRUEBA DICKEY FULLER A LA SERIE ORIGINAL
#========================================c===============================================================
df.results.or <- adf.test(ts.db, alternative = "stationary", k = 6)   # ---> No Estacionaria
df.results.or
df.st <- round(df.results.or$statistic, 2)
p.df <- round(df.results.or$p.value, 2)
#========================================================================================================================================
# APLICANDO DIFERENCIACION DE ORDEN 1
#========================================================================================================================================
t.tsintox <- diff(ts.db, 1)
#========================================================================================================================================
# APLICANDO LA PRUEBA DICKEY FULLER A LA SERIE TRANSFORMADA
#========================================================================================================================================
df.results <- adf.test(t.tsintox, alternative = "stationary", k = 6) 
df.results
df.st <- round(df.results$statistic, 2)
p.df <- round(df.results$p.value, 2)
#========================================================================================================================================
# GENERANDO CORRELOGRAMA CON AC TOTALES Y PARCIALES DE LA SERIE TRANSFORMADA
#========================================================================================================================================
par(mfrow = c(2,1), mai = c(1,1,0.1,0.1))
Acf(t.tsintox, main = NA, ylab = "AC Total")
Pacf(t.tsintox, main = NA, ylab = "AC Parcial")
#========================================================================================================================================
# DETECTANDO CAMBIOS ESTRUCTURALES
#========================================================================================================================================
(break_point <- breakpoints(ts.db ~ 1))
par(mfrow=c(2,1), mai = c(1, 0.5, 0.1, 0.5), font.lab = 2)
plot(break_point, main = "", xlab = "Número de Intervenciones")
plot(ts.db, col = "red", xlab = "Tiempo", ylab = "Número de Intoxicaciones")
summary(break_point)
fitted.ts <- fitted(break_point, breaks = 1)
lines(fitted.ts, col = 4)
lines(confint(break_point, breaks = 1))
#========================================================================================================================================
# DEFINIENDO LA INTERVENCION
#========================================================================================================================================
unique(as.integer(fitted.ts))
breakdates(break_point, breaks = 1)
fitted.ts <- fitted(break_point, breaks = 1)
autoplot(fitted.ts)
#========================================================================================================================================
# ESTIMACION
#========================================================================================================================================ARIMA (0,1,0)(2,1,1)
# ARIMA (0,0,0)(1,1,3)
n0 <- "ARIMA (4,1,1)(4,1,1)"
mod0 <- Arima(ts.db, order = c(4,1,1), seasonal = list(order = c(4,1,1), period = 6), include.mean = TRUE)
summary(mod0)
f0 <- forecast(ts.db, h=12, model = mod0)
ac0 <- accuracy(f0)

# ARIMA (0,0,0)(1,1,1)
n1 <- "ARIMA (2,1,1)(1,1,1)"
mod1 <- Arima(ts.db, order = c(2,1,1), seasonal = list(order = c(1,1,1), period = 6), include.mean = TRUE)
summary(mod1)
f1 <- forecast(ts.db, h=12, model = mod0)
ac1 <- accuracy(f1)

# ARIMA (0,0,0)(0,1,1)
n2 <- "ARIMA (1,1,1)(1,1,1)"
mod2 <- Arima(ts.db, order = c(1,1,1), seasonal = list(order = c(1,1,1), period = 6), include.mean = TRUE)
summary(mod2)
f2 <- forecast(ts.db, h=12, model = mod2)
ac2 <- accuracy(f2)

# ARIMA (0,0,0)(1,1,0)
n3 <- "ARIMA (0,1,1)(1,1,1)"
mod3 <- Arima(ts.db, order = c(0,1,1), seasonal = list(order = c(1,1,1), period = 6), include.mean = TRUE)
summary(mod3)
f3 <- forecast(ts.db, h=12, model = mod3)
ac3 <- accuracy(f3)

# ARIMA (0,0,1)(1,1,1)
n4 <- "ARIMA (0,1,1)(1,0,1)"
mod4 <- Arima(ts.db, order = c(0,1,1), seasonal = list(order = c(1,0,1), period = 6), include.mean = TRUE)
summary(mod4)
f4 <- forecast(ts.db, h=12, model = mod4)
ac4 <- accuracy(f4)

#ARIMA (0,0,1)(1,1,1) CON INTERVENCION
n5 <- "ARIMA (0,1,1)(1,1,1) CON INTERVENCION"
mod5 <- Arima(ts.db, order = c(0,1,1), seasonal = list(order = c(1,1,1), period = 6), xreg = fitted.ts, include.mean = TRUE)
summary(mod5)
ac5 <- summary(mod5)

#Auto ARIMA Model
n6 <- "AutoARIMA (0,1,1)"
auto.mod <- auto.arima(ts.db)
summary(auto.mod)
f6 <- forecast(ts.db, h=12, model = auto.mod)
ac6 <- accuracy(f6)

#========================================================================================================================================
# GENERANDO TABLA CON CRITERIOS DE BONDAD DE AJUSTE
#========================================================================================================================================
mod.names <- c(n0, n1, n2, n3, n4, n5, n6)
e.matrix <- round(rbind(ac0, ac1, ac2, ac3, ac4, ac5, ac6), 2) [,-7]
mod.aics <- round(c(mod0$aic, mod1$aic, mod2$aic, mod3$aic, mod4$aic, mod5$aic, auto.mod$aic), 2)
mod.aiccs <- round(c(mod0$aicc, mod1$aicc, mod2$aicc, mod3$aicc, mod4$aicc, mod5$aicc, auto.mod$aicc), 2)
mod.bics <- round(c(BIC(mod0), BIC(mod1), BIC(mod2), BIC(mod3), BIC(mod4), BIC(mod5), BIC(auto.mod)), 2)
e.df <- cbind(mod.names, as.data.frame(cbind(mod.aics, mod.aiccs, mod.bics, e.matrix)))
colnames(e.df) <- c("Modelo", "AIC", "AICc", "BIC", colnames(e.matrix))
write.csv(e.df, file = "Criterios de Informacion - 18 a 64.csv")
sel.mod <- mod5
#========================================================================================================================================
# NORMALIDAD DE LOS RESIDUOS
#========================================================================================================================================
res <- residuals(sel.mod)
par(mfrow = c(1,1), mai = c(1,1,0.1,0.1))
hist(res, col = "red", main = "", xlab = "Residuos", ylab = "Frecuencia")
jb.test <- jarque.bera.test(residuals(sel.mod))
jb.test
jb.st <- round(jb.test$statistic, 2)
jb.df <- jb.test$parameter
jb.p <- round(jb.test$p.value, 2)
checkresiduals(sel.mod)
#========================================================================================================================================
# AUTOCORRELACION DE LOS RESIDUOS
#========================================================================================================================================
LjungBoxTest(residuals(sel.mod), k=24, lag.max=12)
b.test <- Box.test(res, lag = 6, type = c("Ljung-Box"))
b.test
b.st <- round(b.test$statistic, 2)
b.df <- b.test$parameter
b.p <- round(b.test$p.value, 2)
#========================================================================================================================================
# HOMOCEDASTICIDAD DE LOS RESIDUOS
#========================================================================================================================================
par(mfrow = c(1,1), mai = c(1,1,0.1,0.1))
McLeod.Li.test(sel.mod, gof.lag = 24)
#========================================================================================================================================
# ESTIMACION CON REDES NEURONALES
#========================================================================================================================================
par(mfrow = c(1,1), mai = c(1,0.5,0.1,0.1))
set.seed(2401)
fit <- nnetar(ts.db)
fnet <- forecast(fit,h=12, PI = TRUE)
#========================================================================================================================================
# COMPARACION ARIMA Y REDES
#========================================================================================================================================
# ARIMA SELECCIONADO
fut.reg <- c(rep(max(fitted.ts), 12))
fut.reg <- ts(fut.reg, start = c(2007,01), frequency = 12)
test.forecast <- forecast(sel.mod, h=12, xreg = fut.reg)
par(mfrow = c(1,1), mai = c(1,1,0.1,0.1))
plot(test.forecast, xlab = "Tiempo", main = "", ylab = "Total de Intoxicaciones", ylim = c(0,60))
lines(ts.test, col = "red", lwd = 2)
lines(sel.mod$fitted, col = "green", lty = 2, lwd = 2)
lines(test.forecast$lower[,2], col = "darkgreen", lwd = 2)
lines(test.forecast$upper[,2], col = "darkgreen", lwd = 2)
legend("topleft", c("Serie de Entrenamiento", "Estimados ARIMA sobre Entrenamiento", "Proyección ARIMA", "Intervalos de Confianza del 95%", "Valores Observados"), col = c("black", "green", "blue", "darkgreen", "red"), lty = c(1,2,1,1,1), cex = 0.8, bty = "n")

ar.ac <- accuracy(test.forecast, ts.test)


# REDES
plot(fnet, main = "", xlab = "Tiempo", ylab = "Total de Intoxicaciones", ylim = c(0,60))
lines(ts.test, col = "red", lwd = 2)
lines(fnet$fitted, col = "green", lty = 2, lwd = 2)
lines(fnet$lower[,2], col = "darkgreen", lwd = 2)
lines(fnet$upper[,2], col = "darkgreen", lwd = 2)
legend("topleft", c("Serie de Entrenamiento", "Estimados Redes sobre Entrenamiento", "Proyección Redes", "Intervalos de Confianza del 95%", "Valores Observados"), col = c("black", "green", "blue", "gray", "red"), lty = c(1,2,1,1,1), cex = 0.8, bty = "n")


rn.ac <- accuracy(fnet, ts.test)
summary(fnet)

# Tabla accuracies
final.ac <- rbind(ac.HWA[2,], ac.HWM[2,], ar.ac[2,], rn.ac[2,])
ac.names <- c("Holt Winters Aditivo", "Holt Winters Multiplicativo","ARIMA (0,0,1)(1,1,1) CON INTERVENCION", "REDES")
final.ac <- cbind(ac.names, as.data.frame(final.ac))
final.ac
write.csv(final.ac, file = "Accuracy Redes vs ARIMA.csv")
#========================================================================================================================================
# ESTIMACION DE PRONOSTICOS
#========================================================================================================================================
fut.reg <- c(rep(max(fitted.ts), 24))
fitted.ts <- c(fitted.ts, fut.reg)
fitted.ts <- ts(fitted.ts, start = c(2007,1), frequency = 12)
mod.fin1 <- Arima(or.db, order = c(0,1,1), seasonal = list(order = c(1,1,1), period = 6), xreg = fitted.ts, include.mean = TRUE)
summary(mod.fin1)
fin.f1 <- forecast(mod.fin1, h=24, xreg = fut.reg)
plot(fin.f1, xlab = "Tiempo", main ="", ylab = "Total de Intoxcaciones")
pro1 <- fin.f1
pro1 <- as.data.frame(pro1)
pro1 <- cbind(row.names(pro1), pro1)
pro1 <- pro1[,c(1,2,5,6)]
colnames(pro1) <- c("Mes-Año", "Pronóstico", "Lim. Inf 95%", "Lim. Sup 95%")
row.names(pro1) <- NULL
pro1[, 2] <- round(pro1[,2], 2)
pro1[, 3] <- round(pro1[,3], 2)
pro1[, 4] <- round(pro1[,4], 2)
write.csv(pro1, file = "Pronostico ARIMA 18 a 64 años.csv")


#========
set.seed(2401)
fit <- nnetar(or.db)
fnet <- forecast(fit,h=24, PI = TRUE)
plot(fnet, main = "", xlab = "Tiempo")
pro1 <- fnet
pro1 <- as.data.frame(pro1)
pro1 <- cbind(row.names(pro1), pro1)
pro1 <- pro1[,c(1,2,5,6)]
colnames(pro1) <- c("Mes-Año", "Pronóstico", "Lim. Inf 95%", "Lim. Sup 95%")
row.names(pro1) <- NULL
pro1[, 2] <- round(pro1[,2], 2)
pro1[, 3] <- round(pro1[,3], 2)
pro1[, 4] <- round(pro1[,4], 2)
write.csv(pro1, file = "Pronostico REDES 18 a 64 años.csv")
