# Carga de los paquetes a usar en esta sesión
if(!require(reshape2)){
  install.packages(reshape2, dependencies = TRUE)
  library(reshape2)
}

if(!require(rbokeh)){
  install.packages(rbokeh, dependencies = TRUE)
  library(rbokeh)
}

if(!require(ggplot2)){
  install.packages(ggplot2, dependencies = TRUE)
  library(ggplot2)
}

if(!require(plotly)){
  install.packages(plotly, dependencies = TRUE)
  library(plotly)
}

# Iniciamos la carga de el archivo .csv
getwd()
setwd("/Users/natorro/Dropbox/R-Bits/visceral-datanights/S01E01/data/")
delitos <- read.csv("delitos.csv")
head(delitos)
View(delitos)

# Vemos qué tipo de delitos hay:
unique(delitos$Subtipo.de.delito)

# Filtramos por los subtipos de delito que nos interesan
homicidios <- subset(delitos, 
                     Subtipo.de.delito == "Homicidio doloso" | 
                       Subtipo.de.delito == "Feminicidio")

unique(homicidios$Subtipo.de.delito)
View(homicidios)

# pivotear
delitos_melt <- melt(homicidios, c("Año"), c("Enero", "Febrero", "Marzo", 
                                             "Abril", "Mayo", "Junio", "Julio", 
                                             "Agosto", "Septiembre", "Octubre", 
                                             "Noviembre", "Diciembre"))

head(delitos_melt)
tail(delitos_melt)
head(is.na(delitos_melt$value))
tail(is.na(delitos_melt$value))

# Eliminamos los que tienen NA's
na_index <- is.na(delitos_melt$value)
delitos_melt <- delitos_melt[!na_index, ]

head(delitos_melt)
tail(delitos_melt)

head(delitos_melt)

# Agregamos nuestros datos por años y meses: 
agregado <- aggregate(delitos_melt$value, 
                      by = list(mes = delitos_melt$variable, 
                                año = delitos_melt$Año), 
                      sum)

head(agregado)
# Creamos una columna yearmon

Sys.setlocale(locale="es_ES.UTF-8")
agregado$yearmon <- zoo::as.yearmon(paste(agregado$mes, 
                                       agregado$año))
head(agregado)
agregado$homicidios <- agregado$x
agregado$x <- NULL

plotting_data <- agregado

# Graficación
head(plotting_data)
plot(plotting_data$yearmon, plotting_data$homicidios)

plot(plotting_data$yearmon, plotting_data$homicidios, type = "l",
     xlab = "año", ylab = "homicidios")

# rbokeh
plotting_data <- data.frame(mes = agregado$yearmon, 
                            homicidios = agregado$homicidios)
figure(data = plotting_data) %>% 
  ly_points(mes, homicidios, hover = c(mes, homicidios))



### Plotly
plot_ly(plotting_data, x = plotting_data$mes, y = plotting_data$homicidios, 
        mode = 'lines', 
        text = paste(plotting_data$mes, plotting_data$homicidios, sep=": "),
        hoverinfo="text")


#### ggplot2
fecha <- as.Date(zoo::as.Date.yearmon(plotting_data$mes), frac = 1)
hdata <- cbind(plotting_data, fecha)

ggplot(hdata, aes(fecha, homicidios)) + 
  geom_line() + xlab("año") + 
  ylab("homicidios")

devtools::install_github('bbc/bbplot')

library(bbplot)

ggplot(hdata, aes(fecha, homicidios)) + 
  geom_line() + xlab("año") + 
  ylab("homicidios") +
  bbc_style()

### Datanights S01E02









