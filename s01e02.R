# Carga de los paquetes a usar en esta sesión
if(!require(pacman)){
  install.packages(pacman, dependencies = TRUE)
  library(pacman)
}

paquetes <- c("pacman", "reshape2", "rbokeh", "ggplot2", "plotly")
pacman::p_load(char = c("pacman", "reshape2", "rbokeh", 
                        "ggplot2", "plotly")) 
getwd()
setwd("/Users/natorro/Dropbox/R-Bits/visceral-datanights/S01E01/data/")


# Filtramos de esta manera, pero hay muchas más:
delitos <- read.csv("delitos.csv")

delitos_filtrado <- subset(delitos,
                           Subtipo.de.delito == "Homicidio doloso" |
                             Subtipo.de.delito == "Feminicidio")

delitos_melt <- melt(delitos_filtrado, c("Año"), c("Enero", "Febrero", "Marzo", 
                                                   "Abril", "Mayo", "Junio", "Julio", 
                                                   "Agosto", "Septiembre", "Octubre", 
                                                   "Noviembre", "Diciembre"))

victimas <- read.csv("victimas.csv")

victimas_filtrado <- subset(victimas, 
                            Subtipo.de.delito == "Homicidio doloso" | 
                              Subtipo.de.delito == "Feminicidio")

victimas_melt <- melt(victimas_filtrado, 
                      c("Año"), 
                      c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
                        "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre",
                        "Diciembre"))


head(delitos_melt)
head(victimas_melt)

names(victimas_melt)[2] <-  "Mes"
names(delitos_melt)[2] <-  "Mes"

names(victimas_melt)[3] <-  "Valor"
names(delitos_melt)[3] <-  "Valor"
