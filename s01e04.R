setwd("/Users/natorro/Desktop/r-bits")
pacman::p_load(char = c("readxl"))

# En Windows:
#my_data <- read.table(file = "clipboard", 
#                      sep = "\t", header=TRUE)

filesheets <- excel_sheets("composicion_y_estructura_de_la_poblacion.xlsx")
filesheets
population_data <- read_excel("composicion_y_estructura_de_la_poblacion.xlsx", 
                       col_names = c("estados", "v2", "v3", "poblaciontotal", 
                                     "v5", "pobhombres", "pobmujeres"), 
                       range = paste(filesheets[3],"!", "A16:G47", sep = ""))
population_data <- select(population_data, estados, poblaciontotal, pobhombres, pobmujeres)

# Let's read the data:
victimas <- read.csv("victimas.csv")

victimas_filtrado <- subset(victimas, 
                            Subtipo.de.delito == "Homicidio doloso" | 
                              Subtipo.de.delito == "Feminicidio")
head(victimas_filtrado)

victimas_melt <- melt(victimas_filtrado, 
                      c("Año", "Entidad"), 
                      c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
                        "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre",
                        "Diciembre"))

head(victimas_melt)

na_index <- is.na(victimas_melt$value)
victimas_melt <- victimas_melt[!na_index,]

head(victimas_melt)
victimas_agregado <- aggregate(victimas_melt$value, 
                               by = list(entidad = victimas_melt$Entidad, 
                                         mes = victimas_melt$variable, 
                                         año = victimas_melt$Año), 
                               sum)

head(victimas_agregado)

estados <- population_data$estados
delagregado <- victimas_agregado$entidad
estados %in% delagregado
estados == delagregado

library(dplyr)
names(victimas_agregado)[1] <- "estados"
dataration <- inner_join(victimas_agregado, population_data, by = c("estados"))
head(dataration)
dataration$porcentajes <- (dataration$x / dataration$poblaciontotal) * 100000

dataration <- select(dataration, estados, mes, año, porcentajes)
dataration <- arrange(dataration, estados)
View(dataration)

Sys.setlocale(locale="es_ES.UTF-8")
dataration$yearmon <- zoo::as.yearmon(paste(dataration$mes, 
                                          dataration$año))

head(dataration)
ggplot(dataration, aes(x = yearmon, y = estados)) + geom_tile(aes(fill = porcentajes), 
                                                          colour = "gray") +
  scale_fill_gradient(low = "white", high = "steelblue")
