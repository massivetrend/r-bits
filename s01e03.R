# # Temas a tratar:
# 1. Lectura de datos desde un archivo Excel
# 2. Uso de %in%
# 3. Cambio de etiquetas en eje X verticalmente
# 4. Graficación con ggplot2 con etiquetas de eje X verticalmente

setwd("/Users/natorro/Desktop/r-bits/data")
pacman::p_load(char = c("readxl", "dplyr", "ggplot2"))
my_data <- read.table(pipe("pbpaste"), sep = "\t", header = TRUE)
head(my_data)
# En Windows:
#my_data <- read.table(file = "clipboard", 
#                      sep = "\t", header=TRUE)

filesheets <- excel_sheets("composicion_y_estructura_de_la_poblacion.xlsx")
filesheets
my_data2 <- read_excel("composicion_y_estructura_de_la_poblacion.xlsx", 
                       col_names = c("estados", "v2", "v3", "poblaciontotal", 
                                     "v5", "pobhombres", "pobmujeres"), 
                       range = paste(filesheets[3],"!", "A16:G47", sep = ""))
population_data <- select(my_data2, estados, poblaciontotal, pobhombres, pobmujeres)
head(population_data)
head(my_data)
# Let's read the data:
victimas <- read.csv("victimas.csv")
head(victimas)
victimas_filtrado <- subset(victimas, 
                            Subtipo.de.delito == "Homicidio doloso" | 
                              Subtipo.de.delito == "Feminicidio")

head(victimas_filtrado)
tail(victimas_filtrado)
victimas_melt <- melt(victimas_filtrado, 
                      c("Año", "Entidad"), 
                      c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
                        "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre",
                        "Diciembre"))

head(victimas_melt)
tail(victimas_melt)
na_index <- is.na(victimas_melt$value)
victimas_melt <- victimas_melt[!na_index,]
head(victimas_melt)
tail(victimas_melt)
victimas_agregado <- aggregate(victimas_melt$value, 
                               by = list(entidad = victimas_melt$Entidad), 
                               sum)

estados <- population_data$estados
delagregado <- victimas_agregado$entidad
estados
delagregado
estados %in% delagregado
estados == delagregado

names(victimas_agregado)[1] <- "estados"
names(victimas_agregado)
names(population_data)
dataration <- inner_join(victimas_agregado, population_data)
head(dataration)

dataration$porcentaje <- (dataration$x / dataration$poblaciontotal ) * 100000
head(dataration)
class(dataration$estados)
dataration$estados <- as.factor(dataration$estados)
plot(dataration$estados, dataration$porcentaje)
plot(dataration$estados, dataration$porcentaje, mar=c(5, 4, 0, 2) + 0.1, xaxt="n")
axis(1, at=1:32, labels=dataration$estados, las = 2, cex.axis = 0.8, main = "", xlab ="")

# ggplot
ggplot(dataration, aes(x = estados, y = porcentaje)) +
  theme_bw() + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1)) 
  
ggplot(dataration, aes(x = estados, y = porcentaje)) +
  theme_bw() + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) 

ggplot(dataration, aes(x = estados, y = porcentaje)) +
  theme_bw() + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1)) +
  bbc_style()

ggplot(dataration, aes(x = estados, y = porcentaje)) +
  theme_bw() + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) +
  bbc_style()
