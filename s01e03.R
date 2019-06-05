# # Temas a tratar:
# 1. Lectura de datos desde un archivo Excel
# 
# 
# 
# 
# 
setwd("/Users/natorro/Desktop/r-bits")
pacman::p_load(char = c("readxl"))
my_data <- read.table(pipe("pbpaste"), sep = "\t", header = TRUE)

# En Windows:
#my_data <- read.table(file = "clipboard", 
#                      sep = "\t", header=TRUE)


my_data2 <- read_excel("composicion_y_estructura_de_la_poblacion.xlsx", 
                       sheet = "Cuadro 1.2")
my_data2 <- 
estados <- my_data2[15:46, 1]
poblaciontotal <- my_data2[15:46, 4]
pobhombres <- my_data2[15:46, 6]
pobmujeres <- my_data2[15:46, 7]
library(tibble)
library(dplyr)
population_data <- tibble(estados = estados, poblaciontotal = poblaciontotal,
                              pobhombres = pobhombres, pobmujeres =  pobmujeres)
rename_all(population_data,c("estados", "poblaciontotal", "pobhombres", "pobmujeres"))
View(population_data)
names(population_data) <- 

