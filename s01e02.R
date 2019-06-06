# Temas a tratar:
# 1. Paquete pacman
# 2. Paquete RColorBrewer
# 3. Cómo graficar más de una serie en diferentes data frames con:
#   a) r basic graph
#   b) rbokeh
#   c) plotly
#   d) ggplot
# 4. Faceting
# 5. Interacción entre ggplot y plotly


# Carga de los paquetes a usar en esta sesión
if(!require(pacman)){
  install.packages(pacman, dependencies = TRUE)
  library(pacman)
}

#paquetes <- c("pacman", "reshape2", "rbokeh", "ggplot2", "plotly", "RColorBrewer")
pacman::p_load(char = c("pacman", "reshape2", "rbokeh", 
                        "ggplot2", "plotly", "RColorBrewer", "bbplot")) 
# Hablemos un poco de RColorBrewer:
# El paquete contiene tres tipos de paletas de colores:
# 1. secuencial: Excelente para gradientes: Blues, BuGn, BuPu, GnBu, Greens, Greys, 
# Oranges, OrRd, PuBu, PuBuGn, PuRd, Purples, RdPu, Reds, YlGn, YlGnBu YlOrBr, YlOrRd.
# 2. cualitativas: Excelente para representar datos categóricos: Accent, Dark2, Paired,
# Pastel1, Pastel2, Set1, Set2, Set3
# 3. divergentes: Excelente para poner énfasis tanto en los valores extremos como los de
# enmedio: BrBG, PiYG, PRGn, PuOr, RdBu, RdGy, RdYlBu, RdYlGn, Spectral

# Tres funciones importantes
# brewer.pal(n, name)
# display.brewer.pal(n, name)
# display.brewer.all(n = NULL, type = "all", select = NULL, colorblindFriendly = FALSE)
display.brewer.all()
display.brewer.pal(32, "PRGn")
colors <- brewer.pal(3, "Spectral")

# Let's start!
getwd()
setwd("/Users/natorro/Dropbox/R-Bits/visceral-datanights/S01E01/data/")


# Filtramos de esta manera, pero hay muchas más:
delitos <- read.csv("delitos.csv")

delitos_filtrado <- subset(delitos,
                           Subtipo.de.delito == "Homicidio doloso" |
                             Subtipo.de.delito == "Feminicidio")

delitos_melt <- melt(delitos_filtrado, c("Año", "Entidad"), c("Enero", "Febrero", "Marzo", 
                                                   "Abril", "Mayo", "Junio", "Julio", 
                                                   "Agosto", "Septiembre", "Octubre", 
                                                   "Noviembre", "Diciembre"))

victimas <- read.csv("victimas.csv")

victimas_filtrado <- subset(victimas, 
                            Subtipo.de.delito == "Homicidio doloso" | 
                              Subtipo.de.delito == "Feminicidio")

victimas_melt <- melt(victimas_filtrado, 
                      c("Año", "Entidad"), 
                      c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
                        "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre",
                        "Diciembre"))


head(delitos_melt)
head(victimas_melt)

names(victimas_melt)[3] <-  "Mes"
names(delitos_melt)[3] <-  "Mes"

names(victimas_melt)[4] <-  "Valor"
names(delitos_melt)[4] <-  "Valor"

# Eliminamos los NA

na_index <- is.na(delitos_melt$Valor)
delitos_melt <- delitos_melt[!na_index, ]

na_index <- is.na(victimas_melt$Valor)
victimas_melt <- victimas_melt[!na_index,]

# Agregamos la suma por mes y año año con año
delitos_agregado <- aggregate(delitos_melt$Valor, 
                      by = list(mes = delitos_melt$Mes, 
                                año = delitos_melt$Año), 
                      sum)

victimas_agregado <- aggregate(victimas_melt$Valor, 
                              by = list(mes = victimas_melt$Mes, 
                                        año = victimas_melt$Año), 
                              sum)
head(delitos_agregado)
head(victimas_agregado)

Sys.setlocale(locale="es_ES.UTF-8")
delitos_agregado$yearmon <- zoo::as.yearmon(paste(delitos_agregado$mes, 
                                                delitos_agregado$año))

victimas_agregado$yearmon <- zoo::as.yearmon(paste(victimas_agregado$mes, 
                                                victimas_agregado$año))

# Classic basic plot
# probar con lty = 5 y 6
plot(victimas_agregado$yearmon, victimas_agregado$x, type = "l", col = "green",
     xlab = "Año/Mes", ylab = "#")
lines(delitos_agregado$yearmon, delitos_agregado$x, type = "l", col = "red")

# rbokeh
plotting_data <- data.frame(mes = victimas_agregado$yearmon, 
                            homicidios = victimas_agregado$x)

plotting_data2 <- data.frame(mes = delitos_agregado$yearmon, 
                            homicidios = delitos_agregado$x)

figure() %>% 
  ly_points(mes, homicidios, hover = c(mes, homicidios), 
            data = plotting_data, color= colors[1]) %>% 
  ly_lines(mes, homicidios, data = plotting_data, color = colors[1]) %>% 
  ly_points(mes, homicidios, hover = c(mes, homicidios), 
            data = plotting_data2, color = colors[3]) %>% 
  ly_lines(mes, homicidios, data = plotting_data2, color = colors[3]) 

# Plotly
plotting_data$carpetas <- plotting_data2$homicidios

plot_ly() %>% 
  add_trace(plotting_data, x = plotting_data$mes, y = plotting_data$homicidios, 
            mode = 'lines+markers', 
            text = paste(plotting_data$mes, plotting_data$homicidios, sep=": "),
            hoverinfo="text", name = "Víctimas") %>% 
  add_trace(plotting_data, x = plotting_data$mes, y = plotting_data$carpetas, 
            mode = 'lines+markers', 
            text = paste(plotting_data$mes, plotting_data$homicidios, sep=": "),
            hoverinfo="text", name = "Carpetas") 
  

#### ggplot2
fecha <- as.Date(zoo::as.Date.yearmon(plotting_data$mes), frac = 1)
hdata <- cbind(plotting_data, fecha)
hdata2 <- cbind(plotting_data2, fecha)

ggplot() + 
  geom_line(data = hdata, aes(fecha, homicidios), color = "red") + 
  geom_line(data = hdata2, aes(fecha, homicidios), color = "blue") + 
  xlab("Año") + 
  ylab("#")

# + bbplot
ggplot() + 
  geom_line(data = hdata, aes(fecha, homicidios), color = "red") + 
  geom_line(data = hdata2, aes(fecha, homicidios), color = "blue") + 
  xlab("Año") + 
  ylab("#") + 
  bbc_style()

# facets in ggplot
victimas_facet <- melt(victimas_filtrado, c("Año", "Sexo"), 
                       c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", 
                         "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"))
names(victimas_facet)[3] <-  "Mes"
names(victimas_facet)[4] <-  "Valor"
head(victimas_facet)

victimas_facet_agregado <- aggregate(victimas_facet$Valor,
                                     by = list(mes = victimas_facet$Mes, 
                                               año = victimas_facet$Año,
                                               sexo = victimas_facet$Sexo), 
                                     sum)
head(victimas_facet)
victimas_facet_agregado$yearmon <- zoo::as.yearmon(paste(victimas_facet_agregado$mes, 
                                                  victimas_facet_agregado$año))

fecha <- as.Date(zoo::as.Date.yearmon(victimas_facet_agregado$yearmon), frac = 1)
hdata <- cbind(victimas_facet_agregado, fecha)
head(hdata)

my_graph <- ggplot(hdata, aes(x = fecha, y = x) ) + 
  geom_point() +
  geom_line() +
  facet_grid(. ~ sexo)

ggplotly(my_graph)
