
# Falta ejercicio con tidyr
# Falta pasarlo a rmarkdown

# Instalación de paquetes
#install.packages("tidyverse")
#install.packages("scales")
#install.packages("hrbrthemes")


# Carga de Librerías que utilizaremos en nuestro análisis
library(tidyverse)
library(scales)
library(hrbrthemes)
# library(janitor)

# Objetivo Principal del paquete readR(): Importar datos
# Lectura del dataset de una url
legosets <- read_csv("https://raw.githubusercontent.com/seankross/lego/master/data-tidy/legosets.csv")
View(legosets)

# Objetivo Principal del paquete dplyr(): explorar y trasnformar datos
# Algunas funciones  básicas: head(), tail(), glimpse(), summary()

# head() y tail(): para obtener primeras y últimas filas
legosets %>% head()
legosets %>%  tail()

# glimpse(): para concocer la estructura y tipo de datos del dataset
legosets %>% glimpse()

# str(): sentencia de R base que tiene una función similar a glimpse()
legosets %>% str()

# colnames(): para conocer los nombres de columnas
legosets %>% colnames()

# summary(): resumen descriptivo de las variables del dataset
legosets %>% summary()

# slice(n1:n2): para selecccionar un determinado rango de filas
legosets %>%
  slice(20:30)

# Objetivo: quiero transformar los nombres de columnas a minúsculas
# y seleccionar solo las filas de interés

#Opción 1:
legosets %>% select_all(tolower) %>%
  select(-c(item_number,image_url, availability, ends_with("msrp")))

#Opción 2:
legosets %>% select_all(tolower) %>%
  select(name, year,contains("theme"), pieces, minifigures, usd_msrp)

# También podría utilizar clean_names de janitor para pasar a minúsculas los nombres de columnas
# legosets <- legosets %>% clean_names()


# si quisieramos reordenar las filas, colocando año al principio:
legosets %>%
  select(year, name, everything()) %>%
  head(5)


# REVISION
# legosets %>%
#   distinct(name) %>% arrange(name) %>% view()


#-----------------------------------
# pregunta 1: ¿Cuáles son los legos más caros (en doláres) por año? Teniendo en cuenta datos a partir del 2000
# voy resolviendo por pasos
#1-
legosets %>% 
  select(name, year,contains("theme"), pieces, minifigures, usd_msrp) %>% 
    filter(year >= 2000) %>% arrange(year)
#2- 
legosets %>% 
  select(name, year,contains("theme"), pieces, minifigures, usd_msrp) %>% 
  filter(year >= 2000) %>% arrange(desc(year, usd_msrp)) %>% view()
#3-
legosets %>% 
  select(name, year,contains("theme"), pieces, minifigures, usd_msrp) %>% 
  filter(year >= 2000 & !is.na(usd_msrp)) %>% group_by(year, theme) %>% 
  summarise(mas_caro = max(usd_msrp)) %>% top_n(5, mas_caro) %>%  arrange(desc(mas_caro)) %>%
  view()

#-------------------------------------------------
# pregunta 2: ¿Cuál es el precio promedio por theme?
legosets %>%  filter(year >= 2010 & !is.na(usd_msrp)) %>% 
group_by(theme) %>%
  summarise(Price_promedio = mean(usd_msrp))


#--------------------------------------------------
# stringR
# Pregunta 3: cuá
subtheme_S_w <- legosets %>%
  filter(theme == "Star Wars") %>%
  count(subtheme, sort = TRUE) 


s_wars<- legosets %>%
  filter(theme == "Star Wars" & !is.na(subtheme))
library(tidyr)



# Algunos gráficos con legos
# ggplot2
ggplot(data= s_wars, aes(x= subtheme, fill= subtheme)) +
  geom_bar() +
  coord_flip() +
  scale_y_reordered() +
  scale_color_viridis_d(option = "A") +
  theme_ipsum_pub()

 ggplot(data= s_wars, aes(x= subtheme, y=pieces, fill= subtheme)) +
  geom_col() +
   coord_flip() +
   scale_color_viridis_d(option = "A") +
   theme_ipsum_pub()

 
 #--------------------------------------------------------------
 # PREGUNTA 4: ¿cómo varía el precio a lo largo de los años? Podría unirla a la 1
 # calculamos precio promedio
 # calculamos la media de precios
 
 library(dplyr)
 # precio promedio por año 
 avg_price_per_year <- legosets %>%
   filter(!is.na(usd_msrp)) %>%
   group_by(year) %>%
   summarise(Price = mean(usd_msrp)) %>% view()
 avg_price_per_year
 
 med_price_per_year <- legosets %>%
   filter(!is.na(usd_msrp)) %>%
   group_by(year) %>%
   do(data.frame(Price = median(.$usd_msrp)))
 med_price_per_year
 
 # Gráfico de evolución: pasarlo a tidyverse
 plot(avg_price_per_year, type = "l", col = "blue", 
      main = "Lego set prices over time", ylim = c(0, max(avg_price_per_year$Price)))
 points(med_price_per_year, type = "l", col = "red")
 legend("topleft", inset=c(0.2,0), legend=c("Average","Median"), lty=c(1,1), col=c("blue", "red"))
 
 #--------------------------------------------------------
 # # revisar
# legosets %>% 
#   select(name, year,contains("theme"), pieces, minifigures, usd_msrp) %>% 
#   filter(year == 2015) %>% filter(str_detect(theme,"S|star\sW|wars(\w+)")) %>% group_by(name)  %>%
#   view()
# S|s(\w+) wal(\w+)

 
 # GRAFICOS
# --------------------------------- 
# Gráfico con geom_point
p3 <- ggplot(data= legosets %>% filter(year >= 2010)) + 
  geom_point(aes(pieces, usd_msrp, size = minifigures, color = year), alpha = 0.7) +
  scale_color_viridis_c(option = "A") + 
  scale_y_sqrt(name = "USD", labels = dollar, limits = c(0, 300)) +
  scale_x_sqrt(name = "Piezas", labels = comma, limits = c(0, 2000)) +
  labs(title = "Precios según piezas", color= "año", size= "minifiguras") +
  theme_ipsum_pub()
p3


# para obtener la década utilizamos mutate
lg2 <- legosets %>% 
  mutate(year2 = floor(year/10)*10) %>% 
  select(year2, year, everything())
lg2

# Gráfico  con geom_gitter
p4 <- ggplot(lg2) +
  geom_jitter(aes(factor(year2), usd_msrp, size = pieces, color = year), alpha = 0.5, width = 0.25) +
  scale_color_viridis_c(option = "A") + 
  scale_y_continuous(name = "Precio USD", labels = dollar, limits = c(0, 300)) + 
  scale_x_discrete(name = "Década") +
  labs(title = "Precios/Década") +
  theme_ipsum_pub()
p4


# Gráfico con geom_point
lg3 <- legosets %>% 
  mutate(Themesw = theme == "Marvel Super Heroes")  #Marvel Super Heroes
p8 <- ggplot(lg3) + 
  geom_point(aes(pieces, usd_msrp, size = minifigures, color = Themesw), alpha = 0.7) +
  scale_color_manual(guide = "none", values = c("gray90", "darkred")) + 
  scale_y_sqrt(name = "Precio USD", labels = dollar, limits = c(0, 300)) +
  scale_x_sqrt(name = "Piezas", labels = comma, limits = c(0, 2000)) +
  labs(title = "Precios según piezas") +
  theme_ipsum_pub()

p8

# Gráfico barras
legosets %>% 
  count(theme, sort = TRUE) %>% 
ggplot() +
  geom_bar(aes(theme))

# FORCATS
# factores 
legos <- mutate(legosets, Theme2 = fct_lump(theme, n = 7))
ggplot(legos) + geom_bar(aes(Theme2)) +
  theme_ipsum_pub()


#----------------------------------
# Objetivo Principal del paquete tidyr(): Obtener datos ordenados

# -------------------------------- 
# modelo: regresión lineal
# precio en función de cantida de piezas

# Rmarkdown
# salida de un breve análisis
