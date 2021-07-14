
# Instalación de paquetes:
# Si es la primera vez que utilizas, necesitas instalar los paquetes a continuación:
install.packages("tidyverse")
install.packages("scales")
install.packages("hrbrthemes")


# Carga de Librerías que utilizaremos en nuestro análisis
library(tidyverse)
library(scales)
library(hrbrthemes)
# library(janitor)

# Objetivo Principal del paquete readR(): Importar datos
# Pueden leer el dataset de la url o descargarse el .csv, y luego leerlo:
#legosets <- read_csv("https://raw.githubusercontent.com/seankross/lego/master/data-tidy/legosets.csv")
legosets <- read_csv("legosets.csv")  
View(legosets)

# Objetivo Principal del paquete dplyr(): explorar y transformar datos
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

# Utilizo la sentencia select:
legosets_mini <- legosets %>% select_all(tolower) %>%
  select(name, year,contains("theme"), pieces, minifigures, usd_msrp)
legosets_mini
# También podría utilizar clean_names de janitor para pasar a minúsculas los nombres de columnas
# legosets <- legosets %>% clean_names()


# Si quisieramos reordenar las filas, colocando 'year(año)' al principio:
legosets_mini %>%
  select(year, name, everything()) %>%
  head(5)

#-----------------------------------
# pregunta 1: ¿Cuáles son las  5 categorías de legos más caras para el año 2015? 
# Paso 1-
legosets_mini %>% 
    filter(year == 2015) %>% arrange(theme) %>% view()

# chequeamos y seguimos con paso nro. 2:

# Paso 2- En la columna precio hay valores na (valores no disponibles), entonces los filtro,
# siguiente paso: agrupo y cálculo el mayor precio para cada categoría:

legos_mas_caro <- legosets_mini %>% 
  filter(year  == 2015 & !is.na(usd_msrp)) %>% group_by(theme) %>% 
  summarise(mas_caro = max(usd_msrp)) %>% top_n(5) %>% arrange(desc(mas_caro))
# chequeamos el resultado almacenado en 'legos_mas_caro'

legos_mas_caro %>% view()

#-------------------------------------------------
# pregunta 2: ¿Cuál es el precio promedio por theme o categoría? Teniendo en cuenta los datos a partir de 2010
# 

pregunta2 <- legosets_mini %>%  filter(year >= 2010 & !is.na(usd_msrp)) %>% 
          group_by(theme) %>%
          summarise(precio_prom = round(mean(usd_msrp),2))
pregunta2 %>% view()
#--------------------------------------------------

# Pregunta 3: ¿Cuáles son las subcategorías ofrecidas por LEGO de la temática de Star Wars y la variedad de legos ofrecido para cada una?
pregunta3 <- legosets_mini %>%
  filter (theme == "Star Wars") %>% 
  count(subtheme, sort = TRUE) %>% arrange(subtheme)
pregunta3 %>% view()

# veamos más en detalle la variedad que ofrece lego
legosets_mini %>%
  filter (theme == "Star Wars") %>% 
  count(subtheme, name) %>% arrange(subtheme) %>%  view()
#--------------------------------------------------
# GRAFICOS
#---------------------------------------------------------------------------------------
# ¿Cuáles son los sets disponibles referidos a la temática de Marvel?
lego_marvel <- legosets_mini %>% 
  filter((str_detect(theme,"Marvel")) & !is.na(subtheme)) %>% filter(!is.na(pieces)) %>% 
  select(theme, subtheme, name, everything())
lego_marvel %>% view()


# convierto a factor la variable 'subtheme'
lego_marvel <- lego_marvel %>% mutate(subtheme= as_factor(subtheme))
# verifico con str:
str(lego_marvel$subtheme)

#-----------------------------------
# Gráfico con sets de Marvel
#-----------------------------------
 ggplot(data= lego_marvel, aes(x= reorder(subtheme,pieces), y=pieces, fill= subtheme)) +
  geom_col() +
   coord_flip() +
   scale_color_viridis_d(option = "A") +
   labs(title = "Sets de Marvel y piezas", fill= "Sets", y= "cantidad de piezas", x= " ") +
   theme_ipsum_pub()


# ---------------------------------------------- 
# ¿Cómo varían los precios por década?
# Para obtener la década utilizamos mutate y generamos una nueva variable year2
precios_X_decada <- legosets_mini %>% 
  mutate(year2 = floor(year/10)*10) %>% 
  select(year2, year, everything())
precios_X_decada

#-----------------------------------
# Gráfico con geom_gitter
#-----------------------------------
grafico_decada <- ggplot(precios_X_decada) +
  geom_jitter(aes(factor(year2), usd_msrp, size = pieces, color = year), alpha = 0.5, width = 0.25) +
  scale_color_viridis_c(option = "A", direction = -1) + 
  scale_y_continuous(name = "Precio USD", labels = dollar, limits = c(0, 300)) + 
  scale_x_discrete(name = "Década") +
  labs(title = "Precios/Década", size= "nro. piezas", color= "año") +
  theme_ipsum_pub()
grafico_decada


