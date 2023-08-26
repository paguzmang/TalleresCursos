# Sep 2022
# Generacion de imagenes de invitacion a la sesion
# paguzmang

# Librerias y funcion para hacer el crop a la imagen
library(magick)
library(tidyverse)
source("https://raw.githubusercontent.com/paguzmang/funciones/master/img_circle_crop.R")

# Se importa figura, se hace crop y se re-escala:
datosNoLineal <- image_read("curvasDatos_NoLineal.png") %>%
  img_circle_crop(file = NULL, f = 0.7, col = "none") %>%
  image_scale("60%")

# Texto para el afiche:
titulo <- "Introducción a la regresión no lineal\ncon R"
fecha <- "20-sep; 1:30 a 3:00 PM"
lugar <- "Aula B503"
tema <- "En esta sesión aprenderemos: 
 Cómo ajustar datos a una función no lineal con R"
coord <- "Pablo Andrés Guzmán"
texto <- paste("Nueva sesión del taller:", titulo, fecha, lugar, sep = "\n")

# Composicion de la imagen
fig <- image_blank(width = 336*2.7, height = 336*1.5, color = "grey80")
fig <- fig %>%
  image_annotate(text = "Nueva sesión del taller:", 
                 size = 24, font = "Trebuchet", weight = 400,
                 color = "blue", location = "+340+80") %>%
  image_annotate(text = titulo, 
                 size = 28, font = "Trebuchet", weight = 700,
                 color = "blue3", location = "+350+110") %>%
  image_annotate(text = paste0("Coordinador: ", coord, 
                               "\nFecha: ", fecha, "\nLugar: ", lugar), 
                 size = 24, font = "Trebuchet", weight = 400,
                 color = "blue3", location = "+350+200") %>%
  image_annotate(text = tema, 
                 size = 22, font = "Trebuchet", weight = 400,
                 color = "blue3", location = "+50+350") 

# Se juntan las dos figuras:
fig1 <- image_mosaic(c(fig, datosNoLineal))

# Se importa imagen gif con perritos
dogs <- image_read("https://media.giphy.com/media/oSHrQ3y3WU8Te/giphy.gif") %>%
  image_scale("230")

# Se crea una 2da. figura con todo:
fig2 <- image_composite(fig1, dogs, offset = "+660+350") %>%
  image_animate(fps = 10, optimize = T)
image_write(fig2, path = "images/inv.gif")

