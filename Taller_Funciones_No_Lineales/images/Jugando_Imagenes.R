

library(magick)
library(tidyverse)
source("img_circle_crop.R")

f1 <- image_read("fotoIrrad.png") %>%
  img_circle_crop(file = NULL, f = 0.3, col = "none") %>%
  image_scale("40%")

gatito <- image_read("gato-baile.gif")
glmJoseBar <- image_read("glm_joseBar.jpeg") %>%
  img_circle_crop(file = NULL, f = 0.2, col = "none") %>%
  image_scale("95%")

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

# Imagen final
#fig1 <- image_mosaic(c(fig, f1))
#fig1

fig1 <- image_mosaic(c(fig, glmJoseBar))
fig1
fig1 <- image_mosaic(c(fig, datosNoLineal))

# Se exporta la imagen
image_write(fig1, path = "inv2.png")



# Foreground image
banana <- image_read("https://jeroen.github.io/images/banana.gif")
banana <- image_scale(banana, "150")
dogcat <- image_read("https://media.giphy.com/media/b6mOpV0p0GgQ37T9m0/giphy.gif")
dogcat <- image_scale(dogcat, "130")
dogs <- image_read("https://media.giphy.com/media/oSHrQ3y3WU8Te/giphy.gif") %>%
  image_scale("230")
image_info(banana)
dogFinger <- image_read("perro_dedo.gif") %>%
  image_scale("150")
  

fig2 <- image_composite(fig1, banana) %>%
  image_animate(fps = 10, optimize = T)
fig2
image_write(fig2, path = "inv1.gif")

fig3 <- image_composite(fig1, dogcat, offset = "+750+150") %>%
  image_animate(fps = 10, optimize = T)
fig3
image_write(fig3, path = "inv2.gif")

fig4 <- image_composite(fig1, dogs, offset = "+660+350") %>%
  image_animate(fps = 10, optimize = T)
image_write(fig4, path = "inv4.gif")


fig2 <- image_composite(fig1, dogFinger, offset = "+750+150") %>%
  image_animate(fps = 10, optimize = T)
image_write(fig2, path = "fig2.gif")

image_write(gifscat, path = "gifscat.gif")



