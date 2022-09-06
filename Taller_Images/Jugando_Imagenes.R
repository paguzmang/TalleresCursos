

library(magick)
library(tidyverse)
source("img_circle_crop.R")

f1 <- image_read("fotoIrrad.png") %>%
  img_circle_crop(file = NULL, f = 0.3, col = "none") %>%
  image_scale("40%")

# Texto para el afiche:
titulo <- "Gráficos 2D de funciones no lineales\nen varias variables"
fecha <- "6-sep; 1:30 a 3:00 PM"
lugar <- "Aula B503"
tema <- "En esta sesión aprenderemos:
- Ejemplos de aplicaciones de funciones no lineales en Ciencias biológicas
- Como crear automáticamente una tabla (data.frame) de valores desde la función
- Gráficar el data.frame con ggplot2 usando facetas y mapeo a color o forma."
texto <- paste("Nueva sesión del taller:", titulo, fecha, lugar, sep = "\n")

# Composicion de la imagen
# fig <- image_blank(width = 336*2.7, height = 336*1.5, color = "#FFCC9E")
fig <- image_blank(width = 336*2.7, height = 336*1.5, color = "none")
fig <- fig %>%
  image_annotate(text = "Nueva sesión del taller:", 
                 size = 24, font = "Trebuchet", weight = 400,
                 color = "blue", location = "+340+80") %>%
  image_annotate(text = titulo, 
                 size = 28, font = "Trebuchet", weight = 700,
                 color = "blue3", location = "+350+110") %>%
  image_annotate(text = paste0("Fecha: ", fecha, "\nLugar: ", lugar), 
                 size = 24, font = "Trebuchet", weight = 400,
                 color = "blue3", location = "+350+250") %>%
  image_annotate(text = tema, 
                 size = 22, font = "Trebuchet", weight = 400,
                 color = "blue3", location = "+50+350") 

# Imagen final
fig1 <- image_mosaic(c(fig, f1))
fig1

# Se exporta la imagen
image_write(fig1, path = "inv1.png")

# Foreground image
banana <- image_read("https://jeroen.github.io/images/banana.gif")
banana <- image_scale(banana, "150")
dogcat <- image_read("https://media.giphy.com/media/b6mOpV0p0GgQ37T9m0/giphy.gif")
dogcat <- image_scale(dogcat, "130")
image_info(banana)

fig2 <- image_composite(fig1, banana) %>%
  image_animate(fps = 10, optimize = T)
fig2
image_write(fig2, path = "inv1.gif")

fig3 <- image_composite(fig1, dogcat, offset = "+750+150") %>%
  image_animate(fps = 10, optimize = T)
fig3
image_write(fig3, path = "inv2.gif")



gifscat <- image_read("https://www.animatedimages.org/data/media/209/animated-cat-image-0216.gif")
image_write(gifscat, path = "gifscat.gif")



