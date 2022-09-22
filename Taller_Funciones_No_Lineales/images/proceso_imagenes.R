# Generacion de una imagen

# Librerias
library(tidyverse)
library(magick)

# Datos:
dat <- expand.grid(
  x      = seq(from = 200, to = 2000, by = 50),   # irradianza, umol Fotones / (m2 * s)
  a      = c(0.02, 0.05, 0.07),      # mol CO2 / mol fotones
  Rd     = c(1.5, 2, 3),             # umol CO2 / m2*s
  y.asim = 25                        # umol CO2 / m2*s
) %>%
  mutate(
    y = a*x*y.asim / (y.asim + a*x) - Rd
  )

texto.y1 <- expression(
  "Fotosíntesis neta ("*mu*"mol C0"[2]~~m^-2~s^-1*")"      # esto es notacion matematica de R, ver ayuda en: ?plotmath
)
texto.y2 <- expression(
  atop("Fotosíntesis neta", "("*mu*"mol C0"[2]~~m^-2~s^-1*")" ) 
)

texto.x <- expression(
  "Irradianza ("*mu*"mol Fotones"~~m^-2~s^-1*")" 
)

texto.color <- expression(
  "a (mol C0"[2]~~mol^-1~Fotones*")" 
)

# Se agrega columna Rd.math al data.frame 'dat' con etiquetas para Rd en escritura
# matematica. Esto para lograr un subindice en la etiqueta de Rd en el grafico
dat <- dat %>%
  mutate(
    Rd.math = recode(
      Rd,
      "1.5" = "R[d] == 1.5",   # esto es notacion matematica de R, ver ayuda en: ?plotmath
      "2"   = "R[d] == 2.0",
      "3"   = "R[d] == 3.0",
    )
  )

# Grafico con todo lo anterior
# Note el uso de la nueva columna Rd.math en las facetas
# y el uso del valor "label_parsed" para el argumento "labeller" en
# facet_grid
ggplot(dat, aes(x = x, y = y, color = as.character(a) )) + 
  geom_line() +
  geom_hline(yintercept = c(y.asim/2, y.asim), size = 0.3, linetype = "dashed") +
  facet_grid(~ Rd.math, labeller = label_parsed) + xlim(0,2000) +
  labs(x = texto.x, y = texto.y2, color = texto.color) +
  theme(legend.position = "top", 
        legend.background = element_rect(fill = "#CBF7C3"),
        plot.background = element_rect(fill = "#CBF7C3"))

# Salvando el grafico
ggsave(filename = "fotoIrrad.png", width = 5.4, height = 2.8)

# leyendo el grafico:
yy   <- image_read("fotoIrrad.png")
yy   <- image_read("fotoYo.jpeg")
image_info(yy)
source("img_circle_crop.R")
img_circle_crop(img = yy, file = "fotoIrrad_crop.png", f = 0.3)

image_crop(img, geometry = "840x840+1400+0")

img_circle_crop(img = yy, file = "fotoYo_crop.png", f = 0.15)









