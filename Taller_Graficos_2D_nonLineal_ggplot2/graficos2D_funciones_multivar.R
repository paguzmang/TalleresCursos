# Taller R y Rmarkdown
# Fecha: ago-2022
# Universidad CES - Biologia
# pguzmang
# Nota: archivo sin tildes en comentarios
# Encoding: UTF-8

# Tema: Codigo R para graficar ecuaciones no lineales con varias variables o parametros
# usando ggplot2 y el comando expand.grid

# Librerias
library(tidyverse)

# Ejemplo -----
# Fotosintesis neta vs. irrandianza, de acuerdo a Figura 2 de 
# Archontoulis, S. V., & Miguez, F. E. (2015), https://doi.org/10.2134/agronj2012.0506
# Fotosistensis neta: en umol CO2 / (m2 * s)
# Irradianza: en umol Fotones / (m2 * s)
# Se graficara la funcion de Hiperbola rectangular en la Tabla 1 de 
# Archontoulis, S. V., & Miguez, F. E. (2015)

# Estrategia de cinco pasos ----
# Paso (1): secuencia para predictoras (irradianza) ----
x <- seq(from = 200, to = 2000, by = 50)   # umol Fotones / (m2 * s) 
length(x)   # cuantos valores se generaron
head(x)     # revise los primeros seis
tail(x)     # revise los ultimos seis

# Paso (2): parametros ----
a      <- c(0.02, 0.05, 0.07)  # mol CO2 / mol fotones
Rd     <- c(1.5, 2, 3)         # umol CO2 / (m2 * s)
y.asim <- 25                   # umol CO2 / (m2 * s)

# Paso (3): tabla con combinaciones de valores ----
dat <- expand.grid(x = x, a = a, Rd = Rd, y.asim = y.asim)   # 37 * 3 * 3 * 1 = 333 filas

# revisemos la tabla generada
str(dat)     # estructura de la tabla (1800 filas, 4 columnas)
head(dat)    # 1eras. seis filas de la tabla

# Nota: 
# Ejecucion de pasos (1), (2) y (3) en un solo bloque de codigo R:
# dat <- expand.grid(
#   x      = seq(from = 200, to = 2000, by = 50),   # irradianza, umol Fotones / (m2 * s) 
#   a      = c(0.02, 0.05, 0.07),      # mol CO2 / mol fotones
#   Rd     = c(1.5, 2, 3),             # umol CO2 / m2*s
#   y.asim = 25                        # umol CO2 / m2*s
# )

# Paso (4): evaluacion de la funcion o calculo de y ----
dat <- dat %>%
  mutate(
    y = a*x*y.asim / (y.asim + a*x) - Rd
  )

# Exploremos la nueva tabla:
head(dat)            # 1eras. 6 filas de la tabla
sapply(dat, range)   # el min y max de cada variable

# Paso (5): Grafico ----
ggplot(dat, aes(x = x, y = y, color = as.character(a) )) + 
  geom_line() +
  geom_hline(yintercept = c(y.asim/2, y.asim), size = 0.3, linetype = "dashed") +
  facet_grid(~ Rd, labeller = label_both) + xlim(0,2000) +
  labs(x = "Irradianza", y = "Fotosintesis neta", color = "a")


# Anexo: notacion matematica en el grafico ----
# Definicion de etiquetas a los ejes del grafico con notacion matematica (de R)
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
  theme(legend.position = "top")


