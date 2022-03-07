# 5-mar-2022
# Simulacion de una respuesta categorica ordinal con
# cuatro categorias, asociada con el tratamiento y con
# interaccion entre edad y tratamiento.
# Se recrea un escenario similar al del data.frame Arthritis del
# paquete vcd

# Paquetes
library(tidyverse)

# Se utiliza la idea de variables latentes del comando
# draw_ordered del paquete fabricatr para simular
# la respuesta categoria ordinal sin tener que definir
# de forma explicita las probabilidades para un monton
# de combinaciones de categorias entre las variables

# Parametros para modelar la media de la variable
# latente de la respuesta en funcion del tratamiento y la edad
# mu.yl = beta0 + beta1*trat + beta2*edadl + beta3*edadl*trat
#   donde trat = {0,1} y edadl = {0, 1, 2}
# La edad se maneja por simplicidad asi, pero a partir de dichos
# valores se generan edades reales en tres categorias diferentes y consecutivas.
# El sexo tambien se incluye pero no tendra efecto de nada.
beta0 <- -1.5   
beta1 <- 1.5
beta2 <- 0
beta3 <- 0.5
n.trat <- 100        # cantidad de ind por combinacion de categorias
n <- n.trat*2*2*3    # cantidad total de ind. 2(trat) x 2(sexo) x 3(edades relativas)

# Simulacion de los datos balanceados y cruzados.
# Nota: si quiere simular desbalanceo utilice los comandos group_by y slice_sample (dplyr)
# para generar un subconjunto de menor tamano muestral. Considere esto a la hora
# de seleccionar el n.trat arriba.
dat <- expand_grid(
  ind   = 1:n.trat,
  trat  = c(0,1),      # 0 = placebo; 1 = tratado
  sex   = c(0,1),      # el sexo no tendra nada que ver
  edadl = c(0,1,2)     # edad latente, abajo se obtienen edades reales
) %>%
  mutate(
    edad  = round(rnorm(n = n, mean = 30 + 20*edadl, sd = 2.5)),
    mu.yl = beta0 + beta1*trat + beta2*edadl + beta3*edadl*trat,
    yl = rnorm(n = n, mean = mu.yl),
    y  = cut(x = yl, br = c(-Inf, -1,0,1,Inf), 
             labels = c("None", "Low", "Medium", "High"),
             ordered_result = T) 
  )

# Verificacion de la simulacion:
xtabs(~ trat + y + edadl, data = dat)
ggplot(dat, aes(x = factor(edadl), y = edad)) + geom_boxplot()
ggplot(dat, aes(x = factor(edadl), y = yl)) + geom_boxplot() + 
  facet_grid(~ trat, labeller = label_both)

# Grafico
dat %>%
  xtabs(~ trat + y + edadl + sex, data = .) %>%
  prop.table(margin = c(1,3,4)) %>%
  as.data.frame() %>%
  ggplot(aes(x = edadl, y = Freq, fill = y)) + geom_col() + 
  facet_grid(sex ~ trat, labeller = label_both )


# De los 100 individuos simulados en cada combinacion de categorias 
# se toma una muestra aleatoria de entre un 10 y 30% para crear
# un escenario mas real en tamano de muestra y con desbalanceo aleatorio

# Se agrupan y anidan los datos
dat.nest <- dat %>%
  mutate(
    trat1 = trat,   # se duplican las variables porque mas adelante se pierden
    sex1 = sex,
    edadl1 = edadl
  ) %>%
  group_by(trat1,sex1,edadl1) %>%
  nest()

# Se aplica la funcion slice_sample a data.frame de la lista
# data.nest$data
x <- runif(n = nrow(dat.nest), min = 0.16, max = 0.26)
i <- 1:nrow(dat.nest)
m <- function(x,i) slice_sample(dat.nest$data[[i]], prop = x)
dats <- map2_dfr(x,i, m)  # data.frame con la muestra
rm(x,i,m)  # se borran objetos innecesarios

# Verificacion de la simulacion sobre dats:
xtabs(~ trat + y + edadl, data = dats) %>%
  addmargins(margin = c(1,2) )
ggplot(dats, aes(x = factor(edadl), y = edad)) + geom_boxplot()
ggplot(dats, aes(x = factor(edadl), y = yl)) + geom_boxplot() + 
  facet_grid(~ trat, labeller = label_both)

# Grafico de barras apiladas sobre dats
dats %>%
  xtabs(~ trat + y + edadl + sex, data = .) %>%
  prop.table(margin = c(1,3,4)) %>%
  as.data.frame() %>%
  mutate(
    y = fct_rev(y)
  ) %>%
  ggplot(aes(x = edadl, y = Freq, fill = y)) + geom_col() + 
  facet_grid(sex ~ trat, labeller = label_both ) +
  scale_fill_brewer(direction = -1)


# Se exportan los datos
dats %>%
  transmute(
    id        = sample(1:nrow(dats)),
    Treatment = ifelse(trat == 1, "Treated", "Placebo"),
    Sex = ifelse(sex == 1, "Female", "Male"),
    Age = edad,
    Improved = y
  ) %>% 
  write_csv(file = "datos/tratSimul.csv")


