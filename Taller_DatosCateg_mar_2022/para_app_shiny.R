
# librerias
library(tidyverse)

# Datos
dat <- read.csv(file = "datos/tratSimul.csv")
str(dat)
unique(dat$Treatment)

# Se modifica la tabla
dat1 <- dat %>%
  mutate(
    Edad   = cut(Age, br = c(20,40,60,80), labels = c("20-40","40-60","60-80")),
    Genero = fct_recode(Sex, Mujer = "Female", Hombre = "Male"),
    Trat   = fct_recode(Treatment, Tratado = "Treated"),
    Mejora = factor(Improved, levels = c("None", "Low", "Medium", "High"),
                    labels = c("Ninguna", "Baja", "Media", "Alta")),
  ) %>% select(-(Treatment:Improved))


# Seleccion del usuario:
nomv <- data.frame(
  op = c("Edad", "Género", "Tratamiento"),
  i  = c(2,3,4) 
)
op.r1  <- nomv$op
r1     <- "Género"
r1     <- nomv$i[nomv$op == r1]
nomv2  <- subset(nomv, i != r1)
op.r2  <- c(nomv2$op, "Ninguna")
r2     <- "Tratamiento"
r2     <- ifelse(r2 == "Ninguna", 0, nomv$i[nomv$op == r2])

if(r2 == 0){
  # Grafico si r2 = Ninguna
  dat1f <- select(dat1, r1, 5 )
  names(dat1f)[1] <- "x"
  table(dat1f) %>%
    prop.table(margin = 1) %>%
    as.data.frame() %>%
    ggplot(aes(x = x, y = Freq*100, fill = fct_rev(Mejora))) + 
    geom_col() +
    scale_fill_brewer(direction = -1, palette = "YlGn") +
    labs(x = nomv$op[nomv$i == r1], y = "% de Pacientes", 
         fill = "Mejora en\nlos síntomas")
} else{
  # Grafico si r2 != Ninguna
  dat1f <- select(dat1, r1, 5, r2 )
  names(dat1f)[c(1,3)] <- c("x1", "x2")
  table(dat1f) %>%
    prop.table(margin = c(1,3)) %>%
    as.data.frame() %>%
    ggplot(aes(x = x1, y = Freq*100, fill = fct_rev(Mejora))) + 
    geom_col() +
    scale_fill_brewer(direction = -1, palette = "YlGn") +
    facet_grid(~ x2) +
    labs(x = nomv$op[nomv$i == r1], y = "% de Pacientes", 
         fill = "Mejora en\nlos síntomas")
}








