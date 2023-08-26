


# Parametros para un modelo de una respuesta binaria con dos 
# predictoras: x1 y x2. 
# x1 tiene categorias A, B y C (region del pais)
# x2 tiene categorias "bajo", "medio" , "alto" (estrato)
# x3 nro. de hijos
p0      <- 0.1
odds0   <- p0 / (1 - p0)
beta0   <- log(odds0)
or.x1B  <- 3
or.x1C  <- 3
or.x2medio   <- 4
or.x2alto    <- 4
or.x3 <- 0.5
beta.x1B <- log(or.x1B)
beta.x1C <- log(or.x1C)
beta.x2medio  <- log(or.x2medio)
beta.x2alto  <- log(or.x2alto)
beta.x3  <- log(or.x3)


# Simulacion
library(tidyverse)
N <- 345
#set.seed(4286)
#set.seed(7903)
dat <- tibble(
  x1 = sample(x = LETTERS[1:3], size = N, replace = T),
  x2 = sample(x = c("bajo", "medio", "alto"), size = N, replace = T),
  x3 = rpois(n = N, lambda = 1),
  logit = beta0 + beta.x1B*(x1 == "B") + beta.x1C*(x1 == "C") + 
    beta.x2medio*(x2 == "medio") + beta.x2alto*(x2 == "alto") + beta.x3*x3, 
  p = 1 / (1 + exp(-logit)),
  y = rbinom(n = N, size = 1, prob = p)
) %>% 
  mutate(
    x2 = fct_relevel(x2, "bajo", "medio", "alto")
  ) %>%
  select(-logit, -p)

# Datos agrupados:
datg <- dat %>%
  group_by(x1, x2, x3) %>%
  summarise(
    n = n(),
    ne = sum(y),
    nf = n - ne
  ) %>% ungroup()

# Modelo
m <- glm(cbind(ne, nf) ~  x1 + x2 + x3, data = datg, family = binomial)  

# Coeficientes
coef(m)
tibble(beta0, beta.x1B, beta.x1C, beta.x2alto, beta.x2medio, beta.x3)
exp(coef(m))
car::Anova(m)

boxplot(
  rstandard(m, type = "pearson") ~ datg$x1
)
boxplot(
  rstandard(m, type = "pearson") ~ datg$x2
)

plot(
  rstandard(m, type = "pearson") ~ datg$x3
)

plot(
  rstandard(m, type = "pearson") ~ predict(m, type = "response")
)
  
shapiro.test(rstandard(m, type = "pearson"))

write_csv(dat, file = "datos/plastico.csv")
