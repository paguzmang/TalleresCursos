# Abril 2022
# Codigo para explorar relaciones teoricas entre parametros y 
# valores de x para modelar un patron de dosis-respuesta con un efecto
# parabolico. Es decir, luego de cierto valor x, la respuesta
# cae.

# Funciones
logist <- function(x, b0, b1, b2){
  logit <- b0 + b1*x + b2*x^2
  1 / (1 + exp(-logit))
}
logit <- function(x, b0, b1, b2){
  b0 + b1*x + b2*x^2
}

# Parametros
p0 <- 0.05
b0 <- log(odds(p0))
b1 <- 0.8
x_max <- 14
b2 <- -b1 / (2*x_max)
xlim_max <- 25

# Grafico
par(mfrow = c(1,2), mar = c(3,3,2,1), mgp = c(2,1,0), cex = 0.9)
curve(logit(x, b0,b1,b2), from = 0, to = xlim_max, main = "logit")
abline(v = x_max, h = logit(x = x_max, b0,b1,b2), lty = 2)
curve(logist(x, b0,b1,b2), from = 0, to = xlim_max, ylim = c(0,1), main = "Prob")
abline(v = x_max, h = c(1, logist(x = x_max, b0, b1, b2)), lty = 2)

# Simulacion:
dat <- tibble(
  x = seq(0,25,5),
  n = 20,
  logit = logit(x = x, b0, b1, b2),
  p = logist(x = x, b0, b1, b2),
  y = rbinom(n = length(x), size = n, prob = p),
  p_obs = y /n 
)

ggplot(dat, aes(x = x, y = p_obs)) + geom_point() +
  geom_line(aes(y = p)) +
  geom_smooth(method = glm, formula = y ~ x + I(x^2))