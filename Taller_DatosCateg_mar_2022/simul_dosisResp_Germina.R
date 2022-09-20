# Abril 2022
# Simulacion de dosis respuesta para la germinacion con
# dos nutrientes A y B

# Funciones
odds <- function(p) p/(1-p)
prob <- function(odds) odds / (1 + odds)
logit <- function(x, trat, b0, b1, b2, b3) b0 + b1*x + b2*trat + b3*trat*x
logist <- function(x, trat, b0, b1, b2, b3){
  logit <- b0 + b1*x + b2*trat + b3*trat*x
  1 / (1 + exp(-logit))
}


# Definicion del rango en 
x <- seq(0,30,5)

# Nutriente A
x50 <- 20
p0 <- 0.25
b0 <- log(odds(p0))
b1 <- -b0 / x50

# Nutriente B
x50B <- 8
p0B <- 0.25
b0B <- log(odds(p0B))
b1B <- -b0B / x50B
b2 <- b0B - b0
b3 <- b1B - b1

par(mfrow = c(1,2), mar = c(3,3,2,1), mgp = c(2,1,0), cex = 0.9)
# Logit
curve(logit(x, trat = 0, b0, b1, b2, b3), from = 0, to = 30, col = "red")
curve(logit(x, trat = 1, b0, b1, b2, b3), from = 0, to = 30, add = T, col = "grey50")
abline(h = 0, v = c(x50B, x50), lty = 2, col = c("red", "grey50"))
# Prob
curve(logist(x, trat = 0, b0, b1, b2, b3), from = 0, to = 30, col = "red", ylim = c(0,1))
curve(logist(x, trat = 1, b0, b1, b2, b3), from = 0, to = 30, add = T, col = "grey50")
abline(h = 0.5, v = c(x50B, x50), lty = 2, col = c("red", "grey50"))

# Simulacion
res <- replicate(
  n = 2000,
  expr = {
    dat <- expand_grid(
      nitrog = x,
      trat = c("A", "B"),
      n = 20
    ) %>%
      mutate(
        logit = b0 + b1*nitrog + b2*(trat == "B") + b3*(trat == "B")*nitrog,
        p = prob(exp(logit)),
        n_germ = rbinom(n = length(nitrog), size = n, prob = p),
        p_germ = n_germ /n 
      )
    
    m <- glm(p_germ ~ nitrog*trat, data = dat, family = binomial(link = logit), weights = n)
    raov <-anova(update(m, . ~ . - nitrog:trat), m, test = "LRT")
    p.int <- raov$`Pr(>Chi)`[2]
    b <- coef(m)
    names(b) <- 0:3
    x50A <- - b["0"] / b["1"]
    x50B <- - (b["0"]+b["2"]) / (b["1"] + b["3"])
    c(p.int, x50A, x50B, x50A - x50B)
  }
)

hist(res[3, ])
mean(res[1, ] < 0.05)


nd <- expand_grid(
  nitrog = 0:30,
  trat = c("A", "B")
) %>% mutate(prob = predict(m, newdata = data.frame(nitrog, trat), type = "response"))

