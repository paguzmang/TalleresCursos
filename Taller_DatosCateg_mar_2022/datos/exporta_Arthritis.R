

# Exportacion de tabla de Artritis del paquete vcd
library(vcd)
head(Arthritis)
write.csv(x = Arthritis, file = "artritisFl.csv", quote = F, row.names = F)

library(tidyverse)
Arthritis %>%
  pull(Age) %>%
  hist(main = NA, xlab = "Edad")

Arthritis %>%
  mutate(
    AgeF =  cut(Age, br = seq(20, 80, 20))
  ) %>%
  xtabs(~ AgeF + Improved, data = ., subset = Treatment == "Placebo")


Arthritis %>%
  mutate(
    Age =  cut(Age, br = seq(20, 80, 20), 
               labels = c("(20 a 40]", "(40 a 60]", "(60 a 80]"))
  ) %>%
  select(-ID) %>%
  xtabs(~ ., data = .) %>%
  as.data.frame() %>%
  rename(n = Freq) %>%
  write_csv(file = "artritisFa.csv")



