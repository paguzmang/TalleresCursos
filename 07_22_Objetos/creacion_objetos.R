# Julio 2019
# Codigo para preparar los datos que acompanan la presentacion
# de 07_22_Objetos.Rmd

# Librerias
library(Sleuth3)  # para case2002, datos sobre bird keeping
rm(list = ls())

# (1) Bird Keeping y cancer de pulmon ----
data(case2002)
birdKeep <- case2002
for(i in 1:4) birdKeep[, i] <- as.character(birdKeep[, i])
birdKeep$LC <- ifelse(birdKeep$LC == 'LungCancer', 1, 0)
rm(case2002, i)


# (2) Contaminacion por E. coli en Bahia de California ----
ecoli <- matrix(
  data = c(46,79,35, 29,56,23, 106,32,0, 
           38,63,60, 22,26,08),
  ncol = 5, 
  dimnames = list(loc = c('A', 'B', 'C'),
                  fuente = c('aves', 'mascotas', 'anim_granja', 'humanos', 
                             'mamif_ter')
  )
)

# (3) Variables quimicas de varios rios ----
# en una zona montanosa cerca a New York
lovett <- read.table(file = 'lovett2.csv', header = T, sep = '\t', as.is = T)
typeof(lovett)
class(lovett)
lovett <- transform(
  lovett,
  r2.NH4 = sqrt(NH4),
  r4.H   = H^(1/4)
)

# (4) Una funcion ----
# que describe los objetos en su modo, tipo y clase
descrip_obj <- function(x){
  data.frame(
    typeof = typeof(x),
    class  = class(x),
    mode   = mode(x),
    length = length(x),
    stringsAsFactors = F
  )
}

# Guardar todos los datos en un solo archivo ----
attach(birdKeep)
BK <- factor(BK, levels = c('NoBird', 'Bird'))
save(LC, FM, SS, BK, AG, YR, CD, ecoli, lovett, descrip_obj, file = 'objetos.RData')
detach(birdKeep)
rm(birdKeep, BK, ecoli, lovett, descrip_obj)
