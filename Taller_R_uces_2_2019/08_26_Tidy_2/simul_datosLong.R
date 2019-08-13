# 12-ago-2019
# Simulacion de datos longitudinales
# paguzmang

# Librerias
library(openxlsx)
library(tidyverse)
library(readxl)

# Simulacion (originalmente en formato amplio)
set.seed(3409)
datos <- tibble(
  id = 1:60,
  sexo = rep(c('fem','masc'), each = 30),
  trat = c(rep(c('a', 'b'), times = c(12,18)), 
           rep(c('a', 'b'), times = c(14,16)) ),
  sexo_trat = paste(sexo, trat, sep = '-'),
  t1 = round(rnorm(n = 60, mean = 50, sd = 10), 2),
  t2 = round(rnorm(n = 60, mean = 56, sd = 10), 2),
  t3 = round(rnorm(n = 60, mean = 64, sd = 10), 2)
) %>% sample_n(size = 60, replace = F)
  
# Datos a formato largo:
#datosLargo <- gather(datos, key = 'tiempo', value = 'resp', t1:t3)

# Grafico por sujeto:
#ggplot(datosLargo, aes(x = tiempo, y = resp, group = id)) +
#  geom_line() +  facet_grid(sexo ~ trat )

# Datos a formato amplio
# datosAmplio <-  spread(datosLargo, key  = tiempo, value = resp)

# Se exportan los datos a archivos de excel
wb <- createWorkbook('pablo')
addWorksheet(wb, sheetName = 'metadatos')
addWorksheet(wb, sheetName = 'datos')
writeData(wb, sheet = 'datos', startCol = 3, startRow = 4, borders = 'surrounding', 
          x = datos %>%  select(-sexo, -trat) %>% slice(1:30) )
writeData(wb, sheet = 'metadatos', startCol = 1, startRow = 1, 
          x = 'Descripción de las variables')
writeData(wb, sheet = 'metadatos', startCol = 1, startRow = 3, 
          headerStyle = createStyle(fgFill = "#DCE6F1", halign = "CENTER", textDecoration = "italic",
                                    border = "Bottom"),
          x = tribble(
            ~variable, ~descripción,
            'id', 'Identificación única de cada sujeto',
            'sexo_trat', 'Género (fem ó masc) de cada sujeto en conjunto con el tratamiento asigando (a ó b)',
            't1', 'Respuesta medida en el tiempo 1',
            't2', 'Respuesta medida en el tiempo 2',
            't3', 'Respuesta medida en el tiempo 3'
          ) )
saveWorkbook(wb, "datosLong1.xlsx", overwrite = TRUE)
rm(wb, datos)

# Se exportan los otros datos a un archivo csv:
datos %>% slice(31:60) %>% select(-sexo, -trat) %>%
  write.table(file = 'datosLong2.csv', quote = F, sep = ';', 
              col.names = T, row.names = F)










