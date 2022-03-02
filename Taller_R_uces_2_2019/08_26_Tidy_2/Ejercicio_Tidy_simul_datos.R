# 12-ago-2019
# Operaciones "Tidy" con un data.frame que tiene un conjunto
# de datos longitudinales simulados.
# paguzmang

# Operaciones practicadas:
# Lectura de datos (no es tidy)
# Juntar tablas
# Separar columnas
# Revisar estructura o diseno del estudio
# Pasar de formato amplio a formato largo y viceversa

# Datos:
# Se midio un variable respuesta en tres momentos (t1, t2 y t3) a cada uno
# de 60 sujetos. El estudio incluyo sujetos de ambos sexos (fem y masc) y 
# que fueron asignados a dos tratamientos (a y b). Los datos estan repartidos
# en dos archivos: 'simuLong_masc.xlsx' (tiene los sujetos hombres) y 
# 'simuLong_fem.csv' (tiene los sujetos mujeres).

# Librerias necesarias: tidyverse (usaremos tidyr y dplyr)

# (1) Importe cada archivo y verifique la lectura
# (2) Junte las tablas por filas.
# (3) Separe la columna sexo_trat en dos, una solo con el sexo y otra con el trat.
# (4) Cree un objeto que organice los datos de tal forma que el tiempo (los tres momentos) queden en una sola columna
# (5) Usando el objeto del paso (4) cree otro objeto como el original.
# (6) Piense que analisis se puede hacer desde los datos en un formato y otro.









