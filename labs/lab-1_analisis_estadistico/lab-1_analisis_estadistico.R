# Minería de datos
# Profesor: Max Chacón
# Alumnos: Matías Escudero, Joaquín Macías
# Doctorado en Ciencias de la Ingeniería con Mención en Informática - USACH

### LIBRERÍAS ###
library(readr)
library(tidyverse)

### ENTRADAS ###

## Contenido del DataFrame

allhyper_df = read_csv("data/allhyper.data",
                       col_names = FALSE)


## Se extraen los nombres de las columnas extraídas a partir del archivo allhyper.names
allhyper_names = read_lines("data/allhyper.names")

column_names = gsub(":.*", "", contenido_archivo[11:39])
column_names = gsub(" ", "_", column_names)
column_names = c(column_names, "class") # Se añade la columna "class" que no se especifica en el archivo

## Se asignan al DF
colnames(allhyper_df) = column_names



