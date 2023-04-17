# Minería de datos
# Profesor: Max Chacón
# Alumnos: Matías Escudero, Joaquín Macías
# Doctorado en Ciencias de la Ingeniería con Mención en Informática - USACH

#### LIBRERÍAS ####

library(readr)
library(tidyverse)
library(xtable)

#### ENTRADAS ####

## Se lee el contenido del DataFrame, diciendole que "?" representa los NA's

allhyper_df = read_csv("data/allhyper.data",
                       col_names = FALSE,
                       na = "?")


## Se extraen los nombres de las columnas partir del archivo allhyper.names

# Abre el archivo
allhyper_names = read_lines("data/allhyper.names")

# Extrae únicamente los nombres, descartando la información que no es relevante.
column_names = gsub(":.*", "", allhyper_names[11:39])
column_names = gsub(" ", "_", column_names)
column_names = c(column_names, "class") # Se añade la columna "class" que no se especifica en el archivo

## Se asignan las columnas al DF
colnames(allhyper_df) = column_names


#### PRE-PROCESAMIENTO ####

## Se generan nuevas columnas que separe la clase en nombre e identificador

allhyper_df = extract(data = allhyper_df, 
                      col = class, 
                      into = c("class_name", "class_id"), 
                      regex = "(.*)\\|(.*)")

## Se elimina la columna class_id, ya que no entrega información relevante
allhyper_df$class_id = NULL

#### Análisis Descriptivo ####

# Estadísticas para variables numéricas
numeric_vars = allhyper_df %>% select(where(is.numeric))
numeric_summary = numeric_vars %>% summarise(across(everything(), list(min = ~min(., na.rm = TRUE), q1 = ~quantile(., 0.25, na.rm = TRUE), median = ~median(., na.rm = TRUE), mean = ~mean(., na.rm = TRUE), q3 = ~quantile(., 0.75, na.rm = TRUE), max = ~max(., na.rm = TRUE), sd = ~sd(., na.rm = TRUE), na = ~sum(is.na(.))))) %>% gather("variable", "value") %>% separate(variable, c("variable", "statistic"), sep = "_") %>% spread(statistic, value)

# Se exporta a formato LaTeX
numeric_latex_table = xtable(numeric_summary, caption = "Resumen estadístico variables numéricas.")
print(numeric_latex_table, type = "latex", include.rownames = FALSE)

# Función para calcular la frecuencia de cada categoría
freq_count <- function(x) {
  tbl <- table(x)
  most_common <- names(tbl)[tbl == max(tbl)]
  paste(most_common, collapse = ", ")
}

# Estadísticas para variables categóricas
