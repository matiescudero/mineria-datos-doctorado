# Minería de datos
# Profesor: Max Chacón
# Alumnos: Matías Escudero, Joaquín Macías
# Doctorado en Ciencias de la Ingeniería con Mención en Informática - USACH

#### LIBRERÍAS ####

library(readr)
library(tidyverse)
library(xtable)
library(dlookr)

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

## VARIABLES NUMÉRICAS

# Filtro para variables numéricas 
numeric_vars = allhyper_df %>% select(where(is.numeric))

# Se genera tabla con variables, max, mean, min, n° de na's q1, q3 y SD
numeric_summary = numeric_vars %>% summarise(across(everything(), list(min = ~min(., na.rm = TRUE), q1 = ~quantile(., 0.25, na.rm = TRUE), median = ~median(., na.rm = TRUE), mean = ~mean(., na.rm = TRUE), q3 = ~quantile(., 0.75, na.rm = TRUE), max = ~max(., na.rm = TRUE), sd = ~sd(., na.rm = TRUE), na = ~sum(is.na(.))))) %>% gather("variable", "value") %>% separate(variable, c("variable", "statistic"), sep = "_") %>% spread(statistic, value)

# Se exporta a formato LaTeX
numeric_latex_table = xtable(numeric_summary, caption = "Resumen estadístico variables numéricas.")
print(numeric_latex_table, type = "latex", include.rownames = FALSE)


## VARIABLES CATEGÓRICAS

## Filtro para variables categóricas
categorical_vars = allhyper_df %>% select(where(is.character))

## Se calcula la frecuencia y el porcentaje para cada variable categórica
sex_freq = table(allhyper_df$sex, useNA = "always")
sex_perc = prop.table(sex_freq) * 100

referral_source_freq = table(allhyper_df$referral_source, useNA = "always")
referral_source_perc = prop.table(referral_source_freq) * 100

class_name_freq = table(allhyper_df$class_name, useNA = "always")
class_name_perc = prop.table(class_name_freq) * 100

# Se genera un Data Frame con los resultados
sex_summary = data.frame(variable = "sex", category = names(sex_freq), frequency = as.vector(sex_freq), percentage = as.vector(sex_perc))
referral_source_summary = data.frame(variable = "referral_source", category = names(referral_source_freq), frequency = as.vector(referral_source_freq), percentage = as.vector(referral_source_perc))
class_name_summary = data.frame(variable = "class_name", category = names(class_name_freq), frequency = as.vector(class_name_freq), percentage = as.vector(class_name_perc))

# Se combinan los DF's
categorical_summary = rbind(sex_summary, referral_source_summary, class_name_summary)

# Se redondean los porcentajes a 2 decimales
categorical_summary$percentage = round(categorical_summary$percentage, 2)

# Se exporta a formato LaTeX
categorical_latex_table = xtable(categorical_summary, caption = "Resumen estadístico variables categóricas.")
print(categorical_latex_table, type = "latex", include.rownames = FALSE)

## VARIABLES BOOLEANAS

# Filtro para variables booleanas
boolean_vars = allhyper_df %>% select(where(is.logical))

# Se calculan las frecuencias y porcentajes de los valores True y False para cada variable
boolean_summary_long = gather(boolean_vars, key = "variable", value = "value")
boolean_summary_grouped = group_by(boolean_summary_long, variable, value)
boolean_summary = summarise(boolean_summary_grouped, frequency = n(), percentage = round((frequency / sum(n())) * 100, 2))

# Se cambia el formato de la tabla de resumen
boolean_summary_wide = boolean_summary %>%
  spread(value, frequency) %>%
  rename(count_true = "TRUE", count_false = "FALSE")

# Columnas de porcentaje
boolean_summary_wide$true_ptje = round((boolean_summary_wide$count_true / (boolean_summary_wide$count_true + boolean_summary_wide$count_false)) * 100, 2)
boolean_summary_wide$false_ptje = round((boolean_summary_wide$count_false / (boolean_summary_wide$count_true + boolean_summary_wide$count_false)) * 100, 2)

# Se elimina la columna de porcentaje
boolean_summary_wide$percentage = NULL

# Se exporta la tabla a formato LaTeX
boolean_latex_table = xtable(boolean_summary_wide, caption = "Resumen estadístico variables booleanas.")
print(boolean_latex_table, type = "latex", include.rownames = FALSE)


