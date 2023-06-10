# Minería de datos
# Laboratorio 2: Agrupamiento K-medias
# Profesor: Max Chacón
# Alumnos: Matías Escudero, Joaquín Macías
# Doctorado en Ciencias de la Ingeniería con Mención en Informática - USACH

#### LIBRERÍAS ####

library(readr)
library(tidyverse)
library(xtable)
library(GGally)
library(RColorBrewer)
library(gridExtra)
library(dplyr)
library(corrplot)
library(ggcorrplot)
library(naniar)
library(visdat)
library(corrplot)
library(ggcorrplot)
library(mice)

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


### IMPUTACIÓN DE DATOS ###

## Se genera tabla para estudiar el número de NA's existentes

na_counts = sapply(allhyper_df, function(x) sum(is.na(x))) # Número de NA's por columna
total_counts = sapply(allhyper_df, function(x) length(x)) # Número total de observaciones por columna
na_percentages = round((na_counts / total_counts) * 100, 2) # Porcentaje de NA's por columna

# Se pasan los resultados a un DF
na_table = data.frame(Column = names(na_percentages), na_percentage = na_percentages)

# Se filtran para mostrar únicamente aquellas columnas en dónde existen NA's
na_table = na_table[na_table$na_percentage > 0,]
na_table = na_table[order(na_table$na_percentage), ]

# Se transforma la tabla a formato LaTeX
latex_na = xtable(na_table)
print(latex_na)

## Se elimina el registro con NA para la columna sex y se elimina la columna TBG y referal_source
allhyper_df = allhyper_df %>%
  select(-TBG) %>%    # Elimina la columna TBG
  filter(!is.na(age)) # Elimina los registros NA de age


## Se genera gráfico para estudiar cómo se distribuyen los valores de NA en las distintas variables
allhyper_df_NA = allhyper_df %>% select_if(~any(is.na(.)))

# Se visualiza el gráfico para visualizar NA's
vis_miss(allhyper_df_NA)

## En base a lo anterior, se imputa la moda de la variable sex a los valores faltantes.
allhyper_df$sex[is.na(allhyper_df$sex)] = 'F'


## Matriz de correlación para la imputación de datos
cor_matrix <- cor(allhyper_df[, c("age", "TSH", "T3", "TT4", "T4U", "FTI")], use = "pairwise.complete.obs")

# Crear la visualización de la matriz de correlación
ggcorrplot(cor_matrix, hc.order = TRUE, type = "lower", 
           lab = TRUE, lab_size = 3, method="square", 
           colors = c("#6D9EC1", "white", "#E46726"), 
           title="Matriz de Correlación Variables Numéricas")

## IMPUTACIÓN EN VARIABLES NUMÉRICAS 

# Se genera una copia del DF original
df_imputed = allhyper_df

# imputa 'TT4' usando 'FTI' y 'T3'
temp_data = mice(df_imputed[, c("TT4", "FTI", "T3")], method='pmm', m=1)
df_imputed$TT4 = complete(temp_data,1)$TT4

# imputa 'FTI' usando 'TT4' y 'TSH'
temp_data = mice(df_imputed[, c("FTI", "TT4", "TSH")], method='pmm', m=1)
df_imputed$FTI = complete(temp_data,1)$FTI

# imputa 'T3' usando 'TT4' y 'T4U'
temp_data = mice(df_imputed[, c("T3", "TT4", "T4U")], method='pmm', m=1)
df_imputed$T3 = complete(temp_data,1)$T3

# imputa 'TSH' usando 'FTI'
temp_data = mice(df_imputed[, c("TSH", "FTI")], method='pmm', m=1)
df_imputed$TSH = complete(temp_data,1)$TSH

# imputa 'T4U' usando 'T3'
temp_data = mice(df_imputed[, c("T4U", "T3")], method='pmm', m=1)
df_imputed$T4U = complete(temp_data,1)$T4U


### RE-CODIFICACIÓN ###

df_imputed$sex = ifelse(df_imputed$sex == 'M', 1, 0)

# Se convierten las columnas booleanas a numéricas
df_imputed$on_thyroxine <- as.numeric(df_imputed$on_thyroxine)
df_imputed$query_on_thyroxine <- as.numeric(df_imputed$query_on_thyroxine)
df_imputed$on_antithyroid_medication <- as.numeric(df_imputed$on_antithyroid_medication)
df_imputed$sick <- as.numeric(df_imputed$sick)
df_imputed$pregnant <- as.numeric(df_imputed$pregnant)
df_imputed$thyroid_surgery <- as.numeric(df_imputed$thyroid_surgery)
df_imputed$I131_treatment <- as.numeric(df_imputed$I131_treatment)
df_imputed$query_hypothyroid <- as.numeric(df_imputed$query_hypothyroid)
df_imputed$query_hyperthyroid <- as.numeric(df_imputed$query_hyperthyroid)
df_imputed$lithium <- as.numeric(df_imputed$lithium)
df_imputed$goitre <- as.numeric(df_imputed$goitre)
df_imputed$tumor <- as.numeric(df_imputed$tumor)
df_imputed$hypopituitary <- as.numeric(df_imputed$hypopituitary)
df_imputed$psych <- as.numeric(df_imputed$psych)
df_imputed$TSH_measured <- as.numeric(df_imputed$TSH_measured)
df_imputed$T3_measured <- as.numeric(df_imputed$T3_measured)
df_imputed$TT4_measured <- as.numeric(df_imputed$TT4_measured)
df_imputed$T4U_measured <- as.numeric(df_imputed$T4U_measured)
df_imputed$FTI_measured <- as.numeric(df_imputed$FTI_measured)
df_imputed$TBG_measured <- as.numeric(df_imputed$TBG_measured)

