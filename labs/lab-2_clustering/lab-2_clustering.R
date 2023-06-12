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
library(factoextra)
library(cluster)
library(purrr)

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
df_imputed$on_thyroxine = as.numeric(df_imputed$on_thyroxine)
df_imputed$query_on_thyroxine = as.numeric(df_imputed$query_on_thyroxine)
df_imputed$on_antithyroid_medication = as.numeric(df_imputed$on_antithyroid_medication)
df_imputed$sick = as.numeric(df_imputed$sick)
df_imputed$pregnant = as.numeric(df_imputed$pregnant)
df_imputed$thyroid_surgery = as.numeric(df_imputed$thyroid_surgery)
df_imputed$I131_treatment = as.numeric(df_imputed$I131_treatment)
df_imputed$query_hypothyroid = as.numeric(df_imputed$query_hypothyroid)
df_imputed$query_hyperthyroid = as.numeric(df_imputed$query_hyperthyroid)
df_imputed$lithium = as.numeric(df_imputed$lithium)
df_imputed$goitre = as.numeric(df_imputed$goitre)
df_imputed$tumor = as.numeric(df_imputed$tumor)
df_imputed$hypopituitary = as.numeric(df_imputed$hypopituitary)
df_imputed$psych = as.numeric(df_imputed$psych)
df_imputed$TSH_measured = as.numeric(df_imputed$TSH_measured)
df_imputed$T3_measured = as.numeric(df_imputed$T3_measured)
df_imputed$TT4_measured = as.numeric(df_imputed$TT4_measured)
df_imputed$T4U_measured = as.numeric(df_imputed$T4U_measured)
df_imputed$FTI_measured = as.numeric(df_imputed$FTI_measured)


# Se eliminan las columnas irrelevantes
df_imputed$TBG_measured = NULL
df_imputed$referral_source = NULL

# Se borran las filas donde la clase es 'goitre'
df_imputed = df_imputed[!(df_imputed$class_name == 'goitre.'),]

# Reclasificamos 'T3 toxic' y 'hyperthyroid' como 1 (hipertiroidismo) y 'negative' como 0 (negativo)
df_imputed$class[df_imputed$class_name %in% c('T3 toxic.', 'hyperthyroid.')] = 1
df_imputed$class[df_imputed$class_name == 'negative.'] = 0

# Se elimina la columna class_name
df_imputed$class_name = NULL

# Se cambia el nombre de la variable class por hyperthiroid
colnames(df_imputed)[27] = 'hyperthiroid'


## Se cambia el valor de 455 de la columna age por el promedio
# Se identifica la fila donde age es 455
index <- which(df_imputed$age == 455)
# Se calcula el promedio de la columna age (sin incluir el valor 455)
mean_age <- mean(df_imputed$age[df_imputed$age != 455])
# se reemplaza el valor 455 por el promedio
df_imputed$age[index] <- mean_age

### NORMALIZACIÓN ###

## Se crea una función de normalización de columnas con el método min-max

normalize_column <- function(df, column_name){
  
  # Se obtiene la columna a normalizar
  column = df[[column_name]]
  
  # Aplica la normalización min-max
  min_val = min(column)  # Encuentra el valor mínimo
  max_val = max(column)  # Encuentra el valor máximo
  df[[column_name]] = (column - min_val) / (max_val - min_val)  # Normaliza la columna
  
  return(df)  # Devuelve el dataframe actualizado
}

# Se copia el DF que contiene los valores originales
df_normalized = df_imputed

# Se normalizan las columnas numéricas
df_normalized = normalize_column(df_normalized, 'age')
df_normalized = normalize_column(df_normalized, 'TSH')
df_normalized = normalize_column(df_normalized, 'T3')
df_normalized = normalize_column(df_normalized, 'TT4')
df_normalized = normalize_column(df_normalized, 'T4U')
df_normalized = normalize_column(df_normalized, 'FTI')



#### CLUSTERIZACIÓN ####

### Elección de K ###

# Trazar el total de la suma de cuadrados en función de k
fviz_nbclust(df_normalized, FUNcluster = pam, diss = dist(df_normalized, method = "manhattan"), method = "wss") +
    geom_vline(xintercept = 3, linetype = 2) +
  labs(title = "Número óptimo de clusters",
    subtitle = "Método del Codo",
    x = "Número de clusters",
    y = "Suma total de cuadrados dentro")



### Resultados ###


## K-means ##
# Se ejecuta el modelo para k = 3 y k = 7
clusters3_model = pam(df_normalized, 3, diss = FALSE, metric = "manhattan")
clusters7_model = pam(df_normalized, 7, diss = FALSE, metric = "manhattan")

# Asignar los clusters a los datos
df_normalized$cluster3 = clusters3_model$clustering
df_normalized$cluster7 = clusters7_model$clustering

## Se pasan las variables de clusterización al DF original y así facilitar la interpretación
df_imputed$cluster3 = df_normalized$cluster3
df_imputed$cluster7 = df_normalized$cluster7


# Gráfico de pacientes por cluster
ggplot(df_imputed, aes(x = factor(cluster3))) +
  geom_bar(fill = "steelblue") +
  labs(
    x = "Cluster",
    y = "N° Pacientes",
    title = "Número de pacientes por cluster"
  ) +
  theme_minimal()


# Gráfico de pacientes por cluster
ggplot(df_imputed, aes(x = factor(cluster7))) +
  geom_bar(fill = "steelblue") +
  labs(
    x = "Cluster",
    y = "N° Pacientes",
    title = "Número de pacientes por cluster"
  ) +
  theme_minimal()


## RESUMEN ESTADÍSTICO

# Crea una lista con los nombres de las columnas numéricas
numeric_vars = c("age", "TSH", "T3", "TT4", "T4U", "FTI")  # Asegúrate de ajustar esto a tus columnas numéricas

# Crea una función para obtener las estadísticas descriptivas de cada cluster
get_cluster_summary <- function(cluster_num) {
  df_imputed %>%
    filter(cluster3 == cluster_num) %>%
    summarise(across(numeric_vars, list(mean = mean, sd = sd, median = median, IQR = IQR)))
}


get_cluster_summary_k7 <- function(cluster_num) {
  df_imputed %>%
    filter(cluster7 == cluster_num) %>%
    summarise(across(numeric_vars, list(mean = mean, sd = sd, median = median, IQR = IQR)))
}


# Usa purrr::map_dfr para aplicar la función a cada cluster y combina los resultados en un solo DataFrame
cluster_summaries = purrr::map_dfr(1:3, get_cluster_summary, .id = "Cluster")
cluster7_summaries = purrr::map_dfr(1:7, get_cluster_summary_k7, .id = "Cluster")

# Crear una copia de df_imputed
df_bool = df_imputed

# Variables a excluir
excluded_vars = c('age', 'TSH', 'T3', 'TT4', 'T4U', 'FTI')

# Excluir las variables
df_bool = df_bool[, !(names(df_bool) %in% excluded_vars)]

# Función para contar 0 y 1 por variable
count_zero_one = function(var) {
  df_bool %>%
    group_by(cluster3) %>%
    summarise(zero_count = sum(.data[[var]] == 0, na.rm = TRUE),
              one_count = sum(.data[[var]] == 1, na.rm = TRUE)) %>%
    mutate(variable = var)
}

count_zero_one_k7 = function(var) {
  df_bool %>%
    group_by(cluster7) %>%
    summarise(zero_count = sum(.data[[var]] == 0, na.rm = TRUE),
              one_count = sum(.data[[var]] == 1, na.rm = TRUE)) %>%
    mutate(variable = var)
}



# Aplicar la función a cada variable
summary_bool = lapply(names(df_bool), count_zero_one)
summary_bool_k7 = lapply(names(df_bool), count_zero_one_k7)

# Combinar todos los dataframes en uno solo
summary_bool = do.call(rbind, summary_bool)
summary_bool_k7 = do.call(rbind, summary_bool_k7)

## CORRPLOTS

# Crear subconjuntos de datos para cada cluster
cluster1 = df_imputed[df_imputed$cluster3 == 1, ]
cluster2 = df_imputed[df_imputed$cluster3 == 2, ]
cluster3 = df_imputed[df_imputed$cluster3 == 3, ]

# Variables de interés
numeric_vars = c("age", "TSH", "T3", "TT4", "T4U", "FTI")

# Gráficos de pares para cada cluster
ggpairs(cluster1[numeric_vars]) + ggtitle("Cluster 1")
ggpairs(cluster2[numeric_vars]) + ggtitle("Cluster 2")
ggpairs(cluster3[numeric_vars]) + ggtitle("Cluster 3")



