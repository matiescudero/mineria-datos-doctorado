# Minería de datos
# Laboratorio 4: Árboles de Decisión
# Profesor: Max Chacón
# Alumnos: Matías Escudero, Joaquín Macías
# Doctorado en Ciencias de la Ingeniería con Mención en Informática - USACH

#### LIBRERÍAS ####

library(readr)
library(dplyr)
library(C50)


#### ENTRADAS ####

## Se lee el Data Frame ya procesado a partir del laboratorio 2.

imputed_allhyper = read_csv("data/imputed_allhyper.csv", 
                            col_types = cols(age = col_integer(), 
                                             sex = col_integer(), on_thyroxine = col_integer(), 
                                             query_on_thyroxine = col_integer(), 
                                             on_antithyroid_medication = col_integer(), 
                                             sick = col_integer(), pregnant = col_integer(), 
                                             thyroid_surgery = col_integer(), 
                                             I131_treatment = col_integer(), query_hypothyroid = col_integer(), 
                                             query_hyperthyroid = col_integer(), 
                                             lithium = col_integer(), goitre = col_integer(), 
                                             tumor = col_integer(), hypopituitary = col_integer(), 
                                             psych = col_integer(), TSH_measured = col_integer(), 
                                             T3_measured = col_integer(), TT4_measured = col_integer(), 
                                             T4U_measured = col_integer(), FTI_measured = col_integer(), 
                                             hyperthiroid = col_integer()))


#### PRE-PROCESAMIENTO ####

# Se genera nuevo dataframe
categorical_df = imputed_allhyper


## Se eliminan columnas

# Se calcula el porcentaje de valores 'FALSO' o 0 en cada columna
percentage_false = colMeans(categorical_df == 'FALSO' | categorical_df == 0)

# Se seleccionan las columnas donde más del 98% de los datos son 'FALSO' o 0
columns_to_remove <- names(percentage_false[percentage_false > 0.98])

# Se eliminan estas columnas del DF
categorical_df = categorical_df[ , !(names(categorical_df) %in% columns_to_remove)]


# Además se remueven variables de medición para evitar ruido
categorical_df = subset(categorical_df, 
                        select = !colnames(categorical_df) %in% 
                          c("on_thyroxine", "sick", "query_hypothyroid", "query_hyperthyroid",
                            "tumor", "psych", "TSH_measured", "T3_measured", "TT4_measured",
                            "T4U_measured", "FTI_measured"))

# Además se remueven variables de medición para evitar ruido
imputed_allhyper = subset(imputed_allhyper, 
                        select = !colnames(imputed_allhyper) %in% 
                          c("on_thyroxine", "sick", "query_hypothyroid", "query_hyperthyroid",
                            "tumor", "psych", "TSH_measured", "T3_measured", "TT4_measured",
                            "T4U_measured", "FTI_measured"))

## VARIABLES NUMÉRICAS ##

# Se transforma la columna "age" en categorías
categorical_df$age = cut(categorical_df$age, breaks = c(-Inf, 29, 59, Inf), labels = 
                           c("Adulto Joven", "Adulto", "Mayor de Edad"))
# Lista de columnas a transformar
clin_vars = c("TSH", "T3", "TT4", "FTI", "T4U")

# Se aplica la transformación a las columnas clínicas
for (var in clin_vars) {
  # Se Calculan los cuartiles
  quartiles = quantile(categorical_df[[var]], probs = c(0, 0.25, 0.5, 0.75, 1))
  
  # Se crean las categorías basadas en los cuartiles
  categorical_df[[var]] <- cut(categorical_df[[var]], breaks = quartiles, labels = c("Bajo", "Medio-Bajo", "Medio-Alto", "Alto"), include.lowest = TRUE)
}


## VARIABLES BOOLEANAS ##

# Lista de columnas booleanas
bool_vars = c("sex", "hyperthiroid")

# se definen las etiquetas para las categorías de cada variable
bool_labels = list("sex" = c("Mujer", "Hombre"),
                   "hyperthiroid" = c("Sin Hipertiroidismo", "Con Hipertiroidismo"))

# Para las otras variables, se utiliza "No[Variable]" y "[Variable]"
for (var in bool_vars) {
  if (!(var %in% names(bool_labels))) {
    bool_labels[[var]] = c(paste0("No_", var), var)
  }
}

# Aplicar la transformación a las columnas booleanas
for (var in bool_vars) {
  categorical_df[[var]] = ifelse(categorical_df[[var]] == 0, bool_labels[[var]][1], bool_labels[[var]][2])
}

# Se escribe el DF
write.csv(categorical_df, "data/categorical_imputed_allhyper.csv", row.names = FALSE)


#### CREACIÓN ÁRBOL ####

# Se definen la variable objetivo y las variables predictoras
target = "hyperthiroid"
predictors = setdiff(names(categorical_df), target)


### MODELO NUMERICO
imputed_allhyper$hyperthiroid <- as.factor(imputed_allhyper$hyperthiroid)
imputed_allhyper <- as.data.frame(imputed_allhyper)

# Define the target and predictors
target <- "hyperthiroid"
predictors <- setdiff(names(imputed_allhyper), target)

# Train the model
model <- C5.0(imputed_allhyper[, predictors], as.factor(imputed_allhyper[, target]))


