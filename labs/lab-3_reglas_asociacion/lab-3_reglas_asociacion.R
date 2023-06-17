# Minería de datos
# Laboratorio 2: Agrupamiento K-medias
# Profesor: Max Chacón
# Alumnos: Matías Escudero, Joaquín Macías
# Doctorado en Ciencias de la Ingeniería con Mención en Informática - USACH

#### LIBRERÍAS ####

library(readr)
library(dplyr)
library(arules)
library(arulesViz)


#### ENTRADAS ####

## Se lee el Data Frame ya procesado a partir del laboratorio anterior.

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
bool_vars = c("sex", "on_thyroxine", "query_on_thyroxine", "on_antithyroid_medication", "sick",
               "pregnant", "thyroid_surgery", "I131_treatment", "query_hypothyroid", "query_hyperthyroid", "lithium", 
               "goitre", "tumor", "hypopituitary", "psych", "TSH_measured", "T3_measured", "TT4_measured", 
               "T4U_measured", "FTI_measured", "hyperthiroid")

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


#### CREACIÓN REGLAS ####
# Se convierten los datos en una transacción

transacciones = as(categorical_df, "transactions")

# Visualizar el resumen de la transacción
summary(transacciones)


# Se generan reglas de asociación con apriori
reglas = apriori(transacciones, parameter = list(supp = 0.01, conf = 0.7))

# Visualizar el resumen de las reglas
summary(reglas)
