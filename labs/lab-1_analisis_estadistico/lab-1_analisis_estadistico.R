# Minería de datos
# Profesor: Max Chacón
# Alumnos: Matías Escudero, Joaquín Macías
# Doctorado en Ciencias de la Ingeniería con Mención en Informática - USACH

#### LIBRERÍAS ####

library(readr)
library(tidyverse)
library(xtable)
#library(dlookr)
library(GGally)
library(RColorBrewer)
library(gridExtra)

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

#### Análisis Gráfico ####

## VARIABLES NUMÉRICAS

# En primera instancia, se limpian los valores atípicos de edad debido a su naturaleza
numeric_vars = numeric_vars[(numeric_vars$age < 110),]

# Se eliminan valores atípicos de la variable TSH
numeric_vars = numeric_vars[(numeric_vars$TSH < 100),]

# Se genera una lista para almacenar los gráficos individuales
plots_list = list()

for (var in colnames(numeric_vars)) {
  p = ggplot(numeric_vars, aes_string(x = "1", y = var, fill = "1")) +
    geom_violin(show.legend = FALSE) +
    scale_fill_gradient(low = "#66C2A5", high = "#FC8D62") +
    labs(title = var,
         x = "",
         y = "Valores") +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          plot.margin = margin(5, 5, 5, 5),
          plot.title = element_text(size = 10, hjust = 0.5),
          legend.position = "none")
  
  plots_list[[var]] = p
}

# Se juntan los gráfico de violín en una cuadrícula
grid.arrange(grobs = plots_list, ncol = 2)


# Se terminan de eliminar outliers a partir de la visualización de los gráficos
numeric_vars = numeric_vars[(numeric_vars$T3 < 10),]
numeric_vars = numeric_vars[(numeric_vars$TT4 < 400),]
numeric_vars = numeric_vars[(numeric_vars$FTI < 300),]

## VARIABLES CATEGÓRICAS

# Se reestructura el dataframe de variables categoricas
categorical_vars_long = categorical_vars %>% 
  mutate(id = row_number()) %>% 
  gather(key = "variable", value = "value", -id)

# Se generar gráficos de barra agrupados utilizando facet_wrap
bar_plots = ggplot(categorical_vars_long, aes(x = value)) +
  geom_bar(aes(fill = value), show.legend = FALSE) +
  facet_wrap(~ variable, scales = "free", nrow = 1) +
  labs(x = "",
       y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, face = "bold"))

## Se imprimen para guardar la imagen
print(bar_plots)


#### ANÁLISIS CORRELACIÓN ####

# Se eliminar filas con na's para evitar errores

numeric_vars_sin_na <- numeric_vars[complete.cases(numeric_vars), ]

# Se genera un pair plot con histogramas, scatter plots y coef de correlación de pearson
ggpairs(numeric_vars_sin_na,
        lower = list(continuous = wrap("points", alpha = 0.5)),
        upper = list(continuous = wrap("cor", size = 6, hjust = 0.5, vjust = 0.5)),
        diag = list(continuous = wrap("barDiag", bins = 30)))
