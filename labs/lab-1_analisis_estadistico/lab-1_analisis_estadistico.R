# Minería de datos
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

# Se imprimen para guardar la imagen
print(bar_plots)

# ESTO NO SE INCLUYÓ EN EL INFORME POR SOBREPASAR MÁXIMO DE PÁGINAS.

#### ANÁLISIS CORRELACIÓN ####

## Se eliminan los registros en donde la clase sea "goitre" o "T3 Toxic"

# Se eliminar filas con na's para evitar errores

numeric_vars_sin_na <- numeric_vars[complete.cases(numeric_vars), ]

# Se genera un pair plot con histogramas, scatter plots y coef de correlación de pearson
ggpairs(numeric_vars_sin_na,
        lower = list(continuous = wrap("points", alpha = 0.5)),
        upper = list(continuous = wrap("cor", size = 6, hjust = 0.5, vjust = 0.5)),
        diag = list(continuous = wrap("barDiag", bins = 30)))

# En vista de que se trabajó con DF's separados por tipos de variables, se aplican todos los filtros
# hechos anteriormente sobre el DF original.

allhyper_df$TBG = NULL
allhyper_df$TBG_measured = NULL

allhyper_df = subset(allhyper_df, 
                     age < 110 & 
                       TSH < 100 & 
                       T3 < 10 & 
                       TT4 < 400 & 
                       FTI < 300 &
                       (class_name %in% c("negative.", "hyperthyroid.")))

# Se reclasifica la variable de hipertiroidismo
allhyper_df$hyperthyroidism = ifelse(allhyper_df$class_name == "hyperthyroid.", TRUE, FALSE)

# Se elimina la variable anterior
allhyper_df$class_name = NULL

## Boxplots para variables numéricas en relación a hipertiroidismo
# Se obtienen los nombres de columnas numéricas a partir del df generado anteriormente
numeric_variables = colnames(numeric_vars)

# Cambio de formato para el DF 
hyper_long = allhyper_df %>%
  select(hyperthyroidism, one_of(numeric_variables)) %>%
  gather(key = "variable", value = "value", -hyperthyroidism)

# Se genera el gráfico
hyper_box = ggplot(hyper_long, aes(x = hyperthyroidism, y = value, group = hyperthyroidism)) +
  geom_boxplot(fill = "#66C2A5") +
  facet_wrap(~variable, scales = "free_y", ncol = 2) +
  labs(title = "Boxplots variables numéricas vs. hipertiroidismo",
       x = "Hipertiroidismo",
       y = "Valor")

print(hyper_box)

## Prueba chi cuadrado

# Crear un vector con los nombres de las variables categóricas y booleanas
vars_chi2 = c("sex", "on_thyroxine", "query_on_thyroxine", "on_antithyroid_medication", "sick", "pregnant", "thyroid_surgery", 
                "I131_treatment", "query_hypothyroid", "query_hyperthyroid", 
                "lithium", "goitre", "tumor", "hypopituitary", "psych", "TSH_measured", "T3_measured", 
                "TT4_measured", "T4U_measured", "FTI_measured")

# DF que almacenará los resultados de los p values
chi2_df = data.frame(variable = character(), p_value = numeric())

# Se calcula la prueba de chi-cuadrado para cada variable y se agregan los resultados al data.frame
for (variable in vars_chi2) {
  tabla_contingencia = table(allhyper_df[[variable]], allhyper_df$hyperthyroidism)
  chi2_test = chisq.test(tabla_contingencia)
  
  chi2_df = chi2_df %>%
    add_row(variable = variable, p_value = chi2_test$p.value)
}

# Se ordena el DF por p_value
chi2_df = chi2_df %>%
  arrange(p_value)

# Se exporta la tabla a formato LaTeX
chi2_latex = xtable(chi2_df, caption = "Valores de p-value para pruebas de independencia chi cuadrado")
print(chi2_latex, type = "latex", include.rownames = FALSE)

#### PRUEBAS DE WELCH ####

## CASO 1:
# H0: El tratamiento de litio disminuyendo la disponibilidad de T3 en pacientes 
# con este tipo de tratamiento (carbonato de litio)
# H1: El tratamiento de litio no disminuye la disponibilidad de T3 en pacientes
# con este tipo de tratamiento (carbonato de litio).

# Se separa el DF en pacientes que han recibido tratamiento de Litio y los que no
patients_on_lithium = allhyper_df[allhyper_df$lithium == TRUE, ]
patients_no_lithium = allhyper_df[allhyper_df$lithium == FALSE, ]

# Se realiza la prueba t de Welch
welch_test_case1 = t.test(patients_on_lithium$T3, patients_no_lithium$T3, alternative = "two.sided")

# Se muestran los resultados
welch_test_case1

## CASO 2:
# H0: El uso de tiroxina no afecta significativamente los niveles de TSH en pacientes.
# H1: El uso de tiroxina afecta significativamente los niveles de TSH en pacientes.

# Se separa el DF en paciente que estén tomando tiroxina y en quienes no.
patients_on_thy = allhyper_df[allhyper_df$on_thyroxine == TRUE,]
patients_no_thy = allhyper_df[allhyper_df$on_thyroxine == FALSE,]

# Se realiza la prueba t de Welch
welch_test_case2 = t.test(patients_on_thy$TSH, patients_no_thy$TSH, alternative = "two.sided")

# Se muestran los resultados
welch_test_case2

#### Regresión Logística ####

# Se crea un modelo con todas las variables candidatas de acuerdo al análisis previo
full_model = glm(hyperthyroidism ~ age + FTI + T3 + T4U + TSH + TT4 + TSH_measured + T3_measured + TT4_measured + FTI_measured + query_hyperthyroid + sex,
                 data = allhyper_df, family = binomial(link = "logit"))

# Se utiliza selección de vriables stepwise
selected_model = step(full_model, direction = "both")

# Se visualizan los resultados del modelo seleccionado
summary(selected_model)
