
# 09-clase: Regresión logística -------------------------------------------


# 1. Cargar librerías -----------------------------------------------------

pacman::p_load(sjPlot, 
               tidyverse, 
               srvyr,
               survey,
               magrittr,
               remotes) # Remotes una vez para descargar paquetes fuera de CRAN

remotes::install_github("leifeld/texreg", force = T)

options(scipen = 999) # Evitar notación cientifica

# 2. Cargar datos ---------------------------------------------------------

datos <- readRDS("../input/data/data_proc.rds")


# 3. Explorar datos -------------------------------------------------------

names(datos)

head(datos)

sjPlot::view_df(datos,
                encoding = "UTF-8")


# 4. Crear modelos --------------------------------------------------------

# Modelo nulo -------------------------------------------------------------
modelo0 <- glm(ing_medio ~ 1,
              data = datos, 
              weights = fact_cal_esi,
              family = binomial(link = "logit"))

summary(modelo0)
# No asustarse con los warning

summary(modelo0);summary(modelo0_sin)


# Modelo con 1 predictor --------------------------------------------------
modelo1 <- glm(ing_medio ~ edad,
              data = datos, 
              weights = fact_cal_esi,
              family = binomial(link = "logit"))

summary(modelo1)


# Predictores categoricos -------------------------------------------------

## Pregunta: ¿que deberiamos hacer antes de ...?
modelo2 <- glm(ing_medio ~ sexo,
              data = datos, 
              weights = fact_cal_esi,
              family = binomial(link = "logit"))

summary(modelo2)



# Modelo con todos los predictores ----------------------------------------
modelo3 <- glm(ing_medio ~ edad + sexo + ciuo08 + est_conyugal,
              data = datos, 
              weights = fact_cal_esi,
              family = binomial(link = "logit"))

summary(modelo3)


# Con survey --------------------------------------------------------------
esi_design <- as_survey_design(datos, 
                               ids = 1, 
                               weights = fact_cal_esi)

modelo3_survey <- svyglm(ing_medio ~ edad + sexo + ciuo08 + est_conyugal,
                         family = binomial(link = "logit"),
                         design = esi_design)

summary(modelo3_survey)



# Extraer objetos ---------------------------------------------------------
modelo3_survey$coefficients
modelo3_survey$coefficients[2]
modelo3_survey$coefficients["edad"]

str(summary(modelo3_survey))

summary(modelo3_survey)$deviance
summary(modelo3_survey)$aic

modelo3_survey$fitted.values



# Exponenciacion ----------------------------------------------------------
#exp() exponenciar

# OR de edad
exp(modelo3_survey$coefficients["edad"]) 


# Crear OR ----------------------------------------------------------------
modelo3_survey$or <- exp(modelo3_survey$coefficients) 

#Comprobemos
modelo3_survey$or["edad"]
modelo3_survey$coefficients["edad"]


# Probabilidades ----------------------------------------------------------
#p = or / (1+or)

## Ejercicio: ¿Como se podria calcular?

# Presentacion de resultados ------------------------------------------------------------------
sjPlot::tab_model(objeto_creado, 
                  show.ci= F/T,  # este argumento muestra los intervalos de confianza
                  show.p = F/T, #Este argumento muestra los valores p
                  show.obs = F/T, # Este argumento muestra las observaciones
                  title = "Título de la tabla a crear",
                  digits = 2, # muestra la cantidad de dígitos que tednrá la tabla
                  p.style = c("numeric", "stars", "numeric_stars", "scientific", "scientific_stars"), #cómo representa el pvalue 
                  encoding = "UTF-8",  # evita errores en caracteres
                  file = "output/figures/reg1_tab.doc") # guarda lo creado automáticamente

sjPlot::tab_model(modelo0, 
                  show.ci=FALSE,
                  df.method = 'wald',
                  encoding = "UTF-8")

sjPlot::tab_model(list(modelo0, modelo1, modelo2), # los modelos estimados
                  show.ci=FALSE, # no mostrar intervalo de confianza (por defecto lo hace)
                  p.style = "stars", # asteriscos de significación estadística
                  dv.labels = c("Modelo 1", "Modelo 2", "Modelo 3"), # etiquetas de modelos o variables dep.
                  string.pred = "Predictores", string.est = "β", # nombre predictores y símbolo beta en tabla
                  encoding =  "UTF-8")

sjPlot::plot_model(objeto_creado, 
                   ci.lvl = "", #estima el nivel de confianza 
                   title = "",  # es el título
                   show.p = T, # nos muestra los valores p
                   show.values =  T, # nos muestra los valores
                   vline.color = "") # color de la recta vertical

sjPlot::plot_model(modelo5, 
                   show.p = T,
                   show.values =  T,
                   ci.lvl = c(0.95), 
                   title = "Estimación de predictores", 
                   vline.color = "purple")


# ¿Y lo de texreg?
