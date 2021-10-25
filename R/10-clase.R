
# 10-clase: Regresión logística -------------------------------------------


# 1. Cargar librerías -----------------------------------------------------

pacman::p_load(sjPlot, 
               tidyverse, 
               srvyr,
               survey,
               magrittr,
               remotes) # Remotes una vez para descargar paquetes fuera de CRAN

remotes::install_github("leifeld/texreg", force = T)
library(texreg)

options(scipen = 999) # Evitar notación cientifica

# 2. Cargar datos ---------------------------------------------------------

datos <- readRDS("input/data/data_proc.rds")


# 3. Explorar datos -------------------------------------------------------

names(datos)

head(datos)

sjPlot::view_df(datos,
                encoding = "UTF-8")


# 4. Crear modelos --------------------------------------------------------

# Modelo nulo -------------------------------------------------------------
modelo0 <- glm(ing_medio ~ 1,
               data = datos, 
               family = binomial(link = "logit"))

summary(modelo0)

# No asustarse con los warning

# Modelo con 1 predictor --------------------------------------------------
modelo1 <- glm(ing_medio ~ edad,
               data = datos, 
               family = binomial(link = "logit"))

summary(modelo1)


# Predictores categoricos -------------------------------------------------

## Pregunta: ¿que deberiamos hacer antes de ...?
modelo2 <- glm(ing_medio ~ sexo,
               data = datos, 
               family = binomial(link = "logit"))

summary(modelo2)



# Modelo con todos los predictores ----------------------------------------

modelo3 <- glm(ing_medio ~ edad + sexo + ciuo08 + est_conyugal,
               data = datos, 
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


## Tablas ------------------------------------------------------------------

### Con tab_model() ---------------------------------------------------------

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



### Con texreg ------------------------------------------------------------

# screenreg: para visualizar modelos en la consola
# htmlreg: para visualizar modelos en formato html (renderizado)
# texreg: para visualizar modelos en PDF (LaTeX)

screenreg(modelo3, doctype = F)

screenreg(l = list(modelo3, modelo3_survey))

or <- texreg::extract(modelo3_survey) #Extraemos info del modelo 
or@coef <- exp(or@coef) #Exponenciamos los coeficientes

screenreg(l = list(modelo3_survey, or))

screenreg(l = list(modelo3_survey, or), 
          doctype = F, #No incluimos doctype
          caption = "Leyenda", #Leyenda de la tabla 
          caption.above = T, # Presentar la leyenda en la sección superior. Si = FALSE (predeterminado), la leyenda se sitúa bajo la tabla
          custom.model.names = c("Modelo 3", "Modelo 3 (OR)"), #Personalizar los títulos de la tabla 
          ci.force = c(TRUE,TRUE), #Presentar intervalos de confianza
          override.coef = list(coef(modelo3_survey), or@coef), #Sobreescribir los coeficientes a partir de los coeficientes de los modelos
          custom.note = "$^{***}$ p < 0.001; $^{**}$ p < 0.01; $^{*}$ p < 0.05 <br> Errores estándar entre paréntesis. <br> **Nota**: La significancia estadística de los coeficientes en unidades de Odds ratio está calculada en base a los valores $t$, <br> los cuales a su vez se calculan en base a $log(Odds)/SE$") #Incorporamos una nota al pie de la tabla


## Gráficos ----------------------------------------------------------------

sjPlot::plot_model(objeto_creado, 
                   ci.lvl = "", #estima el nivel de confianza 
                   title = "",  # es el título
                   show.p = T, # nos muestra los valores p
                   show.values =  T, # nos muestra los valores
                   vline.color = "") # color de la recta vertical

sjPlot::plot_model(modelo3, 
                   show.p = T,
                   show.values =  T,
                   ci.lvl = c(0.95), 
                   title = "Estimación de predictores", 
                   vline.color = "purple")