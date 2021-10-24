
# 10-clase: Regresión logística -------------------------------------------


# 1. Cargar librerías -----------------------------------------------------

pacman::p_load(sjPlot, 
               tidyverse, 
               srvyr,
               survey,
               magrittr,
               remotes)

remotes::install_github("leifeld/texreg", force = T)

options(scipen = 999)

# 2. Cargar datos ---------------------------------------------------------

datos <- readRDS("input/data/data_proc.rds")


# 3. Explorar datos -------------------------------------------------------

names(datos)

head(datos)

sjPlot::view_df(datos,
                encoding = "UTF-8")

modelo0_sin <- glm(ing_medio ~ 1,
                  data = datos,
                  family = 'binomial')

summary(modelo0_sin)

modelo0 <- glm(ing_medio ~ 1,
              data = datos, 
              weights = fact_cal_esi,
              family = 'binomial')

summary(modelo0)


summary(modelo0);summary(modelo0_sin)

modelo1 <- glm(ing_medio ~ edad,
              data = datos, 
              weights = fact_cal_esi,
              family = 'binomial')

summary(modelo1)

modelo2_sin <- glm(ing_medio ~ sexo,
                  data = datos, 
                  weights = fact_cal_esi,
                  family = 'binomial')

summary(modelo2_sin)

modelo2 <- glm(ing_medio ~ sexo,
              data = datos, 
              weights = fact_cal_esi,
              family = 'binomial')

summary(modelo2)

modelo3 <- glm(ing_medio ~ ciuo08,
              data = datos, 
              weights = fact_cal_esi,
              family = 'binomial')

summary(modelo3)

modelo4 <- glm(ing_medio ~ est_conyugal,
              data = datos, 
              weights = fact_cal_esi,
              family = 'binomial')

summary(modelo4)

modelo5 <- glm(ing_medio ~ edad + sexo + ciuo08 + est_conyugal,
              data = datos, 
              weights = fact_cal_esi,
              family = 'binomial')

summary(modelo5)

esi_design <- as_survey_design(datos, 
                               ids = 1, 
                               weights = fact_cal_esi)

modelo5_survey <- svyglm(ing_medio ~ edad + sexo + ciuo08 + est_conyugal,
                         family = 'binomial',
                         design = esi_design)

summary(modelo5_survey)

modelo5_survey_q <- svyglm(ing_medio ~ edad + sexo + ciuo08 + est_conyugal,
                         family = 'quasibinomial',
                         design = esi_design)

summary(modelo5_survey);summary(modelo5_survey_q)

str(modelo5)



modelo5_survey$coefficients
modelo5_survey$coefficients[2]
modelo5_survey$coefficients["edad"]

str(summary(modelo5_survey))

summary(modelo5_survey)$deviance
summary(modelo5_survey)$aic

modelo5_survey$fitted.values

get_model_data(modelo5_survey, 
               type = "pred")

get_model_data(modelo5_survey, 
               type = "pred", 
               terms = "sexo")

print <- broom::augment(modelo5_survey) 

tab_df(print, file = "output/data/modelo.doc")

head(print)

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
