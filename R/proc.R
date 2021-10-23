
# Procesamiento práctico 10: Regresión logística --------------------------


# 1. Cargar librerías -----------------------------------------------------

pacman::p_load(tidyverse,
               haven, 
               dplyr,
               car,
               sjmisc,
               sjlabelled,
               forcats)

# 2. Cargar datos ---------------------------------------------------------

data <- read_dta("https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/stata_esi/2020/esi-2020---personas.dta?sfvrsn=7a4b0e2b_4&download=true")

# 3. Transformaciones -----------------------------------------------------


## a) Seleccionar variables ----------------------------------------------


data_p <- data %>% 
  select(ing_t_t,
         edad,
         sexo,
         est_conyugal,
         ciuo08 = b1,
         fact_cal_esi)

## b) Recodificación -----------------------------------------------------

data_p <- data_p %>% 
  mutate_at(vars(sexo, est_conyugal, ciuo08), ~(forcats::as_factor(.))) %>% 
  mutate(est_conyugal = car::recode(.$est_conyugal,
                                    recodes = c("c('Casado/a', 'Soltero/a', 'Viudo/a', 'Separado/a de hecho o anulado/a', 'Divorciado/a') = 'Sin pareja';
                                                c('Conviviente', 'Soltero/a') = 'Con pareja';
                                                c('No aplica', 'No sabe', 'No responde') = NA")),
         ing_medio = ifelse(ing_t_t >= mean(ing_t_t), 1, 0)) %>% 
  na.omit(.)



## c) Etiquetado ---------------------------------------------------------

data_p$ing_medio <- set_label(data_p$ing_medio, '¿Mayor que el ingreso medio?')

# 4. Exportar datos -------------------------------------------------------

saveRDS(data_p, 'input/data/data_proc.rds')
