
# 000. Cargar librerias ---------------------------------------------------
library(tidyverse)
library(glue)
library(writexl)
if(!require(readxl)) install.packages("readxl") else require(readxl)

# 001. Funciones ----------------------------------------------------------
source('script/etl/carga_y_procesamiento/funciones_utiles.R')
crear_directorios()
eval(parse('script/etl/carga_y_procesamiento/carga_y_procesamiento.R', encoding="UTF-8"))
source("script/etl/modelamiento/helpers_training.R", encoding = "utf-8")



# 002. Cargar bases -------------------------------------------------------
viii_seleccion = read_excel('data/input/codificadas_entrenamiento/viii_epf.xlsx', guess_max = 1800) %>% 
  rename( "codigo_nuevo" = "Código Nuevo" ,
          "conformidad" = 'Conformidad del registro ciuo 08.cl') %>% 
  transform(codigo_nuevo = as.numeric(codigo_nuevo)) %>%
  mutate(ciuo_08cl_rev = case_when(conformidad == "CONFORME" ~ ciuo_08cl,
                              conformidad == "NO CONFORME" ~ codigo_nuevo))  %>%
  select(folio, n_linea, ciuo_08cl_rev)

viii_completa = read_excel("data/input/codificadas_entrenamiento/180413_TRASPASO_CIUO_COMPLETO_FINAL.xlsx") %>%
  janitor::clean_names() %>%
  select( submuestra = sub_muestra, folio, n_linea, oficio, tareas, ciuo_08cl)  %>%
  mutate( fuente = "completa")
  
  
  

# 003. Join ---------------------------------------------------------------
viii_final = full_join(viii_seleccion, viii_completa, by=c("folio", "n_linea")) %>%
  mutate(aeciuo_2 = case_when(ciuo_08cl_rev %>% is.na ~ ciuo_08cl,
                              T ~ ciuo_08cl_rev))

