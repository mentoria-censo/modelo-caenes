
# 000.  Librerias y funciones ---------------------------------------------

if(!require(feather)) install.packages("feather") else require(feather)
if(!require(dplyr)) install.packages("dplyr") else require(dplyr)
if(!require(haven)) install.packages("haven") else require(haven)
if(!require(tidyverse)) install.packages("tidyverse") else require(tidyverse)
library(glue); library(tidyverse)
if(!require(readxl)) install.packages("readxl") else require(readxl)
if(!require(keras)) install.packages("keras") else require(keras)
if(!require(caret)) install.packages("caret") else require(caret)

source('script/etl/carga_y_procesamiento/funciones_utiles.R')
crear_directorios()
eval(parse('script/etl/carga_y_procesamiento/carga_y_procesamiento.R', encoding="UTF-8"))
source("script/etl/modelamiento/helpers_training.R", encoding = "utf-8")



# 001.  Cargar modelo -----------------------------------------------------
modelo = load_model_hdf5('data/output/modelo/modelo_ciuo_ix_epf.hdf5')



# 002. Dataframe entrenamiento y a predecir -------------------------------
ciuo = readxl::read_excel('data/input/brutas/ciuo-08-cl-no-modificar.xls') %>%
  rename(aeciuo_2=...2, subgrupo=...3, grupo=...4, glosa=...5) %>%
  filter( !is.na(aeciuo_2) &  is.na(subgrupo) & is.na(grupo)) %>%
  select("aeciuo_2","glosa") %>%
  mutate(aeciuo_2=as.character(aeciuo_2))

for (sm_exec in c(80,81,82)) {
  datos_nuevos = read_feather(glue('data/input/brutas/epf_1_1_ta_acodificar_ciuo_sm{sm_exec}.feather')) %>% 
    filter(!(oficio %in% c("-88", "-99", "-77", "", "-66"))) %>% procesar_texto_dataframe(c(oficio, tareas)) %>%
    procesamiento_extra_para_modelamiento(datos_nuevos = T)
  
  df_completo = armar_df_modelamiento_completo(datos_a_utilizar = 'todo') %>% filter((submuestra<sm_exec) | is.na(submuestra))
  
  
  # 003.  Predicción --------------------------------------------------------
  maxlen = 15
  network_inputs <- pre_process(df_completo, text_variable =  "oficio_proc", label =  aeciuo_2, type = "sequences",
                                maxlen = maxlen)
  y_train = network_inputs[['y_train']] %>% as.data.frame()
  names(y_train)  = 'codigo_int'
  
  keys = network_inputs$keys %>% inner_join(ciuo)
  
  tokenizer <-  network_inputs$tokenizer
  
  datos_a_predecir <- as.array(datos_nuevos[['oficio_proc']])
  x_test = texts_to_sequences(tokenizer, datos_a_predecir) %>% pad_sequences(padding='post', maxlen=maxlen)
  
  y_proba = modelo$predict(x_test)
  proxy_dificultad = apply(y_proba, 1, function(z) {
    z <- sort(z, decreasing = TRUE)[1:2]
    z[1] / z[2]
  })
  y_pred_vector = y_proba %>% Rfast::rowMaxs() - 1
  
  y_pred = y_pred_vector %>% matrix(ncol=1) %>% data.frame %>% setNames('codigo_int')
  y_pred_glosas = y_pred %>% left_join(keys)
  
  datos_nuevos = datos_nuevos %>% mutate(
    glosa_prediccion = y_pred_glosas$glosa,
    prediccion_modelo = y_pred_glosas$aeciuo_2,
    dificultad = proxy_dificultad) %>%
    select(submuestra, interview_id, nro_rph, oficio_proc, tareas_proc, dificultad)
  
  
  }





# preds = readxl::read_xlsx('data/input/codificadas_IXEPF/epf_2_0_ta_codificados_ciuo_sm80.xlsx') %>% procesar_texto_dataframe(c(oficio, tareas)) %>%
#   select(interview_id, nro_rph, submuestra, oficio_proc, tareas_proc, prediccion_modelo, codigo_cod)
# 
