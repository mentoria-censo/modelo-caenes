if(!require(readxl)) install.packages("readxl") else require(readxl)
if(!require(dplyr)) install.packages("dplyr") else require(dplyr)
if(!require(tidyverse)) install.packages("tidyverse") else require(tidyverse)
library(glue)
if(!require(rlang)) install.packages("rlang") else require(rlang)
if(!require(ggplot2)) install.packages("ggplot2") else require(ggplot2)
if(!require(readr)) install.packages("readr") else require(readr)
if(!require(feather)) install.packages("feather") else require(feather)
if(!require(tm)) install.packages("tm") else require(tm)
if(!require(janitor)) install.packages("janitor") else require(janitor)
if(!require(writexl)) install.packages("writexl") else require(writexl)




# 001. Funciones ----------------------------------------------------------
source('script/etl/carga_y_procesamiento/funciones_utiles.R')
crear_directorios()
eval(parse('script/etl/carga_y_procesamiento/carga_y_procesamiento.R', encoding="UTF-8"))
source("script/etl/modelamiento/helpers_training.R", encoding = "utf-8")
ciuo = readxl::read_excel('data/input/brutas/ciuo-08-cl-no-modificar.xls') %>%
  rename(aeciuo_2=...2, subgrupo=...3, grupo=...4, glosa=...5) %>%
  filter( !is.na(aeciuo_2) &  is.na(subgrupo) & is.na(grupo)) %>%
  select("aeciuo_2","glosa") %>%
  mutate(aeciuo_2=as.character(aeciuo_2))


preds = readxl::read_xlsx('/home/compartido/data_auxiliar/02_codificados/epf_2_0_ta_codificados_ciuo_sm80.xlsx') %>% procesar_texto_dataframe(c(oficio, tareas, predice)) 
preds %>% names
sm80 = preds %>% group_by(codigo_cod, predice_proc) %>% summarise(n = n()) %>% 
  mutate(prop = n / sum(n)) %>% ungroup() %>%  complete(predice_proc, codigo_cod, fill= list(n=0, prop=0) ) %>% 
  filter(predice_proc == 'si') %>%
  select(-n, -predice_proc) %>% 
  merge((preds %>% count(codigo_cod)), by='codigo_cod') %>% 
  arrange(prop %>% desc) %>% merge(ciuo, by.x= 'codigo_cod', by.y = 'aeciuo_2') %>% rename(prop80=prop, n80 = n)



preds = readxl::read_xlsx('/home/compartido/data_auxiliar/02_codificados/epf_2_0_ta_codificados_ciuo_sm81.xlsx') %>% procesar_texto_dataframe(predice) 

sm81 = preds %>% group_by(codigo_cod, predice_proc) %>% summarise(n = n()) %>% 
  mutate(prop = n / sum(n)) %>% ungroup() %>%  complete(predice_proc, codigo_cod, fill= list(n=0, prop=0) ) %>% 
  filter(predice_proc == 'si') %>%
  select(-n, -predice_proc) %>% 
  merge((preds %>% count(codigo_cod)), by='codigo_cod') %>% 
  arrange(prop %>% desc) %>% merge(ciuo, by.x= 'codigo_cod', by.y = 'aeciuo_2') %>% rename(prop81=prop, n81 = n)


preds = readxl::read_xlsx('/home/compartido/data_auxiliar/02_codificados/epf_2_0_ta_codificados_ciuo_sm82.xlsx') %>% procesar_texto_dataframe(predice) 

sm82 = preds %>% group_by(codigo_cod, predice_proc) %>% summarise(n = n()) %>% 
  mutate(prop = n / sum(n)) %>% ungroup() %>%  complete(predice_proc, codigo_cod, fill= list(n=0, prop=0) ) %>% 
  filter(predice_proc == 'si') %>%
  select(-n, -predice_proc) %>% 
  merge((preds %>% count(codigo_cod)), by='codigo_cod') %>% 
  arrange(prop %>% desc) %>% merge(ciuo, by.x= 'codigo_cod', by.y = 'aeciuo_2') %>% rename(prop82=prop, n82 = n)

preds <- sm80 %>% select(-glosa) %>% full_join(sm81, by = c("codigo_cod"))
preds <- preds %>% select(-glosa) %>% full_join(sm82, by = c("codigo_cod"))



writexl::write_xlsx(preds, glue('{ruta_minuta}/resumen_por_codigo_mb.xlsx') )




get_tablas_ciuo = function(ruta){
  tabla = read_excel(ruta) %>%  janitor::clean_names() %>% procesar_texto_dataframe(c(oficio, tareas, predice)) %>% 
    select(predice_proc, submuestra) %>% filter(submuestra >= 80)
}

procesar_texto_columna <- function(columna, quitar_digitos, stopwords_especificas= F) {
  'Función procesa el texto de una columna específica. Actualmente hace lo siguiente:
  - remueve stopwords.
  - pasa texto a minúsculas.
  - reemplaza carácteres especiales (acentos y ñ) por sus contrapartes sin tildes
    (ojo: por lo tanto ñ queda como n.)
  - elimina puntuación.
  - elimina dobles espacios y espacios antes y después del string
  - Opcionalmente se pueden eliminar dígitos.
  - Opcionalmente se puede ingresar una lista específica de stopwords.'
  if(is.vector(stopwords_especificas)){
    stp = stopwords_especificas
  } else{
    
    stp = tm::stopwords('es')
    stp = stp[stp!= 'no']    
    
  }
  
  
  stopwords_regex = paste(stp, collapse = '\\b|\\b')
  stopwords_regex = glue('\\b{stopwords_regex}\\b')
  
  columna %>%
    tolower() %>% 
    str_remove_all(stopwords_regex) %>%
    str_remove_all("[[:punct:]]") %>% 
    {if(quitar_digitos) str_remove_all(., "\\d+" ) else .} %>% 
    str_squish() 
  
}


procesar_texto_dataframe <- function(df, columns_list, sufijo = '_proc', quitar_digitos = F,
                                     stopwords_especificas = NULL) {
  'Función procesa el texto de las columnas especificadas en column_list,
  utilizando la función procesar_texto_columna.
  Las columnas con texto procesado, tendrán el sufijo "_proc", lo que permite comparar
  el texto original con el texto procesado.'
  
  df_procesado = df %>% mutate(across( {{columns_list}}, ~procesar_texto_columna(.x, quitar_digitos = quitar_digitos,
                                                                                 stopwords_especificas = stopwords_especificas), .names = "{.col}{sufijo}"))
}


preds_total = list.files('/home/compartido/data_auxiliar/02_codificados/', pattern = 'ciuo', full.names = T) %>%
  map_dfr(get_tablas_ciuo ) %>% mutate(predice_proc = if_else(predice_proc == 1, 'si', predice_proc)) %>% 
  filter(predice_proc != '66')


tabla_resumen = preds_total %>% group_by(submuestra, predice_proc) %>% summarise(num_observaciones = n()) %>%
  arrange(submuestra, predice_proc %>% desc, .groups = 'drop') %>% group_by(submuestra) %>%
  mutate(proporcion_total_sm =glue( '{((num_observaciones / sum(num_observaciones))*100) %>% round(2) %>% as.character}%')) %>% rename(`¿predicción correcta?`=predice_proc)

# Gráfico -----------------------------------------------------------------


tibble(categoria = c( "Automática CIUO"), 
       "Marcha blanca I" = c( 0.656),
       "Marcha blanca II" = c(0.729),
       "Marcha blanca III" = c(0.781)) %>%
  pivot_longer(cols = c(-categoria)) %>%
  mutate(value = value*100,
         name = factor(name, levels = c("Marcha blanca I", "Marcha blanca II", "Marcha blanca III")),
         categoria = factor(categoria, levels = c("Automática CIUO"))) %>%
  ggplot(aes(x = categoria, y = value, fill = name)) +
  geom_col(position = "dodge") +
  geom_text(aes(label=paste(round(value, 2), "%"), color = after_scale(prismatic::best_contrast(fill, c("white", "black")))), position=position_dodge(width=0.9), vjust=1.5) +
  scale_y_continuous(breaks = c(seq(0, 100, 10)), labels = paste(seq(0, 100, 10), "%")) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(title = "Porcentaje de codificación automatizada", subtitle = "Clasificador automático CIUO",
       y = "", x = "", fill = "Submuestras \ncodificadas")

resumen = read_excel('data/output/presentacion_cierre2021/resumen_por_codigo_mb.xlsx')
