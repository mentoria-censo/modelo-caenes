
# Definir resultados a utilizar -------------------------------------------
##Puede ser "FALSE" de una entrada (oficio) o "TRUE" de dos entradas (oficio y tareas)
doble_entrada = F


# 000. Funciones ----------------------------------------------------------
library(tidyverse)
library(glue)
library(writexl)
library(janitor)
source('script/etl/carga_y_procesamiento/funciones_utiles.R')
crear_directorios()
eval(parse('script/etl/carga_y_procesamiento/carga_y_procesamiento.R', encoding="UTF-8"))
source("script/etl/modelamiento/helpers_training.R", encoding = "utf-8")

get_prop_si = function(prop, filter_sm = NULL){
  df_tail = df %>% 
    {if(!filter_sm %>% is.null) filter(., submuestra == filter_sm) else .} %>%
    slice_tail(prop = prop)
  n_si = df_tail %>% count(predice2) %>% filter(predice2 == 1) %>% pull(n)
  print(df_tail %>% count(predice2))
  n_si / df_tail %>% nrow
  
}


cargar_datos = function(dir) {
  tabla_final = readxl:::read_excel(dir) %>% 
    clean_names() %>% 
    #print_pipe_message %>% 
    mutate(cod_rev_et = as.numeric(cod_rev_et)) %>% 
    rename(codigo_cod = matches("cod[^cod_rev_et][^codificadores]")) %>% 
    mutate(codigo_cod = as.character(codigo_cod)) %>% 
    rename(observaciones_codificadores = matches("observaciones")) 
  
}
#precodificadas %>% names


if(list.files('/home') %>% identical(character(0))) ruta_input = 'data/input/codificadas_IXEPF' else{
  ruta_input = '/home/compartido/data_auxiliar/02_codificados'
} 

if(list.files('/home') %>% identical(character(0))) ruta_output = 'data/output/codigos_maca' else{
  ruta_output = '/home/compartido/data_auxiliar/01_a_codificar/ciuo/01_intermedias'
} 

# 000. Cargar datos -------------------------------------------------------
codigos_aislados = list.files(glue('{ruta_input}/'), full.names = T, pattern = 'codificados_ciuo')  %>%
  map_dfr(cargar_datos) %>% 
  select(interview_id, submuestra, interview_key, nro_rph, oficio, tareas, observaciones_codificadores, codigo_cod) %>% 
  filter(submuestra < 80) %>% filter(codigo_cod %in% c(53, 91)) %>% arrange(oficio) %>% mutate(codigo_final = '')

codigos_aislados %>% writexl::write_xlsx('data/obsoleto/codigos_91_53.xlsx')
