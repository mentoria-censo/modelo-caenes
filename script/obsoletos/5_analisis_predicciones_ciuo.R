source('script/etl/carga_y_procesamiento/carga_y_procesamiento.R')
library(glue); library(tidyverse)
library(tm)

install.packages('tm')
ruta_codificados = '../../compartido/data_auxiliar/02_codificados'
ruta_minuta = 'data/output/minuta_4'
dir.create(ruta_minuta)
ciuo = readxl::read_excel('data/input/brutas/ciuo-08-cl-no-modificar.xls') %>%
  rename(aeciuo_2=...2, subgrupo=...3, grupo=...4, glosa=...5) %>%
  filter( !is.na(aeciuo_2) &  is.na(subgrupo) & is.na(grupo)) %>%
  select("aeciuo_2","glosa") %>%
  mutate(aeciuo_2=as.character(aeciuo_2))


preds = readxl::read_xlsx(glue('{ruta_codificados}/epf_2_0_ta_codificados_ciuo_sm80.xlsx')) %>% 
  procesar_texto_dataframe(predice)

resumen_total = preds %>% group_by(predice_proc) %>% summarise(n = n(), prop = n()/nrow(preds))
writexl::write_xlsx(resumen_total, glue('{ruta_minuta}/resumen_agregado.xlsx') )

tabla_resumen_x_cod = preds %>%  group_by(codigo_cod, predice_proc) %>% summarise(n = n()) %>% 
  mutate(prop = n / sum(n)) %>% ungroup() %>%  complete(predice_proc, codigo_cod, fill= list(n=0, prop=0) ) %>% 
  filter(predice_proc == 'si') %>%
  select(-n, -predice_proc) %>% 
  merge((preds %>% count(codigo_cod)), by='codigo_cod') %>% 
  arrange(prop %>% desc) %>% merge(ciuo, by.x= 'codigo_cod', by.y = 'aeciuo_2')

writexl::write_xlsx(tabla_resumen_x_cod, glue('{ruta_minuta}/resumen_por_codigo.xlsx') )



