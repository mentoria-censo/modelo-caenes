
# 000. Librerías ----------------------------------------------------------

if(!require(feather)) install.packages("feather") else require(feather)
if(!require(tidyverse)) install.packages("tidyverse") else require(tidyverse)
library(glue); library(tidyverse)
if(!require(haven)) install.packages("haven") else require(haven)
if(!require(readxl)) install.packages("readxl") else require(readxl)







# 001. Ruta ---------------------------------------------------------------

# Si se corre localmente:
# ruta <- "C:/Users/ajriverosp/Instituto Nacional de Estadisticas/SD Estadísticas Socioeconómicas - 07 Procesamiento/data_auxiliar/02_codificados"

# Si se corre desde el servidor:
ruta <- "/home/compartido/data_auxiliar/02_codificados"


# 002. Integración tabla a codificar de codificación y equipo técn --------


codificacion = glue("{ruta}/01_intermedias_ciuo/epf_2_0_ta_codificados_ciuo_sm{sm_str}_codificacion.xlsx") %>%  
  glue %>% read_excel %>% mutate(origen = 'codificación') %>% janitor::clean_names() %>% 
  rename_with(.cols = starts_with('codigo'),
              ~str_replace(.x, 'codigo.*', 'CODIGO')) %>% select(where(~ !all(is.na(.)))) %>%
  mutate(CODIGO = if_else(CODIGO=="Recuperar", "-66",CODIGO)) %>% 
  mutate(CODIGO = CODIGO %>% as.numeric) %>% rename(PREDICE = predice)


tecnica = glue("{ruta}/01_intermedias_ciuo/epf_2_0_ta_codificados_ciuo_sm{sm_str}_tecnica.xlsx") %>% 
  glue %>% read_excel  %>% mutate(CODIGO = if_else(predice_bien == 1, prediccion_modelo, codigo_final %>% as.character)) %>% 
  select(-codigo_final, -predice_bien, -persona_codifica) %>% mutate(origen = 'técnica') %>% 
  mutate(CODIGO = as.numeric(CODIGO))

  
  tabla_final = codificacion %>% bind_rows(tecnica) %>% mutate(CODIGO = replace_na(CODIGO, -77)) %>% 
    mutate(PREDICE = case_when(is.na(PREDICE) & CODIGO!=prediccion_modelo ~ "no",
                               is.na(PREDICE) & CODIGO==prediccion_modelo ~ "si",
                               TRUE ~ PREDICE)) 



if (sm_exec<=8) {
  
  df_recodificaciones <- readxl::read_excel(glue("{ruta}/01_intermedias_ciuo/codigos_91_53.xlsx")) %>% 
    select(interview_id,interview_key,submuestra,nro_rph,codigo_cod,codigo_final)
  
  tabla_final <- left_join(tabla_final, df_recodificaciones, by=c("interview_id","interview_key","submuestra","nro_rph")) %>% 
    mutate(CODIGO = if_else(!is.na(codigo_final),as.character(codigo_final),as.character(CODIGO))) %>% 
    select(-codigo_cod,-codigo_final)
  
} 


# 003. Exportar tabla -----------------------------------------------------


tabla_final %>% writexl::write_xlsx('~/procesamiento_ix_epf/data/data_auxiliar/ciuo/epf_2_0_ta_codificados_ciuo_sm{sm_str}.xlsx' %>% glue)
  


# tabla_final %>% filter(origen=="técnica" & PREDICE=="no") %>% view
# table(tabla_final$PREDICE)

