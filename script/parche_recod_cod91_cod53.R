"Se integró a script 5_integracion_bases_intermedias.R"

"Este script es un parche que se tuvo que correr por recodificaciones
en los codigos 91 y 53 desde la sm01 a la sm08"




# 000. Librerías ----------------------------------------------------------


if(!require(feather)) install.packages("feather") else require(feather)
if(!require(tidyverse)) install.packages("tidyverse") else require(tidyverse)
library(glue); library(tidyverse)
if(!require(haven)) install.packages("haven") else require(haven)
if(!require(readxl)) install.packages("readxl") else require(readxl)



# 001. Funciones ----------------------------------------------------------


recodificacion_cod91_cod53 <- function(submuestra) {
  
  if(submuestra<10){
    sm_str = glue('0{submuestra}')
    
  } else{
    sm_str = submuestra %>% as.character()
  }
  
  df_orig <- readxl::read_excel(glue("{ruta}/epf_2_0_ta_codificados_ciuo_sm{sm_str}.xlsx"))
  
  df_recodificaciones <- readxl::read_excel(glue("{ruta}/01_intermedias_ciuo/codigos_91_53.xlsx")) %>% 
    select(interview_id,interview_key,submuestra,nro_rph,codigo_cod,codigo_final)
  
  df_final <- left_join(df_orig, df_recodificaciones, by=c("interview_id","interview_key","submuestra","nro_rph")) %>% 
    mutate(CODIGO = if_else(!is.na(codigo_final),as.character(codigo_final),as.character(CODIGO))) %>% 
    select(-codigo_cod,-codigo_final)
  
  return(df_final)
}


# 002. Rutas --------------------------------------------------------------


#Aquí se debe escribir la ruta a los archivos codificados de CIUO. 
ruta <- "C:/Users/ajriverosp/Instituto Nacional de Estadisticas/SD Estadísticas Socioeconómicas - 07 Procesamiento/data_auxiliar/02_codificados"



# 003. Recodificaciones ---------------------------------------------------

for (i in 1:8) {
  recodificacion_cod91_cod53(submuestra=i)  %>% 
    writexl::write_xlsx(glue('{ruta}/epf_2_0_ta_codificados_ciuo_sm0{i}.xlsx'))
    
}
