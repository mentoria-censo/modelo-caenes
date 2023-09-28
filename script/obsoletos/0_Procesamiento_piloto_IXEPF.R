###=========================================================================
###=========================================================================
###                                                                      ===
###                 CODIFICACIÓN OCUPACIÓN BAJO CIUO-08:                 ===
###                      ANÁLISIS BASE PILOTO IX EPF                     ===
###                              2021-08-16                              ===
###                                                                      ===
###=========================================================================
###=========================================================================


# Introducción ------------------------------------------------------------
# El objetivo de este script es estudiar y adaptar la base del grupo tratamiento, 
# acumulada hasta la submuestra 95, para su uso en la codificación por 
# machine-learning.
# input: rph_4_epf20_estructurada_tratamiento_smXX.feather
#        rph_4_epf20_estructurada_control_smXX.feather
# output: 


# 000. Librerías y banner -------------------------------------------------

# bannerCommenter::banner("Codificación ocupación bajo CIUO-08:", 
#                         "Análisis base piloto IX EPF",
#                         as.character(Sys.Date()),
#                         emph = T,bandChar = "=")
#install.packages('feather')
#install.packages('bannerCommenter')
#install.packages('here')
#install.packages('glue')
#install.packages('tm')

library(tidyverse)
library(haven)
library(readxl)
library(dplyr); library(glue)
source('script/etl/carga_y_procesamiento/funciones_utiles.R')
eval(parse('script/etl/carga_y_procesamiento/carga_y_procesamiento.R', encoding="UTF-8"))

crear_directorios()
submuestras = 91:95
cnames = readxl::read_xls('data/input/brutas/ciuo-08-cl-no-modificar.xls', skip = 1,
                        n_max = 0) %>% names
cnames[9] = 'error'
ciuo = readxl::read_xls('data/input/brutas/ciuo-08-cl-no-modificar.xls', skip = 2,
                        col_names = cnames)
a = ciuo %>% distinct(`Subgrupo Principal`) 

df = armar_dataframe_procesado()

df %>% filter(tipo == 'tratamiento') %>% count(aeciuo_2) %>% nrow / 42
df %>% count(aeciuo_2) %>% nrow / 42
# EXISTEN 42 CLASIFICACIONES CIUO

<<<<<<< HEAD:script/0_Análisis_piloto_IXEPF.R
df %>% filter(tipo == 'tratamiento') %>% 
  count(aeciuo_2) %>% arrange(desc(n)) %>% 
  filter(n>5)  %>% nrow

df  %>% 
  count(aeciuo_2) %>% arrange(desc(n)) %>% 
  filter(n>5)  %>% nrow

oficio_t = df %>% filter(tipo == 'tratamiento') %>% select(oficio_proc)
oficio_c = df %>% filter(tipo == 'control') %>% select(oficio_proc)
cbind(oficio_t, oficio_c)
map2(oficio_t, oficio_c, left_join)
comparativa_oficio = df %>% select(oficio, oficio_proc) %>% slice(140:150)
=======
df %>% count(aeciuo_2) %>% arrange(desc(n)) %>% 
  filter(n>2 & !(aeciuo_2 %in% c(-77, NA, 999))) %>% nrow

codigos_suficientes = df %>% count(aeciuo_2)  %>% 
  filter(n>2) %>% pull(aeciuo_2) 

a = df %>% filter(aeciuo_2 %in% codigos_suficientes)

# 001. Bases control brutas ---------------------------------------------


control_bruto = cargar_control_bruto()

# 002. Bases codificadas ---------------------------------------------------------
ct = cargar_control_y_tratamiento()
control_mal_encoding = ct[[1]]
tratamiento = ct[[2]]

# 003. Arreglo variables string usando control bruto -----------------------------------------

control = merge(control_mal_encoding, control_bruto, by.x = c('folio', 'nro_rph'), 
                by.y = c('FOLIO', 'nro_rph')) %>% 
  select(-c('oficio', 'tareas')) %>% 
  rename(oficio = AE09A, tareas = AE09B)

# 004. Consolidación bases de datos ---------------------------------------

df = rbind(control, tratamiento)

# 005. Códigos especiales -------------------------------------------------

df %>% 
  filter(!(oficio== "-88" | oficio== "-99" | oficio== "-77")) -> df

# 006.  Procesamiento texto ---------------------------------------------
stopwords_regex = paste(tm::stopwords('es'), collapse = '\\b|\\b')
stopwords_regex = glue('\\b{stopwords_regex}\\b')



df = df %>% procesar_texto_dataframe(c(oficio, tareas))
>>>>>>> 238a7e2a0d95a3f516b46add4ab72d9052e4ba35:script/obsoletos/0_Procesamiento_piloto_IXEPF.R

readr::write_excel_csv2(comparativa_oficio, 'data/output/presentacion_analisis/comparativa_oficios.csv') 
# 007. VIII EPF ----------------------------------------------------------------
viii = read_excel('data/input/codificadas/viii_epf.xlsx', guess_max = 1800) %>% 
        rename( "codigo_nuevo" = "Código Nuevo" ,
                "conformidad" = 'Conformidad del registro ciuo 08.cl') %>% 
        transform(codigo_nuevo = as.numeric(codigo_nuevo)) %>%
  mutate(aeciuo_2 = case_when(conformidad == "CONFORME" ~ ciuo_08cl,
                              conformidad == "NO CONFORME" ~ codigo_nuevo))


viii = viii  %>% procesar_texto_dataframe(c(oficio, tareas))


viii2 = procesar_viii_epf()



df %>% filter(tipo == 'tratamiento') %>% count(oficio_proc) %>%
  arrange(n %>% desc) %>% slice(1:10)
df %>% filter(tipo == 'control') %>% count(oficio_proc) %>%
  arrange(n %>% desc) %>% slice(1:10)



# nnn. Código legacy  -----------------------------------------------------
# control %>% slice(str_which(control[['AE09B']], pattern = 'Ñ', negate = F)) %>% count(sm)
# folios_unicos = df_c91 %>% select(folio) %>% unique
# control_91 = control %>% filter(FOLIO %in% df_c91$folio )
# control_91$nro_rph == df_c91$nro_rph




#Las bases de control traían problemas de encoding que no tenían las bases de tratamiento.
#La siguiente línea me permite inferir qué tipo de encodinf puede tener
#   stringi::stri_enc_detect(df_c91$tareas, filter_angle_brackets = FALSE)


  

