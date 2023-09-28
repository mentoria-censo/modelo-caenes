
# Definir resultados a utilizar -------------------------------------------
##Puede ser "FALSE" de una entrada (oficio) o "TRUE" de dos entradas (oficio y tareas)


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

df %>% filter(submuestra == 3) %>% slice_tail(prop = .05) %>% select(oficio, tareas, pred_nueva, codigo_cod, predice2) %>% view
df %>% names
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
  ruta_output = '/home/compartido/data_auxiliar/01_a_codificar/ciuo/'
} 

# 000. Cargar datos -------------------------------------------------------
codificados_maca = list.files(glue('{ruta_input}/'), full.names = T, pattern = 'codificados_ciuo')  %>%
  map_dfr(cargar_datos) %>% 
  select(interview_id, submuestra, predice, nro_rph, oficio, tareas, observaciones_codificadores, codigo_cod)
codificados_maca %>% count(submuestra)


precodificadas = list.files(glue('{ruta_output}'), full.names = T, pattern = 'acodificar_ciuo', recursive = T) %>%
  map_dfr(readxl::read_excel) %>%
  select(interview_id, nro_rph, pred_nueva=prediccion_modelo, submuestra)
precodificadas %>% count(submuestra)


df = precodificadas %>% left_join(codificados_maca %>% select(-submuestra), by=c('interview_id', 'nro_rph')) %>% 
  mutate(predice = predice %>% str_to_lower,
         predice2 = if_else(pred_nueva==codigo_cod,1,0),
         submuestra = as.numeric(submuestra),
         codigo_cod = as.numeric(codigo_cod)) %>%
  filter(!submuestra %in% c('11', '12') & !is.na(codigo_cod) & !codigo_cod < 0  ) %>% 
  mutate(submuestra2 = case_when(submuestra==80 ~ 1,
                                 submuestra==81 ~ 2,
                                 submuestra==82 ~ 3,
                                 submuestra==1  ~ 4,
                                 submuestra==2  ~ 5,
                                 submuestra==3  ~ 6,
                                 submuestra==4  ~ 7,
                                 submuestra==5  ~ 8,
                                 submuestra==6  ~ 9)) %>% 
  arrange(submuestra2) %>%
  mutate(submuestra = as.character(submuestra) %>% as.factor %>% forcats::fct_inorder() ) %>%
  select(-submuestra2)
  

pred_x_sm = df %>% group_by(submuestra) %>% summarise(predice = sum(predice2)/n()) 


p = ggplot(pred_x_sm, aes(x=submuestra, y=predice, group = 1)) +
  scale_y_continuous(labels = scales::percent, limits = c(0,1))+
  geom_line(aes(color = 'Modelo CIUO') ) + 
  geom_hline(aes(yintercept =.76, color = 'Línea base'), size = .3) +
  scale_colour_manual(values = c('red', 'blue'))+
  labs(title = 'Precisión de la codificación de oficios por redes neuronales',
       color = '') 
  

ggsave('graficos/rendimiento_modelo.png', plot = p, width = 8)
?geom_line
df %>% filter(submuestra == 3) %>% view
df %>% names
df %>% filter(submuestra=="2" & is.na(codigo_cod)) %>% nrow
df %>% filter(submuestra == 2 ) %>% count(predice2) %>% summarise(sum = sum(n))
df %>% filter(pred_nueva == '01')
df %>% count(codigo_cod) %>% arrange(desc(n)) %>% view
#get_prop_si(1, filter_sm = 82)

# 001. Puebas proporcion superior -----------------------------------------
mat <- matrix(0, nrow = 12, ncol = 13, dimnames = list(c("0,05","0,1","0,15","0,2","0,25","0,30","0,35","0,4","0,45","0,50","0,55","0,6" ),
                                                     c("80","81","82", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10")))
for(sm in c(80:82,1:10)) {
  if(sm %in% 80:82) {
  j=sm-79 
  } else {
    j=sm+3
  }
  i=0
  for(prop in c(.05, .1, .15, .2, .25, .3, .35, .4, .45, .5, .55, .6)){
    i=i+1
    print(sm)
    
    
    mat
    mat[i,j]=get_prop_si(prop, filter_sm = sm) 
  }
}


openxlsx::write.xlsx(data.frame(Prop = row.names(mat), mat), glue("data/output/analisis_punto_corte/Prop_correctos.xlsx"), sheetName="Prop_correctos", overwrite = TRUE) 


# 002. Pérdida para distintos puntos de corte -----------------------------
mat <- matrix(0, nrow = 13, ncol = 5, dimnames = list(c("0,05","0,1","0,15","0,2","0,25","0,30","0,35","0,4","0,45","0,5","0,55","0,6","Total_sm"), c("80","81","82","01","02")))
T_obs<-df %>% filter(submuestra==80) %>%  count(submuestra) %>% 
  bind_rows(df %>% filter(submuestra==81) %>%  count(submuestra)) %>% 
  bind_rows(df %>% filter(submuestra==82) %>%  count(submuestra)) %>% 
  bind_rows(df %>% filter(submuestra==1) %>%  count(submuestra)) %>% 
  bind_rows(df %>% filter(submuestra==2) %>%  count(submuestra)) 

for(sm in c(80,81,82,1,2)) {
  if(sm %in% c(80,81,82)) {
    j=sm-79 
  } else {
    j=sm+3
  }
  i=0
  for(prop in c(.05, .1, .15, .2, .25, .3, .35, .4, .45, .5, .55, .6)){
    i=i+1
    mat[i,j]=(1-get_prop_si(prop, filter_sm = sm))*(as.integer(T_obs[j,2])*prop)
  }
  mat[13,j]=as.integer(T_obs[j,2])
}

mat<-round(mat,0)
openxlsx::write.xlsx(data.frame(Prop = row.names(mat), mat), glue("data/output/analisis_punto_corte/N_incorrectos.xlsx"), sheetName="N_perdidas", overwrite = TRUE) 



# 003. SGP con mayor presencia por submuestra -----------------------------

sgp <- df %>%group_by(submuestra,codigo_cod,predice2) %>% count(predice2) %>% pivot_wider(names_from = submuestra, values_from = n) 
openxlsx::write.xlsx(sgp, glue("data/output/analisis_punto_corte/analisis_sgp_submuestra.xlsx"), sheetName="N_sgp_predice", overwrite = TRUE) 



# 004. Analisis SGP 52 -----------------------------------------------------
vendedores <- df %>% 
  procesar_texto_dataframe(c(oficio, tareas)) %>%
  filter(pred_nueva==52) %>%
  filter(str_detect(oficio_proc, "\\bvende") & !str_detect(oficio_proc, "(seguros?)|(ambulante)|(callejero)|(coler(o|a))"))

table(vendedores$submuestra)
table(vendedores$predice2, vendedores$submuestra)

# 005. Punto de corte + extr. vendedores ----------------------------------
df <- df %>% 
  anti_join(vendedores, by=c("interview_id","nro_rph"))

mat <- matrix(0, nrow = 12, ncol = 5, dimnames = list(c("0,05","0,1","0,15","0,2","0,25","0,30","0,35","0,4","0,45","0,5","0,55","0,6"),
                                                      c("80","81","82","01","02")))
for(sm in c(80,81,82,1,2)) {
  if(sm %in% c(80,81,82)) {
    j=sm-79 
  } else {
    j=sm+3
  }
  i=0
  for(prop in c(.05, .1, .15, .2, .25, .3, .35, .4, .45, .5, .55, .6)){
    i=i+1
    mat[i,j]=get_prop_si(prop, filter_sm = sm) 
    
  }
}
get_prop_si(.05, filter_sm = '2') 
openxlsx::write.xlsx(data.frame(Prop = row.names(mat), mat), glue("data/output/analisis_punto_corte/Prop_correctos_sin_sgp52.xlsx"), sheetName="Prop_correctos", overwrite = TRUE) 




# MATRIZ ------------------------------------------------------------------


mat <- matrix(0, nrow = 13, ncol = 5, dimnames = list(c("0,05","0,1","0,15","0,2","0,25","0,30","0,35","0,4","0,45","0,5","0,55","0,6","Total_sm"), c("80","81","82","01","02")))
T_obs<-df %>% filter(submuestra==80) %>% count(submuestra) %>%
  bind_rows(df %>% filter(submuestra==81) %>% count(submuestra)) %>%
  bind_rows(df %>% filter(submuestra==82) %>% count(submuestra)) %>%
  bind_rows(df %>% filter(submuestra==1) %>% count(submuestra)) %>%
  bind_rows(df %>% filter(submuestra==2) %>% count(submuestra)) 
df2 = df %>% group_by(submuestra) %>% mutate(dificultad = n() - row_number()) %>% 
  mutate(cuantil = cut(dificultad, include.lowest = T, right = F,
                       breaks = quantile(dificultad, 
                       probs = c(0, .05, .1, .15, .2,
                                 .25, .3, .35, .4, .45,
                                 .5, .55, .6, 1)),
                       labels = c("0,05","0,1","0,15","0,2","0,25","0,30","0,35",
                                  "0,4","0,45","0,5","0,55","0,6", "1"))) 

df2

df2 %>% count(cuantil)
df2 %>% group_by(submuestra, cuantil)  %>% summarise(n_incorrecto = sum(predice2 == 0)) %>%
  pivot_wider(id_cols = submuestra, values_from = n_incorrecto, cuantil ) %>% 
  ungroup  %>% select(2:14)  %>% t %>% as.data.frame  %>%  cumsum %>% t %>% as.data.frame %>% 
  mutate(submuestra = c(1, 2, 80, 81, 82)) %>% relocate(submuestra)

?relocate(submuestra, .before = first_column())


for(sm in c(80,81,82,1,2)) {
  if(sm %in% c(80,81,82)) {
    j=sm-79 
  } else {
    j=sm+3
  }
  i=0
  for(prop in c(.05, .1, .15, .2, .25, .3, .35, .4, .45, .5, .55, .6)){
    i=i+1
    mat[i,j]=(1-get_prop_si(prop, filter_sm = sm))*(as.integer(T_obs[j,2])*prop)
  }
  mat[13,j]=as.integer(T_obs[j,2])
}

mat<-round(mat,0)
openxlsx::write.xlsx(data.frame(Prop = row.names(mat), mat), glue("data/output/analisis_punto_corte/N_incorrectos_sin_sgp52.xlsx"), sheetName="N_perdidas", overwrite = TRUE) 
