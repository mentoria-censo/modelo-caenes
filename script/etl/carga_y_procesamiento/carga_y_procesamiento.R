


# Carga de datos ----------------------------------------------------------


cargar_control_bruto = function(){
  submuestras = 91:95
  control_bruto = data.frame()
  for(sm in submuestras){

    aux = read_dta(glue('data/input/brutas/rph_sm{sm}.dta'), encoding = 'latin1')
    # aux$sm = sm
    control_bruto = rbind(control_bruto, aux)
  }
  control_bruto = control_bruto %>% group_by(FOLIO) %>%
                                    mutate(nro_rph = seq_along(FOLIO))

  control_bruto_final = control_bruto %>% select(AE09A, AE09B, FOLIO, nro_rph)


}

cargar_control_y_tratamiento = function(){
  submuestras = 91:95
  control = data.frame()
  tratamiento = data.frame()
  for(sm in submuestras){
    for(ct in c('control', 'tratamiento')){

      aux = feather::read_feather(glue('data/input/codificadas_entrenamiento/rph_4_epf20_estructurada_{ct}_sm{sm}.feather')) %>%
        select("sub_muestra","oficio", "tareas", "aeciuo_2", 'folio', 'nro_rph') %>%
        mutate(tipo=ct)
      if(ct == 'control'){
        control = rbind(control, aux)
      } else{
        tratamiento = rbind(tratamiento, aux)
      }

    }

  }
  return = list(control, tratamiento)
}

cargar_2_0_ta_codificados_ciuo = function(sm_exec){
  archivos=list.files(path = "data/input/codificadas_IXEPF", pattern = "^epf_2_0_ta_codificados_ciuo_sm")
  ta_codificados = data.frame()
  for(file in archivos){
    print(file)
    if (file=="epf_2_0_ta_codificados_ciuo_sm80.xlsx"){
      aux = read_excel(glue('data/input/codificadas_IXEPF/{file}')) %>%
        select("oficio", "tareas", "aeciuo_2"="codigo_cod", 'folio', 'nro_rph') %>%
        mutate(submuestra="80")
    } else {
      aux = read_excel(glue('data/input/codificadas_IXEPF/{file}')) %>%
        select("submuestra","oficio", "tareas", "aeciuo_2"=matches("codigo(_cod)?"), 'folio', 'nro_rph')

    }
    ta_codificados = rbind(ta_codificados, aux)
  }

  ta_codificados_final = ta_codificados  %>%
    mutate(submuestra = as.numeric(submuestra)) %>%
    {if(sm_exec >=80 ) filter(., submuestra < sm_exec  & submuestra > 24 ) else filter(., submuestra  >= 80 | submuestra < sm_exec)}

  return(ta_codificados_final)
}

# Procesamiento de datos --------------------------------------------------




procesamiento_extra_para_modelamiento = function(df, datos_nuevos = F){
  "Elimina códigos con menos de 3 observaciones, reemplaza codigos especiales por vaciós y genera columna que junta oficio y tarea"
  if( datos_nuevos == F){
    codigos_suficientes = df %>% count(aeciuo_2)  %>%
      filter(n>2) %>% pull(aeciuo_2)

    df = df %>% filter(aeciuo_2 %in% codigos_suficientes) %>%

      unite(oficio_tarea, oficio_proc, tareas_proc, remove = F, sep= ' ') %>%

      mutate(tareas_proc = case_when(tareas_proc %in% c("66", "77", "88", "99") ~ "",

                                     TRUE ~ tareas_proc))

  } else{
    df = df  %>% unite(oficio_tarea, oficio_proc, tareas_proc, remove = F, sep= ' ') %>%
      mutate(tareas_proc = case_when(tareas_proc %in% c("66", "77", "88", "99") ~ "",
                                     TRUE ~ tareas_proc))

  }

}

## Procesamiento de texto --------------------------------------------------


procesar_texto_columna <- function(columna, quitar_digitos, quitar_puntuacion, quitar_stopwords,
                                   todo_minuscula,
                                   stopwords_especificas= F) {
  'Función procesa el texto de una columna específica. Actualmente hace lo siguiente:
  - Opcionalmente remueve stopwords.
  - Opcionalmente pasa texto a minúsculas.
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
    {if(todo_minuscula) tolower(.) else .} %>%
    {if(quitar_stopwords) str_remove_all(., stopwords_regex) else .} %>%
    {if(quitar_puntuacion) str_remove_all(., "[[:punct:]]") else .} %>%
    {if(quitar_digitos) str_remove_all(., "\\d+" ) else .} %>%
    str_squish() %>% replace_na('')

}


procesar_texto_dataframe <- function(df, columns_list, sufijo = '_proc', quitar_digitos = F,
                                     quitar_puntuacion = T, quitar_stopwords = T,
                                     todo_minuscula = T,
                                     stopwords_especificas = NULL) {
  'Función procesa el texto de las columnas especificadas en column_list,
  utilizando la función procesar_texto_columna.
  Las columnas con texto procesado, tendrán el sufijo "_proc", lo que permite comparar
  el texto original con el texto procesado.'

  df_procesado = df %>% mutate(across( {{columns_list}},
                                       ~procesar_texto_columna(.x, quitar_digitos = quitar_digitos,
                                                                   quitar_puntuacion = quitar_puntuacion,
                                                                   quitar_stopwords = quitar_stopwords,
                                                                   stopwords_especificas = stopwords_especificas,
                                                               todo_minuscula = todo_minuscula),
                                       .names = "{.col}{sufijo}"))
}




# Consolidación de datos --------------------------------------------------


armar_dataframe_viii_epf = function(completa = T){
  "Usa datos codificados de la VIII EPF y los traspasa a formato adecuado para modelamiento"
  stopwords_regex = paste(tm::stopwords('es'), collapse = '\\b|\\b')
  stopwords_regex = glue('\\b{stopwords_regex}\\b')
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
                                T ~ ciuo_08cl_rev)) %>% filter(!aeciuo_2 %in% c(101, 102))

  if(completa) {
    viii = viii_final} else {
    viii = viii_final %>% filter(!ciuo_08cl_rev %>% is.na)}


  viii = viii  %>% procesar_texto_dataframe(c(oficio, tareas))


}


armar_dataframe_piloto_ixepf = function(){
  ""



    control_bruto = cargar_control_bruto()

    # 002. Bases codificadas
    ct = cargar_control_y_tratamiento()
    control_mal_encoding = ct[[1]]
    tratamiento = ct[[2]]

    # 003. Arreglo variables string usando control bruto

    control = merge(control_mal_encoding, control_bruto, by.x = c('folio', 'nro_rph'),
                    by.y = c('FOLIO', 'nro_rph')) %>%
      select(-c('oficio', 'tareas')) %>%
      rename(oficio = AE09A, tareas = AE09B)

    # 004. Consolidación bases de datos

    df = rbind(control, tratamiento) %>%
      filter(!(oficio %in% c("-88", "-99", "-77", "", "-66")) & (aeciuo_2 != 999) %>% replace_na(TRUE))






  # 007.  Procesamiento texto


  df = df %>% procesar_texto_dataframe(c(oficio, tareas)) %>%
    # Genera edición manual solo si son datos originales:
    mutate(id = seq_along(oficio),
           aeciuo_2 = case_when(id == 422 ~ 91, # RElleno de NAs que les faltaba codificación
                                id == 423 ~ 93,
                                id == 424 ~ 74,
                                id == 425 ~ 91,
                 # 006.  Re-codificación manual subgrupo principal(sgp) 101
                 #se asignaba 101 genéricamente para las FFAA, existen dos observaciones
                 #que pertenecen al sgp 03 y que están codificadas con 101.
                 aeciuo_2 == 101 & folio=="94004-1" & nro_rph==1 & sub_muestra==94 ~ 03,
                 aeciuo_2 == 101 & folio=="95069-1" & nro_rph==1 & sub_muestra==95 ~ 03,
                                TRUE~aeciuo_2))

}

armar_dataframe_para_predecir = function(sm_exec){
  df = read_feather(glue('data/input/brutas/epf_1_1_ta_acodificar_ciuo_sm{sm_exec}.feather')) %>%
    filter(!(oficio %in% c("-88", "-99", "-77", "", "-66"))) %>% procesar_texto_dataframe(c(oficio, tareas))

}


armar_dataframe_ix_epf_codificada = function(sm_exec, datos_nuevos = F){
  "Junta todas las bases codificadasque hemos recibido y luego las procesa,
  para poder ser utilizadas en el re-entrenamiento."

  # 001. Bases codificadas
  df=cargar_2_0_ta_codificados_ciuo(sm_exec = sm_exec)

  # 002. Consolidación bases de datos

  df = df %>%
    filter(!(oficio %in% c("-88", "-99", "-77", "", "-66")) & (aeciuo_2 != 999) %>% replace_na(TRUE))


  # 007.  Procesamiento texto

  df = df %>% procesar_texto_dataframe(c(oficio, tareas))
}





armar_df_modelamiento_completo = function(datos_a_utilizar, sm_exec, viii_completa = T){
  "Arma dataframe que listo para ser usado en el modelo.
  datos_a_utilizar acepta los valores 'piloto', 'viii', 'todo' y 'nuevos' "

  if (datos_a_utilizar=="viii") {
    df = armar_dataframe_viii_epf(completa = viii_completa) %>%
      procesamiento_extra_para_modelamiento

  } else if (datos_a_utilizar=="piloto") {
    df = armar_dataframe_piloto_ixepf() %>%
      procesamiento_extra_para_modelamiento(datos_nuevos = F)

  } else if (datos_a_utilizar=="todo") {

    viii = armar_dataframe_viii_epf(completa = viii_completa) %>% mutate(origen = 'viii_epf')
    piloto = armar_dataframe_piloto_ixepf() %>% mutate(origen = 'piloto_ix_epf')

    ix_epf = armar_dataframe_ix_epf_codificada(sm_exec = sm_exec) %>% mutate(origen = 'ix_epf')

    df <- plyr::rbind.fill(viii, piloto, ix_epf) %>%
      procesamiento_extra_para_modelamiento(datos_nuevos = F)  %>%
      filter(!is.na(aeciuo_2) & !aeciuo_2 %in% c('-66','-88')) %>% mutate(aeciuo_2 = if_else(aeciuo_2 == '3', '03', aeciuo_2))
    rm(viii,piloto,ix_epf)

  } else if(datos_a_utilizar == 'nuevos'){
    df = armar_dataframe_para_predecir(sm_exec) %>%
      procesamiento_extra_para_modelamiento(datos_nuevos = T)
  }
  else{
    return(print('Error: ingresar "viii", "piloto", "todo" o "nuevos".')) #todo incluye ix codificadas
  }
  return(df)
}



