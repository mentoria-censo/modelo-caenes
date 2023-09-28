
# Carga datos
# modelo = load_model_hdf5('data/output/modelo/modelo_ciuo_ix_epf_doble_entrada.hdf5')

modelo = load_model_hdf5(glue('data/output/modelo/mejor_modelo_sm{sm_exec}.h5'))

df_completo = armar_df_modelamiento_completo(datos_a_utilizar = 'todo', sm_exec= sm_exec)

datos_nuevos = armar_df_modelamiento_completo(datos_a_utilizar = 'nuevos', sm_exec = sm_str)
datos_nuevos %>% count(submuestra)
ciuo = readxl::read_excel('data/input/brutas/ciuo-08-cl-no-modificar.xls') %>%
  rename(aeciuo_2=...2, subgrupo=...3, grupo=...4, glosa=...5) %>%
  filter( !is.na(aeciuo_2) &  is.na(subgrupo) & is.na(grupo)) %>%
  select("aeciuo_2","glosa") %>%
  mutate(aeciuo_2=as.character(aeciuo_2))

maxlen_oficio = 15
maxlen_tareas = 25

oficio_network_inputs <- pre_process(df_completo, text_variable =  "oficio_proc",
                                     label =  aeciuo_2, type = "sequences",
                                     maxlen = maxlen_oficio)
tareas_network_inputs <- pre_process(df_completo, text_variable =  "tareas_proc", 
                                     label =  aeciuo_2, type = "sequences",
                                     maxlen = maxlen_tareas)
y_train = oficio_network_inputs[['y_train']] %>% as.data.frame()
names(y_train)  = 'codigo_int'

keys = oficio_network_inputs$keys %>% inner_join(ciuo)
oficio_network_inputs$keys
oficio_tokenizer <- oficio_network_inputs$tokenizer
tareas_tokenizer <- tareas_network_inputs$tokenizer

datos_a_predecir <- list(as.array(datos_nuevos[['oficio_proc']]), as.array(datos_nuevos[['tareas_proc']]))
x_test_oficio = texts_to_sequences(oficio_tokenizer, datos_nuevos[['oficio_proc']]) %>%
  pad_sequences(padding='post', maxlen=maxlen_oficio)
x_test_tareas = texts_to_sequences(tareas_tokenizer, datos_nuevos[['tareas_proc']]) %>%
  pad_sequences(padding='post', maxlen=maxlen_tareas)

y_proba = modelo$predict(list(x_test_oficio, x_test_tareas))

proxy_dificultad = apply(y_proba, 1, function(z) {
  z <- sort(z, decreasing = TRUE)[1:2]
  z[1] / z[2]
})
y_pred_vector = y_proba %>% Rfast::rowMaxs() - 1

y_pred = y_pred_vector %>% matrix(ncol=1) %>% data.frame %>% setNames('codigo_int')
y_pred_glosas = y_pred %>% left_join(keys)
y_pred %>% count(codigo_int)

datos_nuevos = datos_nuevos %>% mutate(
  glosa_prediccion = y_pred_glosas$glosa,
  prediccion_modelo = y_pred_glosas$aeciuo_2,
  dificultad = proxy_dificultad,
  `Codificacion_manual_Codificador_A` = '',
  `Codificacion_manual_Codificador_B` = '',
  `Revisión cruzada-> comparación de códigos asignados por Codificador A y B ` = '',
  `Revisión de contraste` = '',
  `CODIGO` = '') %>%
  arrange(dificultad) %>% 
  select(-c(oficio_proc, oficio_tarea, tareas_proc, dificultad))

prop_sin_codificar = .6
datos_nuevos_cod = datos_nuevos %>% slice_head(prop = (1-prop_sin_codificar))
datos_nuevos_nocod = datos_nuevos %>% anti_join(datos_nuevos_cod) %>%
  mutate(persona_codifica = if_else(row_number() %% 2 == 0, 'Anto', 'Juane'),
         predice_bien = '', codigo_final = '') %>% 
  select(-c(Codificacion_manual_Codificador_A, Codificacion_manual_Codificador_B,
            `Revisión cruzada-> comparación de códigos asignados por Codificador A y B `,
            `Revisión de contraste`,
            `CODIGO`)) %>% 
  relocate(oficio, tareas, .before = glosa_prediccion)

datos_nuevos_cod %>%  
  writexl::write_xlsx(glue('data/output/codigos_maca/epf_1_1_ta_acodificar_ciuo_sm{sm_str}_codificacion.xlsx'))
datos_nuevos_cod %>%  
  writexl::write_xlsx(glue('data/output/codigos_maca/epf_2_0_ta_codificados_ciuo_sm{sm_str}_codificacion.xlsx'))



datos_nuevos_nocod %>% writexl::write_xlsx(glue('data/output/codigos_automaticos/epf_1_1_ta_acodificar_ciuo_sm{sm_str}_tecnica.xlsx'))
datos_nuevos_nocod %>% writexl::write_xlsx(glue('data/output/codigos_automaticos/epf_2_0_ta_codificados_ciuo_sm{sm_str}_tecnica.xlsx'))
