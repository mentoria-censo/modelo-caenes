modelo = load_model_hdf5('data/output/modelo/modelo_ciuo_ix_epf.hdf5')


datos_nuevos = armar_df_modelamiento_completo(datos_a_utilizar = 'nuevos', sm_exec = sm_exec)
df_completo = armar_df_modelamiento_completo(datos_a_utilizar = 'todo', sm_exec = sm_exec)

ciuo = readxl::read_excel('data/input/brutas/ciuo-08-cl-no-modificar.xls') %>%
  rename(aeciuo_2=...2, subgrupo=...3, grupo=...4, glosa=...5) %>%
  filter( !is.na(aeciuo_2) &  is.na(subgrupo) & is.na(grupo)) %>%
  select("aeciuo_2","glosa") %>%
  mutate(aeciuo_2=as.character(aeciuo_2))
ciuo %>% filter(aeciuo_2 %in% c('44', '96', '92', '34', '35'))

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
  dificultad = proxy_dificultad,
  `Codificacion_manual_Codificador_A` = '',
  `Codificacion_manual_Codificador_B` = '',
  `Revisión cruzada-> comparación de códigos asignados por Codificador A y B ` = '',
  `Revisión de contraste` = '') %>%
  arrange(dificultad) %>% 
  select(-c(oficio_proc, oficio_tarea, tareas_proc, dificultad))

datos_nuevos %>%   writexl::write_xlsx(glue('data/output/codigos_maca/epf_1_1_ta_a_codificar_ciuo_sm{sm_exec}.xlsx'))
