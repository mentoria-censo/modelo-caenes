
# renv::activate()


# 100. Selección datos a utilizar -----------------------------------------
#piloto
#viii
#todo

# 200.  Dataframe con los datos de la piloto y/o viii procesados ----------

# df = armar_df_modelamiento_completo(datos_a_utilizar = 'todo', sm_exec = sm_exec)
df = 'data/input/brutas/caenes_pre_censo.xlsx' %>% read_excel %>% clean_names %>%
  mutate(codigo_final = if_else(codigo_nomenclaturas == 0, seccion, codigo_nomenclaturas)) %>%
  rename(oficio = titulo_ocupacion,
         tareas = tareasdeberes) %>%  procesar_texto_dataframe(c(oficio, tareas, act_principal))

df %>% select(matches('oficio|tareas|act_principal'), codigo_final) %>% view

# 300.  Inputs ------------------------------------------------------------
maxlen_act_ppal = 25
maxlen_oficio = 15
maxlen_tareas = 25

act_ppal_network_inputs <- pre_process(df, text_variable =  "act_principal_proc",
                                       label =  codigo_final, type = "sequences",
                                       maxlen = maxlen_act_ppal)
tareas_network_inputs <- pre_process(df, text_variable =  "tareas_proc", label =  codigo_final, type = "sequences",
                             maxlen = maxlen_tareas)
oficio_network_inputs <- pre_process(df, text_variable =  "oficio_proc", label =  codigo_final, type = "sequences",
                                     maxlen = maxlen_oficio)

# oficio_network_inputs$x_train %>% dim
# tareas_network_inputs$x_train %>% dim

# 400.  Modelo ------------------------------------------------------------
# 410.  Parámetros de la red ----------------------------------------------
act_ppal_vocab_size = length(act_ppal_network_inputs$tokenizer$word_index) + 1

oficio_vocab_size = length(oficio_network_inputs$tokenizer$word_index) + 1
tareas_vocab_size = length(tareas_network_inputs$tokenizer$word_index) + 1


clases = length(unique(df$codigo_final))

# 420.  Embbedings --------------------------------------------------------
#Spanish Word Embeddings de Jorge Perez
reticulate::source_python('script/etl/modelamiento/load_model.py')
if(!exists('modelo_embeddings')){
  modelo_embeddings = get_modelo(tipo_modelo = 'grande') # acepta chico o grande
}

# Crear los embeddings a partir del modelo gensim.
act_ppal_tokenizer <- act_ppal_network_inputs$tokenizer
oficio_tokenizer <- oficio_network_inputs$tokenizer
tareas_tokenizer <- tareas_network_inputs$tokenizer
embedding_dim = 300
###AQUI-> tuve que cambiar la función, también dupliqué el archivo
reticulate::source_python("script/etl/modelamiento/create_embeddings_doble_entrada.py")

oficio_embedding_matrix = crear_matriz_embeddings(vocab_size = oficio_vocab_size,
                                                  dimensiones_modelo = embedding_dim,
                                                  tokenizer = oficio_tokenizer,
                                                  modelo_embeddings = modelo_embeddings)

tareas_embedding_matrix = crear_matriz_embeddings(vocab_size = tareas_vocab_size,
                                                  dimensiones_modelo = embedding_dim,
                                                  tokenizer = tareas_tokenizer,
                                                  modelo_embeddings = modelo_embeddings)

act_ppal_embedding_matrix = crear_matriz_embeddings(vocab_size = act_ppal_vocab_size,
                                                  dimensiones_modelo = embedding_dim,
                                                  tokenizer = act_ppal_tokenizer,
                                                  modelo_embeddings = modelo_embeddings)



# 430. Arquitectura de la red ---------------------------------------------
oficio_input = layer_input(shape = list(maxlen_oficio), name = "oficio_input")
tareas_input = layer_input(shape = list(maxlen_tareas), name = "tareas_input")
act_ppal_input = layer_input(shape = list(maxlen_act_ppal), name = "act_ppal_input")

oficio_model = oficio_input %>%
  layer_embedding(input_dim = oficio_vocab_size, output_dim = embedding_dim, input_length = maxlen_oficio,
                  weights = list(oficio_embedding_matrix), trainable = FALSE) %>%
  layer_spatial_dropout_1d(rate = 0.2 ) %>%
  layer_gru(units = 150, return_sequences = TRUE, trainable = T)

oficio_max_pool = oficio_model %>% layer_global_max_pooling_1d(trainable = T)
oficio_ave_pool = oficio_model %>% layer_global_average_pooling_1d(trainable = T)

tareas_model = tareas_input %>%
  layer_embedding(input_dim = tareas_vocab_size, output_dim = embedding_dim, input_length = maxlen_tareas,
                  weights = list(tareas_embedding_matrix), trainable = FALSE) %>%
  layer_spatial_dropout_1d(rate = 0.2 ) %>%
  layer_gru(units = 150, return_sequences = TRUE, trainable = T)

tareas_max_pool = tareas_model %>% layer_global_max_pooling_1d(trainable = T)
tareas_ave_pool = tareas_model %>% layer_global_average_pooling_1d(trainable = T)

act_ppal_model = act_ppal_input %>%
  layer_embedding(input_dim = act_ppal_vocab_size, output_dim = embedding_dim, input_length = maxlen_act_ppal,
                  weights = list(act_ppal_embedding_matrix), trainable = FALSE) %>%
  layer_spatial_dropout_1d(rate = 0.2, trainable = T ) %>%
  layer_gru(units = 150, return_sequences = TRUE, trainable = T)

act_ppal_max_pool = act_ppal_model %>% layer_global_max_pooling_1d(name='ap_mp', trainable = T)
act_ppal_ave_pool = act_ppal_model %>% layer_global_average_pooling_1d(name = 'ap_ap', trainable = T)




output = layer_concatenate(list(act_ppal_max_pool, act_ppal_ave_pool,
                                oficio_max_pool, oficio_ave_pool,
                                tareas_max_pool, tareas_ave_pool), trainable = F) %>%
  layer_dense(embedding_dim, activation='relu') %>%
  layer_dropout(0.5) %>%
  layer_dense(units = clases, activation = "softmax")

model = keras_model(list(act_ppal_input, oficio_input, tareas_input), output)
#model = keras_model(act_ppal_input, output)
# 440.  Compilar modelo ---------------------------------------------------
model %>%
  compile(
    loss = "sparse_categorical_crossentropy",
    optimizer = "adam",
    metrics = 'accuracy')

summary(model)


# 450.  Entrenamiento -----------------------------------------------------
cbs= list(callback_early_stopping(verbose = T, patience=15, monitor='val_loss'),
          callback_model_checkpoint(filepath=glue('data/output/modelo/mejor_modelo_doble_entrada.h5'),
                                    monitor='val_loss',
                                    save_best_only = T, verbose = T))
tf$random$set_seed(104)
history <- model %>%
  fit(list(act_ppal_network_inputs$x_train %>% data.matrix, oficio_network_inputs$x_train %>% data.matrix, tareas_network_inputs$x_train %>% data.matrix),
      act_ppal_network_inputs$y_train,
      epochs = 100,
      validation_data = list(list(act_ppal_network_inputs$x_test, oficio_network_inputs$x_test, tareas_network_inputs$x_test),
                             act_ppal_network_inputs$y_test),
      batch_size = 256,
      verbose = TRUE, callbacks = cbs)

# history <- model %>%
#   fit(list(act_ppal_network_inputs$x_train %>% data.matrix),
#       act_ppal_network_inputs$y_train,
#       epochs = 100,
#       validation_data = list(act_ppal_network_inputs$x_test, act_ppal_network_inputs$y_test),
#       batch_size = 256,
#       verbose = TRUE, callbacks = cbs)



# Evaluación modelo -------------------------------------------------------

modelo = load_model_hdf5('data/output/modelo/mejor_modelo_doble_entrada.h5')
keys = act_ppal_network_inputs$keys

x_test_act_ppal = texts_to_sequences(act_ppal_tokenizer, act_ppal_network_inputs$df_test$act_principal) %>%
  pad_sequences(padding='post', maxlen=maxlen_act_ppal)

x_test_oficio = texts_to_sequences(oficio_tokenizer, act_ppal_network_inputs$df_test[['oficio_proc']]) %>%
  pad_sequences(padding='post', maxlen=maxlen_oficio)
x_test_tareas = texts_to_sequences(tareas_tokenizer, act_ppal_network_inputs$df_test[['tareas_proc']]) %>%
  pad_sequences(padding='post', maxlen=maxlen_tareas)

y_proba = modelo$predict(list(x_test_act_ppal, x_test_oficio, x_test_tareas))


proxy_dificultad = apply(y_proba, 1, function(z) {
  z <- sort(z, decreasing = TRUE)[1:2]
  z[1] / z[2]
})

pbb_mas_alta = apply(y_proba, 1, function(z) {
  z <- sort(z, decreasing = TRUE)[1:2]
  z[1]
})

y_pred_vector = y_proba %>% Rfast::rowMaxs() - 1

y_pred = y_pred_vector %>% matrix(ncol=1) %>% data.frame %>% setNames('codigo_int')
act_ppal_network_inputs$df_test$codigo_final

y_pred_glosas = y_pred %>% left_join(keys) %>% bind_cols(y_test = act_ppal_network_inputs$y_test ) %>%
  mutate(predice = int(y_pred == y_test),
         dificultad = proxy_dificultad,
         pbb_caso = pbb_mas_alta)
y_pred_glosas %>% arrange(-pbb_caso) %>% slice_head(prop = .3) %>% summarise(media_accuracy = mean(predice))
y_pred_glosas %>% summarise(media_accuracy = mean(predice))
y_pred_glosas %>% group_by(codigo_final) %>% summarise(media_accuracy = mean(predice),
                                                       n_casos = n()) %>%
  arrange(-n_casos) %>% writexl::write_xlsx('data/output/summary_data/resumen_modelo_simple.xlsx')

# model %>% save_model_hdf5('data/output/modelo/modelo_ciuo_ix_epf_doble_entrada.hdf5')


# # 460. Análisis resultados ------------------------------------------------
#
# y_test = network_inputs$y_test
# model %>% save_model_hdf5('data/output/modelo/modelo_ciuo_ix_epf.hdf5')
# model2 = load_model_hdf5('data/output/modelo/modelo_ciuo_ix_epf.hdf5')
#
# y_proba = model2$predict(network_inputs$x_test)
# a = pmax(y_proba)
# y_pred_vector = y_proba %>% Rfast::rowMaxs() - 1
# y_pred = y_pred_vector %>% matrix(ncol=1) %>% data.frame %>% setNames('codigo_int')
#
# ciuo = readxl::read_excel('data/input/brutas/ciuo-08-cl-no-modificar.xls') %>%
#   rename(aeciuo_2=...2, subgrupo=...3, grupo=...4, glosa=...5) %>%
#   filter( !is.na(aeciuo_2) &  is.na(subgrupo) & is.na(grupo)) %>%
#   select("aeciuo_2","glosa") %>%
#   mutate(aeciuo_2=as.character(aeciuo_2))
# keys = network_inputs$keys %>% inner_join(ciuo)
# readr::write_csv2(keys, 'data/output/keys.csv')
#
# df = df %>% mutate(aeciuo_2 = as.character(aeciuo_2))
#
# y_pred_glosas = y_pred %>% inner_join(keys)
# y_pred_test = y_pred_glosas %>% cbind(y_test)%>% mutate(accuracy = if_else(codigo_int == y_test, 1, 0))
# readr::write_csv2(y_pred_test, 'data/output/y_pred_test.csv')
#
# accuracy_por_codigo = y_pred_test %>% group_by(aeciuo_2) %>% summarise(media_acc = mean(accuracy),
#                                                                        n_obs = n())
# codigos_faltantes = y_pred %>% inner_join(keys) %>% full_join(ciuo) %>% filter(is.na(codigo_int))
#
#
#
#
# y_test
# x_test = network_inputs$x_test
#
# x_test = network_inputs$x_test
# x_train = network_inputs$x_train
# df_test= network_inputs$df_test
# df_test %>% colnames
# tabla_maca = y_pred_glosas %>% cbind(df_test %>% select(folio, n_linea, oficio,tareas )) %>% select(!codigo_int) %>%
#   relocate(aeciuo_2, .after = last_col()) %>% relocate(glosa, .after = last_col())%>%
#   rename(`predicción del modelo` = aeciuo_2, `glosa predicción` = glosa) %>% mutate(`está bien la predicción?` = '',
#                                                                             `codificación correcta` = '')
#
# df_test %>% colnames
# openxlsx::write.xlsx(tabla_maca, 'data/output/codigos_maca/tabla_maca_ejemplo.xlsx', overwrite = TRUE)
#
# # Prueba ------------------------------------------------------------------
#
#
#
