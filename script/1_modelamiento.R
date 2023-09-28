
# renv::activate()


# 100. Selección datos a utilizar -----------------------------------------
#piloto
#viii
#todo

# 200.  Dataframe con los datos de la piloto y/o viii procesados ----------

df = armar_df_modelamiento_completo(datos_a_utilizar = 'todo', sm_exec = sm_exec)

list.files('data/input/codificadas_entrenamiento/')
# 300.  Inputs ------------------------------------------------------------

maxlen = 15
df = df %>% unite(oficio_tarea, oficio_proc, tareas_proc, remove = F, sep=' ' ) 

network_inputs <- pre_process(df, text_variable =  "oficio_proc", label =  aeciuo_2, type = "sequences",
                              maxlen = maxlen)

# 400.  Modelo ------------------------------------------------------------

# 410.  Parámetros de la red ----------------------------------------------

vocab_size = length(network_inputs$tokenizer$word_index) + 1
 

clases = length(unique(df$aeciuo_2)) 
# 420.  Embbedings --------------------------------------------------------
#Spanish Word Embeddings de Jorge Perez
reticulate::source_python('script/etl/modelamiento/load_model.py')
if(!exists('modelo_embeddings')){
  modelo_embeddings = get_modelo(tipo_modelo = 'grande') # acepta chico o grande
}

# Crear los embeddings a partir del modelo gensim.
tokenizer <- network_inputs$tokenizer
embedding_dim = 300
reticulate::source_python("script/etl/modelamiento/create_embeddings.py")
embedding_matrix = crear_matriz_embeddings(dimensiones_modelo = embedding_dim, modelo_embeddings = modelo_embeddings) 

# 430. Arquitectura de la red ---------------------------------------------
input = layer_input(shape = list(maxlen), name = "input")

model = input %>%
  layer_embedding(input_dim = vocab_size, output_dim = embedding_dim, input_length = maxlen,
                  weights = list(embedding_matrix), trainable = FALSE) %>%
  layer_spatial_dropout_1d(rate = 0.15 ) %>%
  layer_gru(units = 150, return_sequences = TRUE)

max_pool = model %>% layer_global_max_pooling_1d()
ave_pool = model %>% layer_global_average_pooling_1d()

output = layer_concatenate(list(ave_pool, max_pool)) %>%
  layer_dense(embedding_dim, activation='relu') %>%
  layer_dropout(0.5) %>%
  layer_dense(units = clases, activation = "softmax")

model = keras_model(input, output)


# 440.  Compilar modelo ---------------------------------------------------
model %>% 
  compile(
    loss = "sparse_categorical_crossentropy", 
    optimizer = "adam",
    metrics = 'accuracy')

summary(model)

cbs= list(callback_early_stopping(verbose = T, patience=10, monitor='val_loss'),
          callback_model_checkpoint(filepath='data/output/modelo/modelo_ciuo_ix_epf.hdf5', monitor='val_loss', save_best_only = T, verbose = T ))
# 450.  Entrenamiento -----------------------------------------------------
tf$random$set_seed(104) 
history <- model %>% fit(
  network_inputs$x_train %>% data.matrix, network_inputs$y_train,
  epochs = 100,
  validation_data = list(network_inputs$x_test %>% data.matrix, network_inputs$y_test),
  batch_size = 256,
  verbose = TRUE, callbacks = cbs)


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
