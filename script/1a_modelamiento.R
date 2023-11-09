

# Cargamos datos caenes
df = 'data/input/brutas/caenes_pre_censo.xlsx' %>% read_excel %>% clean_names %>%
  # Generamos código final en base a auditoría del modelo
  mutate(codigo_final = if_else(codigo_nomenclaturas == 0, seccion, codigo_nomenclaturas)) %>%
  rename(oficio = titulo_ocupacion,
         tareas = tareasdeberes) %>%
  # Procesamos texto
  procesar_texto_dataframe(c(oficio, tareas, act_principal))




# Variables de texto que usaremos en la arquitectura
variables_texto = list('act_principal', 'oficio', 'tareas')
# Largo máximo de las secuencias de las glosas
maxlens = c((df %>% pull(act_principal_proc)) %>% quanteda::tokens() %>% map(length) %>% unlist %>% max
, (df %>% pull(oficio_proc)) %>% quanteda::tokens() %>% map(length) %>% unlist %>% max
, (df %>% pull(tareas_proc)) %>% quanteda::tokens() %>% map(length) %>% unlist %>% max
) %>% set_names(variables_texto)

#  Inputs ------------------------------------------------------------

# Network inputs para cada una de las variables. Incluye x_train, y_train, x_test, y_test, tokenizer, keys y df_test
network_inputs = variables_texto %>%
  map(~pre_process(df, text_variable =  glue("{.x}_proc"),
                   label =  codigo_final, type = "sequences",
                   maxlen = maxlens[.x])) %>%
  set_names(variables_texto)




# Parámetros de la red ----------------------------------------------

# tamaños del vocabulario (n° de palabras únicas) de cada una de las variables de texto:
vocab_sizes = variables_texto %>% map(~(length(network_inputs[[.x]]$tokenizer$word_index) + 1)) %>%
  set_names(variables_texto)

# N° de clases a predecir
clases = length(unique(df$codigo_final))

# 420.  Embbedings --------------------------------------------------------
# Cargamos spanish Word Embeddings de Jorge Perez, modelo grande
reticulate::source_python('script/etl/modelamiento/load_model.py')
if(!exists('modelo_embeddings')){
  modelo_embeddings = get_modelo(tipo_modelo = 'grande') # acepta chico o grande
}

# N° de dimensiones del embedding (info externa)
embedding_dim = 300

# Trae función crear_matriz_embeddings de python
reticulate::source_python("script/etl/modelamiento/create_embeddings.py")


# crea matriz de embeddings para cada variable de dimensión vocab_size x embedding_dim (300)
embedding_matrices = variables_texto %>%
  map(~crear_matriz_embeddings(vocab_size = vocab_sizes[[.x]],
                               dimensiones_modelo = embedding_dim,
                               tokenizer = network_inputs[[.x]]$tokenizer,
                               modelo_embeddings = modelo_embeddings)) %>%
  set_names(variables_texto)



# Arquitectura de la red ---------------------------------------------

# capas de input:
inputs = variables_texto %>% map(~layer_input(shape = list(maxlens[[.x]]), name = glue("{.x}_input"))) %>%
  set_names(variables_texto)


build_model = function(inputs, var){
  'Función build model arma la arquitectura general de la red:
  - Parte de la capa de input
  - Agrega la matriz de embedding
  - La pasa por dropout y luego una capa GRU
  - Finalmente le aplica max pooling y average pooling
  - Entrega ambas capas de pooling'
  model = inputs[[var]] %>%
    layer_embedding(input_dim = vocab_sizes[[var]], output_dim = embedding_dim, input_length = maxlens[[var]],
                    weights = list(embedding_matrices[[var]]), trainable = T) %>%
    layer_spatial_dropout_1d(rate = 0.2 ) %>%
    layer_gru(units = 64, return_sequences = TRUE, trainable = T)

  max_pool = model %>% layer_global_max_pooling_1d(trainable = T)
  ave_pool = model %>% layer_global_average_pooling_1d(trainable = T)

  return(c(max_pool, ave_pool))


}

# Pools para cada uno de las variables (evito usar map para que código quede más explícito)
oficio_pools = build_model(inputs, 'oficio')
tareas_pools = build_model(inputs, 'tareas')
act_principal_pools = build_model(inputs, 'act_principal')



# Modelo final, a las capas de pool, luego le pasa una capa densa, un dropout de 50% y una nueva capa densa de output
output = layer_concatenate(c(oficio_pools,
                             tareas_pools,
                             act_principal_pools) %>% unlist, trainable = F) %>%
  layer_dense(embedding_dim, activation='relu') %>%
  layer_dropout(0.5) %>%
  layer_dense(units = clases, activation = "softmax")

# Juntamos capas input con output
model = keras_model(list(inputs$act_principal, inputs$oficio, inputs$tareas), output)

# Compilar modelo ---------------------------------------------------
model %>%
  compile(
    loss = "sparse_categorical_crossentropy",
    optimizer = "adam",
    metrics = 'accuracy')

summary(model)


# Callbacks para early stopping y checkpoint para guardar modelo
cbs= list(callback_early_stopping(verbose = T, patience=15, monitor='val_loss'),
          callback_model_checkpoint(filepath=glue('data/output/modelo/mejor_modelo_doble_entrada.h5'),
                                    monitor='val_loss',
                                    save_best_only = T, verbose = T))
tf$random$set_seed(104)
#  Entrenamoiento ----
history <- model %>%
  fit(list(network_inputs$act_principal$x_train %>% data.matrix,
           network_inputs$oficio$x_train %>% data.matrix,
           network_inputs$tareas$x_train %>% data.matrix),
      network_inputs$act_principal$y_train,
      epochs = 100,
      validation_data = list(list(network_inputs$act_principal$x_test,
                                  network_inputs$oficio$x_test,
                                  network_inputs$tareas$x_test),
                             network_inputs$act_principal$y_test),
      batch_size = 256,
      verbose = TRUE, callbacks = cbs)

# Evaluación modelo ----
x_test_act_principal = texts_to_sequences(network_inputs$act_principal$tokenizer, network_inputs$act_principal$df_test$act_principal) %>%
  pad_sequences(padding='post', maxlen=maxlens[['act_principal']])

x_test_oficio = texts_to_sequences(network_inputs$oficio$tokenizer, network_inputs$act_principal$df_test[['oficio_proc']]) %>%
  pad_sequences(padding='post', maxlen=maxlens[['oficio']])
x_test_tareas = texts_to_sequences(network_inputs$tareas$tokenizer, network_inputs$act_principal$df_test[['tareas_proc']]) %>%
  pad_sequences(padding='post', maxlen=maxlens[['tareas']])



model %>% evaluate(list(x_test_act_principal, x_test_oficio, x_test_tareas),
                   network_inputs$act_principal$y_test, verbose = 2)

# history <- model %>%
#   fit(list(network_inputs$act_principal$x_train %>% data.matrix),
#       network_inputs$act_principal$y_train,
#       epochs = 100,
#       validation_data = list(network_inputs$act_principal$x_test, network_inputs$act_principal$y_test),
#       batch_size = 256,
#       verbose = TRUE, callbacks = cbs)



# Evaluación modelo -------------------------------------------------------

modelo = load_model_hdf5('data/output/modelo/mejor_modelo_doble_entrada.h5')
keys = network_inputs$act_principal$keys



# Análisis resultados -----------------------------------------------------



y_proba = modelo$predict(list(x_test_act_principal, x_test_oficio, x_test_tareas))


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
network_inputs$act_principal$df_test$codigo_final

y_pred_glosas = y_pred %>% left_join(keys) %>% bind_cols(y_test = network_inputs$act_principal$y_test ) %>%
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
