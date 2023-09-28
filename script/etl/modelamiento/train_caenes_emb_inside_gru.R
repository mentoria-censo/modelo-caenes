
library(keras)
library(reticulate)
library(tidyverse)
library(caret)
library(feather)
library(tensorflow)
tensorflow::tf_version()


#reticulate::source_python('scripts/entrenamiento/red/load_model.py')


# Cargar funciones
source("scripts/entrenamiento/helpers_training.R", encoding = "utf-8")

# Cargar datos
path = "data/finales/train.feather"
df = read_feather(path)

# Aquí están los inputs para la red
network_inputs <- pre_process(df, text_variable =  "glosa_dep", label =  "cod_final", type = "sequences")

# Parámetros de la red
vocab_size = length(network_inputs$tokenizer$word_index) + 1  
embedding_dim = 30 # OJO CON EMBEDDING DIMENSION, IMPORTANTE
maxlen = 40
clases = length(unique(df$cod_final)) 

# Crear los embeddings a partir del modelo gensim. Tarda en correr, debido a que el modelo es muy pesado
tokenizer <- network_inputs$tokenizer

source_python("scripts/entrenamiento/red/create_embeddings.py")

# Matriz de embeddings creada con código python
embedding_matrix <-  py$embedding_matrix

##########################
# Arquitectura de la red #
##########################

input = layer_input(shape = list(maxlen), name = "input")

model = input %>%
  layer_embedding(input_dim = vocab_size, output_dim = 30, input_length = maxlen,
                  weights = list(embedding_matrix), trainable = FALSE) %>%
  layer_spatial_dropout_1d(rate = 0.1 ) %>%
  #bidirectional(
  layer_gru(units = 150, return_sequences = TRUE)
#)
max_pool = model %>% layer_global_max_pooling_1d()
ave_pool = model %>% layer_global_average_pooling_1d()

output = layer_concatenate(list(ave_pool, max_pool)) %>%
  layer_dense(30, activation='relu') %>%
  layer_dropout(0.5) %>%
  layer_dense(units = clases, activation = "softmax")

model = keras_model(input, output)


# Compilar modelo
model %>% 
  compile(
    loss = "sparse_categorical_crossentropy", 
    optimizer = "adam",
    metrics = 'accuracy')

summary(model)

# Entrenar
tf$random$set_seed(104) 
history <- model %>% fit(
  network_inputs$x_train, network_inputs$y_train,
  epochs = 25,
  validation_data = list(network_inputs$x_test, network_inputs$y_test),
  batch_size = 256,
  verbose = TRUE
)
y_pred = model$predict(network_inputs$x_test)
test = network_inputs$x_test
y_pred %>% dim
test %>% dim # y_test tiene 40 columnas: especificado en maxlen
embedding_matrix %>% dim # 30 columnas: embedding max especificado

# Save the model
save_model_hdf5(model, "data/finales/modelos/modelo_red_emb_gru")


