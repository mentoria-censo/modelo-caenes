#################
# TRAIN HELPERS #
#################
pre_process <- function(df, text_variable, label, maxlen, type = "sequences") {


  #label = expr(codigo_final)
  # Es necesario que label esté en el mismo formato qye text_variable para que
  #funcione.

  df <- df %>%
    mutate ({{label}} := as.character({{label}}))


  # Renombrar etiquetas para evitar problemas con el número de categorías

  keys <- df %>% pull({{label}}) %>% unique %>%
    as.data.frame() %>%
    rename({{label}}:= '.') %>%
    arrange({{label}}) %>%
    mutate(codigo_int = row_number() - 1)

  print(keys)

  df <- df %>%
    left_join(keys, by =  names(select(., {{label}})))


  # Número de SB
  sbgrupo = length(unique(df$codigo_final))

  # Separar en train y test
  set.seed(1234)
  trainindex <- createDataPartition(df %>% pull({{label}}), p=0.8,list=FALSE)

  train <- df %>%
    dplyr::slice(trainindex)

  df_test <- df %>%
    dplyr::slice(-trainindex)

  # Pasar todo a formato array
  y_train <- as.array(train$codigo_int)
  y_test <- as.array(df_test$codigo_int)
  x_train <- as.array(train[[text_variable]])
  x_test <- as.array(df_test[[text_variable]])

  # Tokenizar
  tokenizer <-  text_tokenizer( oov_token = "OOV")
  fit_text_tokenizer(tokenizer, x_train)


  #save_text_tokenizer(tokenizer, "data/finales/modelos/modelo_red_seq")

  if (type == "sequences") {
    # Transformar en secuencias
    train_sequences = texts_to_sequences(tokenizer, x_train)
    test_sequences = texts_to_sequences(tokenizer, x_test)



    # Generar padding
    x_train <- pad_sequences(train_sequences, padding='post', maxlen=maxlen)
    x_test <- pad_sequences(test_sequences, padding='post', maxlen=maxlen)

  } else if (type == "tfidf") {

    # Generar tfidf
    x_train = texts_to_matrix(tokenizer, x_train, mode='tfidf')
    x_test = texts_to_matrix(tokenizer, x_test, mode = "tfidf")
  }


  #save_text_tokenizer(tokenizer, filename = "scripts/entrenamiento/app/data/tokenizer_red_tfidf")

  output <- list(x_train, y_train,  x_test, y_test, tokenizer, keys, df_test)
  names(output) <- c("x_train", "y_train",  "x_test", "y_test", "tokenizer", "keys", "df_test")
  return(output)

}




# pre_process_por_sm <- function(df, text_variable, label, maxlen, sm_filtro, type = "sequences") {
#
#
#
#   # Es necesario que label esté en el mismo formato qye text_variable para que
#   #funcione.
#   df <- df %>%
#     mutate ({{label}} := as.character({{label}}))
#
#
#   # Renombrar etiquetas para evitar problemas con el número de categorías
#
#   keys <- df %>% pull({{label}}) %>% unique %>%
#     as.data.frame() %>%
#     rename({{label}}:= '.') %>%
#     arrange({{label}}) %>%
#     mutate(codigo_int = row_number() - 1)
#
#   #print(keys)
#
#   df <- df %>%
#     left_join(keys, by =  names(select(., {{label}})))
#
#
#   # Número de SB
#   sbgrupo = length(unique(df$codigo_int))
#
#   # Separar en train y test
#   set.seed(1234)
#  #  trainindex <- createDataPartition(df %>% pull({{label}}), p=0.8,list=FALSE)
#
#   train <- df %>% filter(origen != 'ix_epf' | (submuestra  %in% 80:82 & origen == 'ix_epf'))
#   print(train %>% nrow)
#   print(train %>% filter(origen == 'ix_epf') %>%  count(submuestra))
#   df_test <- df %>% filter(submuestra >= sm_filtro & submuestra < 80 & origen == 'ix_epf' )
#
#   print(df_test %>% nrow)
#   print(df_test %>% count(submuestra))
#
#   # Pasar todo a formato array
#   y_train <- as.array(train$codigo_int)
#   y_test <- as.array(df_test$codigo_int)
#   x_train <- as.array(train[[text_variable]])
#   x_test <- as.array(df_test[[text_variable]])
#
#   # Tokenizar
#   tokenizer <-  text_tokenizer( oov_token = "OOV")
#   fit_text_tokenizer(tokenizer, x_train)
#
#   #save_text_tokenizer(tokenizer, "data/finales/modelos/modelo_red_seq")
#
#   if (type == "sequences") {
#     # Transformar en secuencias
#     train_sequences = texts_to_sequences(tokenizer, x_train)
#     test_sequences = texts_to_sequences(tokenizer, x_test)
#
#
#
#     # Generar padding
#     x_train <- pad_sequences(train_sequences, padding='post', maxlen=maxlen)
#     x_test <- pad_sequences(test_sequences, padding='post', maxlen=maxlen)
#
#   } else if (type == "tfidf") {
#
#     # Generar tfidf
#     x_train = texts_to_matrix(tokenizer, x_train, mode='tfidf')
#     x_test = texts_to_matrix(tokenizer, x_test, mode = "tfidf")
#   }
#
#
#   #save_text_tokenizer(tokenizer, filename = "scripts/entrenamiento/app/data/tokenizer_red_tfidf")
#
#   output <- list(x_train, y_train,  x_test, y_test, tokenizer, keys, df_test)
#   names(output) <- c("x_train", "y_train",  "x_test", "y_test", "tokenizer", "keys", "df_test")
#   return(output)
#
# }



