######################################
# TRAINING WITH DIFFERENT STRATEGIES #
######################################

# Cargar el modelo de fasttext

 # install.packages('reticulate')
 # install.packages('Rcpp')
 # install.packages('keras')

library(reticulate)

reticulate::source_python("scripts/entrenamiento/red/load_model.py")

 #-----------------#
# CAENES 2 digits #  
#-----------------#

# # feed forward network based on sequences
# source("scripts/entrenamiento/red/train_caenes_seq.R", encoding = "utf-8")
# 
# # feed forward network based on tfidf
# source("scripts/entrenamiento/red/train_caenes_tfidf.R", encoding = "utf-8")
# 
# # remove all objects
# rm(list = ls())
# 
# # feed forward network + word embeddings
# source("scripts/entrenamiento/red/train_caenes_emb_inside.R", encoding = "utf-8")

# gru architecture + word embeddings
source("scripts/entrenamiento/red/train_caenes_emb_inside_gru.R", encoding = "utf-8")


#----------------#
# CAENES 1 digit #  
#----------------#

# feed forward network based on sequences
source("scripts/entrenamiento/red/train_caenes_seq_1d.R", encoding = "utf-8")

# feed forward network based on tfidf
source("scripts/entrenamiento/red/train_caenes_tfidf_1d.R", encoding = "utf-8")

# remove all objects
rm(list = ls())

# feed forward network + word embeddings
source("scripts/entrenamiento/red/train_caenes_emb_inside_1d.R", encoding = "utf-8")

# gru architecture + word embeddings
source("scripts/entrenamiento/red/train_caenes_emb_inside_gru_1d.R", encoding = "utf-8")


