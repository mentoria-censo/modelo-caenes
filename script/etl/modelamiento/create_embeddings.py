import gensim 
import numpy as np
from numpy import zeros
# from gensim.models.wrappers import FastText

def crear_matriz_embeddings(dimensiones_modelo, modelo_embeddings):
  # Sacar el tamaño del vocabulario, usando 

  vocab_size =   int(r.vocab_size)
  print(dimensiones_modelo)
  # Crear matriz con ceros
  embedding_matrix = zeros((vocab_size, int(dimensiones_modelo)))

  # Para cada palabra dentro de mi vocabulario
  for word, i in r.tokenizer.word_index.items():

    # Si el modelo predice algo para la palabra
    if word in modelo_embeddings.wv: 
      # Guardar el embedding en la mtriz de ceros
      
      embedding_vector = modelo_embeddings.wv[word] 
      # print('vector: ', embedding_vector.shape)
      # print('matriz: ', embedding_matrix.shape)
      embedding_matrix[i] = embedding_vector

      
      
  #x = np.sum(embedding_matrix, axis = 1)
  contar = 0

  for w, i in r.tokenizer.word_index.items():
    if w in modelo_embeddings.wv:
      contar = contar + 1 
    
  return embedding_matrix
  # Construir minimos y máximos para cada texto

