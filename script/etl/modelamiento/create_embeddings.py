import gensim 
import numpy as np
from numpy import zeros
# from gensim.models.wrappers import FastText

def crear_matriz_embeddings(vocab_size, dimensiones_modelo, tokenizer, modelo_embeddings):
  # Sacar el tamaño del vocabulario, usando 

  vocab_size =   int(vocab_size)

  # Crear matriz con ceros
  embedding_matrix = zeros((vocab_size, int(dimensiones_modelo)))

  # Para cada palabra dentro de mi vocabulario
  contar = 0
  for word, i in tokenizer.word_index.items():

    # Si el modelo predice algo para la palabra
    if word in modelo_embeddings.wv: 
      # Guardar el embedding en la mtriz de ceros
      
      embedding_vector = modelo_embeddings.wv[word] 
      # print('vector: ', embedding_vector.shape)
      # print('matriz: ', embedding_matrix.shape)
      embedding_matrix[i] = embedding_vector
      
      contar = contar + 1 

      
      
  #x = np.sum(embedding_matrix, axis = 1)
  

      
  print(contar)  
  return embedding_matrix
  # Construir minimos y máximos para cada texto

