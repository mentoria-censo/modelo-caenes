import gensim 
import numpy as np
from numpy import zeros
from gensim.models import fasttext


def get_modelo(tipo_modelo):
    if tipo_modelo == 'chico':
        wordvectors2 = fasttext.load_facebook_model('data/input/modelos/embeddings-s-model.bin') 
    elif tipo_modelo == 'neo-grande':
        wordvectors2 = fasttext.load_facebook_model('data/input/modelos/embeddings-new_large-general_3B_fasttext.bin')
        
    elif tipo_modelo == 'grande':
        wordvectors2 = fasttext.load_facebook_model('data/input/modelos/embeddings-l-model.bin')
    else:
        return print('Error: ingreso tipo_modelo "chico" o "grande"')

    return wordvectors2



