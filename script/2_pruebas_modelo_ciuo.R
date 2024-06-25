library(httr)
req = GET('https://rapps.ine.cl:9292/download?dataset=ciuo')
df = req$content %>% RcppSimdJson::fparse()
df2 = 'data/modelo_klaus/split_train_test/train_ciuo.feather' %>% read_feather
df3 = 'data/modelo_klaus/split_train_test/test_ciuo.feather' %>% read_feather
df4 = 'data/modelo_klaus/split_train_test/test_ciuo.feather'



modelo = load_model_hdf5('data/modelo_klaus/modelos/modelo_red_emb_gru_ciuo')

