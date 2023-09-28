library(httr)
library(feather)
df <- read_feather("src/data/split_train_test/test.feather")

request <-  httr::POST("http://143.198.79.143:8080/predict",
                       encode = "json",
                       body =  list(text = df$oficio,
                                    classification = "ciuo",
                                    digits = 2)
)

httr::status_code(request)
response <- httr::content(request)
a = response
preds = list()
for(i in response){
  preds = preds %>% append(i[[2]])
}
df$pred_sscc = preds


(df$pred_sscc == df$codigo_cod) %>% sum()
(df$prediccion_modelo == df$codigo_cod) %>% sum()
1799 / 2475
df2 = df %>% filter(submuestra == 82)
((df2$pred_sscc == df2$codigo_cod) %>% sum()) / df2 %>% nrow
((df2$prediccion_modelo == df2$codigo_cod) %>% sum()) / df2 %>% nrow
