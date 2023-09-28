
# 100.  Correr modelo -----------------------------------------------------


#Buscar la manera de guardar los objetos del modelo.
#source("script/2_modelamiento.R", encoding = "utf-8")
df = read_csv2('data/output/y_pred_test.csv')
keys = read_csv2('data/output/keys.csv')

# 200. Matriz de confusión ------------------------------------------------


# Confusion matrix
cm=table(factor(df$y_test, levels=min(df$y_test):max(df$y_test)),
      factor(df$codigo_int, levels=min(df$y_test):max(df$y_test))) %>%
  as.data.frame.matrix() 

#es necesario modificar la matriz hacia un df para poder crear un objeto que sea 
#leídopor geom_tile.

cm %>%
  rownames_to_column('x') %>%
  gather(key="y", value="Positivos", -x) %>%
  mutate(x=as.numeric(x),y=as.numeric(y)) -> cm 

cm %>% 
  left_join(keys, by = c("x" = "codigo_int")) %>%
  left_join(keys, by = c("y" = "codigo_int")) %>%
  select(-x,-y)  %>%
  rename (x=aeciuo_2.x,y=aeciuo_2.y) -> cm #asigno las llaves para poder visualizar el subgrupo

#heatmap  
ggplot(cm, aes(x=factor(x), y=factor(y), fill=Positivos)) + 
  geom_tile() +
  labs(x="subgrupo real",
       y="subgrupo predicho",
       title="Matriz de confusión",
       subtitle="clasificador CIUO-08.CL") +
  geom_text(aes(label = Positivos), colour="white", size=2) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) -> plot1
ggsave("graficos/heat_map.png", plot=plot1)


# 300. Métricas -----------------------------------------------------------

predicted <- factor(df$codigo_int, levels=min(df$y_test):max(df$y_test))
true <- factor(df$y_test, levels=min(df$y_test):max(df$y_test))

metrics=ml_test(predicted, true, output.as.table = TRUE)
metrics %>% 
  mutate(codigo_int=(0:35)) -> metrics
readr::write_excel_csv2(metrics, 'data/output/metricas.csv') 


