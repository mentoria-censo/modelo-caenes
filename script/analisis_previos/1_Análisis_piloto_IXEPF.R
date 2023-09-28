###=========================================================================
###=========================================================================
###                                                                      ===
###                 CODIFICACIÓN OCUPACIÓN BAJO CIUO-08:                 ===
###                      ANÁLISIS BASE PILOTO IX EPF                     ===
###                              2021-08-18                              ===
###                                                                      ===
###=========================================================================
###=========================================================================


# Introducción ------------------------------------------------------------
# El objetivo de este script es comparar las bases piloto y tramtamiento de
# la piloto IX EPF con respecto a la utilización de script para describir 
# oficio y tareas.
# input: 
#        
# output:  


# 000. Librerías y banner -------------------------------------------------

# bannerCommenter::banner("Codificación ocupación bajo CIUO-08:",
#                         "Análisis base piloto IX EPF",
#                         as.character(Sys.Date()),
#                         emph = T,bandChar = "=")
#install.packages('feather')
#install.packages('bannerCommenter')
#install.packages('here')
#install.packages('glue')
#install.packages('tm')
# install.packages('ggpubr')
library(tidyverse)
library(haven)
library(dplyr); library(glue)
source('script/etl/funciones_utiles.R')
source('script/etl/carga_y_procesamiento.R')
submuestras = 91:95


# 001.  Recuperar dataframe -----------------------------------------------
df=armar_dataframe_procesado() 


# 002.  Top 20 de ocupaciones ---------------------------------------------
ciuo = readxl::read_excel('data/input/brutas/ciuo-08-cl-no-modificar.xls') %>%
  rename(aeciuo_2=...2, subgrupo=...3, grupo=...4, glosa=...5) %>%
  filter( !is.na(aeciuo_2) &  is.na(subgrupo) & is.na(grupo)) %>% 
  select("aeciuo_2","glosa") %>%
  mutate(aeciuo_2=as.double(aeciuo_2)) #No tiene todos los códigos del rank

df %>%                                     
  group_by(tipo, aeciuo_2) %>%
  summarise(n=n()) %>%
  slice_max(order_by = n, n=20) %>%
  mutate(order= 1:n())  -> top20

left_join(top20, ciuo, by = "aeciuo_2", na_matched = "na", keep=FALSE) -> top20 

top20 %>% 
  pivot_wider(id_cols=order, 
    names_from = tipo,
    names_sep = ".",
    values_from = c(aeciuo_2, n, glosa)) %>%
  select('order', 'glosa.control', 'n.control', 'glosa.tratamiento', 'n.tratamiento') -> top20_IX     
  #de esta manera podemos hacer una comparación directa entre T y C.
  
readr::write_excel_csv2(top20_IX, 'data/output/presentacion_analisis/top20_IX.csv') 



# 003.  Número de palabras ------------------------------------------------
df %>%
  mutate(oficio_N = sapply(oficio_proc, function(x) length(unlist(strsplit(as.character(x), "\\W+"))))) %>%
  mutate(tareas_N = sapply(tareas_proc, function(x) length(unlist(strsplit(as.character(x), "\\W+"))))) %>%
  mutate(oficio_b_N = sapply(oficio, function(x) length(unlist(strsplit(as.character(x), "\\W+"))))) %>%
  mutate(tareas_b_N = sapply(tareas, function(x) length(unlist(strsplit(as.character(x), "\\W+"))))) -> df



# 004. Distribución -------------------------------------------------------

# g_histograma <- function(data,var,titulo) {
#     data%>%
#     ggplot(aes(x=var)) +
#     geom_histogram(aes(y=..density..),
#                    position = "identity", binwidth = 1,
#                    colour="white", fill="darkturquoise") +
#     labs(title=titulo, x="N palabras", y = "Densidad") +
#     stat_function(fun = dnorm, args = list(mean = mean(data$var), sd = sd(data$var)), colour = "brown3") +
#     facet_wrap(~tipo) +
#     scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12), limits=c(0, 12)) +
#     scale_y_continuous(breaks=c(0.1,0.2,0.3,0.4,0.5), limits=c(0, 0.5)) +
#     theme_gray() -> gr
#   return(gr)
# }
# 
# g_histograma(df, oficio_N, "Histograma variable oficio bruta") -> g1
# g_histograma(df, oficio_b_N , "Histograma variable oficio procesada") -> g2
# ggpubr::ggarrange(g1, g2, ncol = 1, nrow = 2) -> g3

df%>%
  ggplot(aes(x=oficio_N)) +
  geom_histogram(aes(y=..density..),
                 position = "identity", binwidth = 1,
                 colour="white", fill="darkturquoise") +
  labs(title="Histograma variable oficio procesada",
       x="N palabras", y = "Densidad") +
  stat_function(fun = dnorm, args = list(mean = mean(df$oficio_N), sd = sd(df$oficio_N)), colour = "brown3") +
  facet_wrap(~tipo) +
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12), limits=c(0, 12)) +
  scale_y_continuous(breaks=c(0.1,0.2,0.3,0.4,0.5), limits=c(0, 0.5)) +
  theme_gray() -> g1

df%>%
  ggplot(aes(x=oficio_b_N)) +
  geom_histogram(aes(y=..density..),
                 position = "identity", binwidth = 1,
                 colour="white", fill="darkturquoise") +
  labs(title="Histograma variable oficio bruta",
       x="N palabras", y = "Densidad") +
  stat_function(fun = dnorm, args = list(mean = mean(df$oficio_N), sd = sd(df$oficio_N)), colour = "brown3") +
  facet_wrap(~tipo) +
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12), limits=c(0, 12)) +
scale_y_continuous(breaks=c(0.1,0.2,0.3,0.4,0.5), limits=c(0, 0.5)) +
  theme_gray() -> g2

ggpubr::ggarrange(g2, g1, ncol = 1, nrow = 2) ->g3
ggsave("graficos/histograma_oficio.png", plot=g3)

# 005.  Medidas de dispersión ---------------------------------------------
df %>%                                     
  group_by(tipo) %>%
  summarise(oficio_mean=mean(oficio_N),
            oficio_median=median(oficio_N),
            oficio_sd=sd(oficio_N),
            tareas_mean=mean(tareas_N),
            tareas_median=median(tareas_N),
            tareas_sd=sd(tareas_N)) -> sum

readr::write_excel_csv2(sum, 'data/output/presentacion_analisis/summ_proce_IX.csv') 

df %>%                                     
  group_by(tipo) %>%
  summarise(oficio_mean=mean(oficio_b_N),
            oficio_median=median(oficio_b_N),
            oficio_sd=sd(oficio_b_N),
            tareas_mean=mean(tareas_b_N),
            tareas_median=median(tareas_b_N),
            tareas_sd=sd(tareas_b_N)) -> sum_bruta

readr::write_csv2(sum_bruta, 'data/output/presentacion_analisis/summ_bruta_IX.csv') 

# 006. Test de media ------------------------------------------------------
t.test(oficio_N ~ tipo, data = df) #Test de hipótesis no descarta que sean iguales. p-value = 0.1959
t.test(oficio_b_N ~ tipo, data = df) #Test de hipótesis no descarta que sean iguales. p-value = 0.6279



# 007.  Disponibilidad de datos por código CCIF ---------------------------
df %>%                                     
  group_by(aeciuo_2) %>%
  summarise(n=n()) %>%
  arrange(desc(n))-> df2

readr::write_excel_csv2(df2, 'data/output/presentacion_analisis/subgrupos_IX.csv') 


