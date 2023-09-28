###=========================================================================
###=========================================================================
###                                                                      ===
###                 CODIFICACIÓN OCUPACIÓN BAJO CIUO-08:                 ===
###                        ANÁLISIS BASE VIII EPF                        ===
###                              2021-08-19                              ===
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
#                         "Análisis base VIII EPF",
#                         as.character(Sys.Date()),
#                         emph = T,bandChar = "=")
#install.packages('feather')
#install.packages('bannerCommenter')
#install.packages('here')
#install.packages('glue')
#install.packages('tm')
#install.packages('ggpubr')
#install.packages("readxl")
#install.packages("plyr")
library(tidyverse)
library(haven)
library(readxl)
library(dplyr); library(glue)
source('script/etl/carga_y_procesamiento/funciones_utiles.R')
eval(parse('script/etl/carga_y_procesamiento/carga_y_procesamiento.R', encoding="UTF-8"))
submuestras = 91:95


# 100.  Recuperar dataframe -----------------------------------------------
df_VIII=procesar_viii_epf()
df_IX=armar_dataframe_procesado() %>% 
  filter(tipo=="tratamiento") #mantengo tratamiento porque me interesa comparar
                              #versión papel con versión DMC.

#Los dataframes no tienen la versión de la encuesta, por esto, para distinguirlas
#una vez que haga el append, creo una variable llamada "versión"
df_IX %>%
  mutate(version="IX - tratamiento") -> df_IX
df_VIII %>%
  mutate(version="VIII") -> df_VIII

df <- plyr::rbind.fill(df_IX, df_VIII)

# 200.  Top 20 de ocupaciones VIII---------------------------------------------
ciuo = readxl::read_excel('data/input/brutas/ciuo-08-cl-no-modificar.xls') %>%
  rename(aeciuo_2=...2, subgrupo=...3, grupo=...4, glosa=...5) %>%
  filter( !is.na(aeciuo_2) &  is.na(subgrupo) & is.na(grupo)) %>% 
  select("aeciuo_2","glosa") %>%
  mutate(aeciuo_2=as.double(aeciuo_2)) #No tiene todos los códigos del rank

df_VIII %>%                                     
  group_by(aeciuo_2) %>%
  summarise(n=n()) %>%
  slice_max(order_by = n, n=20) %>%
  mutate(order= 1:n())  -> top20

left_join(top20, ciuo, by = "aeciuo_2", na_matched = "na", keep=FALSE) %>%
  select('order', 'glosa', 'n') -> top20_VIII 

readr::write_excel_csv2(top20_VIII, 'data/output/top20_VIII.csv') 

# 300.  Disponibilidad de datos por código CCIF ---------------------------
df_VIII %>%                                     
  group_by(aeciuo_2) %>%
  summarise(n=n()) %>%
  arrange(desc(n))-> df2
readr::write_excel_csv2(df2, 'data/output/subgrupos_VIII.csv') 

# 400.  Comparación VIII y Tratamiento piloto IX --------------------------
# 410.  Estadísticos ------------------------------------------------------
df %>%
  mutate(oficio_N = sapply(oficio_proc, function(x) length(unlist(strsplit(as.character(x), "\\W+"))))) %>%
  mutate(tareas_N = sapply(tareas_proc, function(x) length(unlist(strsplit(as.character(x), "\\W+"))))) %>%
  mutate(oficio_b_N = sapply(oficio, function(x) length(unlist(strsplit(as.character(x), "\\W+"))))) %>%
  mutate(tareas_b_N = sapply(tareas, function(x) length(unlist(strsplit(as.character(x), "\\W+"))))) -> df

df %>%
  group_by(version) %>%
  summarise(oficio_mean=mean(oficio_N),
            oficio_median=median(oficio_N),
            oficio_sd=sd(oficio_N),
            oficio_max=max(oficio_N),
            oficio_min=min(oficio_N),
            tareas_mean=mean(tareas_N),
            tareas_median=median(tareas_N),
            tareas_sd=sd(tareas_N),
            tareas_max=max(tareas_N),
            tareas_min=min(tareas_N)) -> sum

readr::write_excel_csv2(sum, 'data/output/summ_proce_VIII.csv') 

df %>%
  group_by(version) %>%
  summarise(oficio_mean=mean(oficio_b_N),
            oficio_median=median(oficio_b_N),
            oficio_sd=sd(oficio_b_N),
            oficio_max=max(oficio_b_N),
            oficio_min=min(oficio_b_N),
            tareas_mean=mean(tareas_b_N),
            tareas_median=median(tareas_b_N),
            tareas_sd=sd(tareas_b_N),
            tareas_max=max(tareas_b_N),
            tareas_min=min(tareas_b_N)) -> sum

readr::write_excel_csv2(sum, 'data/output/summ_VIII.csv') 

# 420.  Histogramas -------------------------------------------------------
df%>%
  ggplot(aes(x=oficio_N)) +
  geom_histogram(aes(y=..density..),
                 position = "identity", binwidth = 1,
                 colour="white", fill="darkturquoise") +
  labs(title="Histograma variable oficio procesada",
       x="N palabras", y = "Densidad") +
  stat_function(fun = dnorm, args = list(mean = mean(df$oficio_N), sd = sd(df$oficio_N)), colour = "brown3") +
  facet_wrap(~version) +
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12), limits=c(0, 12)) +
  scale_y_continuous(breaks=c(0.1,0.2,0.3,0.4,0.5,0.6), limits=c(0, 0.6)) +
  theme_gray() -> g1

df%>%
  ggplot(aes(x=oficio_b_N)) +
  geom_histogram(aes(y=..density..),
                 position = "identity", binwidth = 1,
                 colour="white", fill="darkturquoise") +
  labs(title="Histograma variable oficio bruta",
       x="N palabras", y = "Densidad") +
  stat_function(fun = dnorm, args = list(mean = mean(df$oficio_N), sd = sd(df$oficio_N)), colour = "brown3") +
  facet_wrap(~version) +
  scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12), limits=c(0, 12)) +
  scale_y_continuous(breaks=c(0.1,0.2,0.3,0.4,0.5,0.6), limits=c(0, 0.6)) +
  theme_gray() -> g2

#Se excluyeron 3 datos de la VIII que tenían más de 12 palabras (14, 17 y 20 respectivamente).
ggpubr::ggarrange(g2, g1, ncol = 1, nrow = 2) ->g3
ggsave("graficos/histograma_oficio_VIII_IX.png", plot=g3)


# 430. Test de medias -----------------------------------------------------
t.test(oficio_N ~ version, data = df) #Test de hipótesis no descarta que sean iguales. p-value = 0.7488
t.test(oficio_b_N ~ version, data = df) #Test de hipótesis no descarta que sean iguales. p-value = 0.122

