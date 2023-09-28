
bannerCommenter::banner("Codificación IX EPF", 
                        "CIUO", 
                        paste("Fecha: ", as.character(Sys.Date())), 
                        R.version.string, bandChar="=")


# 000. Encoding -----------------------------------------------------------

options(encoding = "utf-8")


# 001. Librerias y funciones ----------------------------------------------
if(!require(dplyr)) install.packages("dplyr") else require(dplyr)
if(!require(tidyverse)) install.packages("tidyverse") else require(tidyverse)
if(!require(haven)) install.packages("haven") else require(haven)
if(!require(feather)) install.packages("feather") else require(feather)
if(!require(janitor)) install.packages("janitor") else require(janitor)
if(!require(glue)) install.packages("glue") else require(glue)
if(!require(readxl)) install.packages("readxl") else require(readxl)
if(!require(RTextTools)) install.packages("RTextTools") else require(RTextTools)

source('script/etl/carga_y_procesamiento/funciones_utiles.R')
crear_directorios()
eval(parse('script/etl/carga_y_procesamiento/carga_y_procesamiento.R', encoding="UTF-8"))
source("script/etl/modelamiento/helpers_training.R", encoding = "utf-8")


# 002. Submuestra ---------------------------------------------------------
sm_exec <- 80 #esta es la sm a predecir


# 003. Cargar datos procesados --------------------------------------------
df = armar_df_modelamiento_completo(datos_a_utilizar = 'todo')


# 004.  Crear matriz de texto ---------------------------------------------
dtMatrix <- create_matrix(cbind(df$oficio_proc,df$aeciuo_2))