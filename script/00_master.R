###========================================================================
###========================================================================
###                                                                     ===
###                  CODIFICACIÓN AUTOMATIZADA CIUO-08                  ===
###                            MASTER SCRIPT                            ===
###                              2021-11-27                             ===
###                                                                     ===
###========================================================================
###========================================================================

# bannerCommenter::banner("Codificación automatizada CIUO-08",
#                         "Master script",
#                         as.character(Sys.Date()),
#                         emph = T,bandChar = "=")



# 000. Librerías  ---------------------------------------------------------

instalar_ambiente = function(){

  pak::pak("rstudio/reticulate")
  tensorflow::install_tensorflow()
  reticulate::virtualenv_install('r-tensorflow', packages = 'gensim')


}

# remotes::install_github("rstudio/tensorflow")
# reticulate::import('asyncio')
# reticulate::source_python('C:/Users/Jeconchao/Documents/.virtualenvs/r-tensorflow/Lib/site-packages/asyncio/__init__.py')
# reticulate::conda_create('r-tensorflow', packages = c('tensorflow', 'gensim'))
# reticulate::install_miniconda()
# tensorflow::install_tensorflow()
# keras::install_keras()
# keras::install_keras(method = 'conda')

reticulate::use_virtualenv('r-tensorflow', required = T)
if(!require(Rfast)) install.packages("Rfast") else require(Rfast)
if(!require(readxl)) install.packages("readxl") else require(readxl)
if(!require(dplyr)) install.packages("dplyr") else require(dplyr)
if(!require(haven)) install.packages("haven") else require(haven)
if(!require(tidyverse)) install.packages("tidyverse") else require(tidyverse)
library(glue); library(tidyverse)
if(!require(caret)) install.packages("caret") else require(caret)
if(!require(keras)) install.packages("keras") else require(keras)
if(!require(tensorflow)) install.packages("tensorflow") else require(tensorflow)
if(!require(renv)) install.packages("renv") else require(renv)
if(!require(rlang)) install.packages("rlang") else require(rlang)
if(!require(ggplot2)) install.packages("ggplot2") else require(ggplot2)
if(!require(reshape2)) install.packages("reshape2") else require(reshape2)
if(!require(readr)) install.packages("readr") else require(readr)
if(!require(mltest)) install.packages("mltest") else require(mltest)
if(!require(feather)) install.packages("feather") else require(feather)
if(!require(tm)) install.packages("tm") else require(tm)
if(!require(janitor)) install.packages("janitor") else require(janitor)
if(!require(reticulate)) install.packages("reticulate") else require(reticulate)
if(!require(writexl)) install.packages("writexl") else require(writexl)


# 000. Submuestra a ejecutar ----------------------------------------------



# 001. Funciones ----------------------------------------------------------
source('script/etl/carga_y_procesamiento/funciones_utiles.R')

crear_directorios()
eval(parse('script/etl/carga_y_procesamiento/carga_y_procesamiento.R', encoding="UTF-8"))
source("script/etl/modelamiento/helpers_training.R", encoding = "utf-8")


# 002. Modelamiento -------------------------------------------------------

source('script/1a_modelamiento.R')



# 004. Prediccion datos nuevos (Obsoleto)

#source('script/4b_prediccion_datos_nuevos_doble_entrada.R')



