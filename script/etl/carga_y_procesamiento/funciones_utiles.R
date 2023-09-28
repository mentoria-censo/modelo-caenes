# explorar_clasificacion = function(producto, columna = "DESCRIPCION_DEL_GASTO"){
#
#   EPF %>% filter(CODIGO_CCIF_01 == producto) %>% select(sym(columna)) %>%
#     count(!! sym(columna)) %>% arrange(desc(n)) %>%
#     gt::gt()
#
# }
#
#
#
# buscar_patron = function(patron){
#
#   EPF %>% filter(str_detect(DESCRIPCION_DEL_GASTO, patron)) %>% select(DESCRIPCION_DEL_GASTO) %>%
#     count(DESCRIPCION_DEL_GASTO) %>% arrange(desc(n)) %>%
#     gt::gt()
#
# }


# FUNCIONES ÚTILES

crear_directorios <- function(){
  dir.create(file.path('data'), showWarnings = FALSE)
  dir.create(file.path('data/input'), showWarnings = FALSE)
  dir.create(file.path('data/input/brutas'), showWarnings = FALSE)
  #dir.create(file.path('data/input/codificadas_entrenamiento'), showWarnings = FALSE)
  #dir.create(file.path('data/input/codificadas_IXEPF'), showWarnings = FALSE)
  dir.create(file.path('data/input/modelos'), showWarnings = FALSE)
  dir.create(file.path('data/output'), showWarnings = FALSE)

  #dir.create(file.path('data/output/codigos_maca'), showWarnings = FALSE)
  #dir.create(file.path('data/output/presentacion_analisis'), showWarnings = FALSE)
  dir.create(file.path('script/etl'), showWarnings = FALSE)
}
