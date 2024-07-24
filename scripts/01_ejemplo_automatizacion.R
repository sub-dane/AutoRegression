cat("014")
options(scipen = 999)
rm(list = ls(all = T))

library(dplyr)

source("00_funciones.R", encoding = "UTF-8")

#===============================================================#
#           1. Ejemplo: Predecir precios de vinos          ######
#===============================================================#
## Cargar datos de entrenamiento y prueba
uu = readRDS("F:/One_Drive/OneDrive - dane.gov.co/Proyectos/03_Licores/02_out/01_data/data_train_test_whisky_vt.rds")

df.entrenamiento = uu$train
df.prueba = uu$test

rm(uu)
gc()

## Sacar una muestra más pequeña para que no demore mucho
set.seed(465)
df.entrenamiento = df.entrenamiento[sample(1:nrow(df.entrenamiento),50000),]
df.entrenamiento$log_PRECIO_UNIDAD = log(df.entrenamiento$PRECIO_UNIDAD)
df.prueba$log_PRECIO_UNIDAD = log(df.prueba$PRECIO_UNIDAD)

## Ajuste de varios modelos
AutoRegression(
  ##=========== Datos
  df.train = df.entrenamiento,
  df.test = df.prueba,
  ##============ Modelos a entrenar (En caso de que no se quiera entrenar alguno, dejar FALSE)
  modelo.lm = T,
  modelo.cuantilica = T,
  modelo.rf = T,
  modelo.xgboost = T,
  modelo.lm.multinivel = T,
  modelo.nn = T,
  
  ##============= Variables
  ## Variable respuesta
  var.y = "log_PRECIO_UNIDAD",
  
  ## Variables para calcular con logaritmo
  var.log = c("CONTEO_TIENDAS","DIS_MERCADO"), # En caso de que no haya ninguna, dejar vacío, ej: c("")
  
  ## Variables para raíz cuadrada
  var.sqrt = NULL, # En caso de que no haya ninguna, dejar vacío, es decir: NULL
  
  ## Variables categóricas u otras que no entran con transformación para añadir al modelo 
  # Nota: Las categóricas tienen que ser de tipo caracter
  var.otras = c("CLASE_FUENTE","MARCA_HOMOLOGADA","DEPARTAMENTO"),
  
  ## Variables para estandarizar sin incluir las variables de log o sqrt
  var.e = c("ANOS_ANADAS","GRADOS_ALCOHOL"),
  
  ## Variable para modelo multinivel
  var.multinivel = "PAIS_HOMOLOGADO",
  
  ##============== Otros
  ## Nombre para crear carpeta principal con resultados
  .folder = "01_ejemplo_whisky",
  
  ## Sufijo para guardar resultados
  .suffix = "05"
)

#===========================#
# Predicción de los modelos #
#===========================#
df.estimaciones = AutoRegressionPredict(
  ##=========== Datos
  df.test = df.prueba[c(1,500,132,18452,32147),],
  ##============ Modelos con los que se quiere obtener la predicción
  modelo.lm = T,
  modelo.cuantilica = T,
  modelo.rf = T,
  modelo.xgboost = T,
  modelo.lm.multinivel = T,
  modelo.nn = T,
  
  ##============= Variables
  ## Variable respuesta
  var.y = "log_PRECIO_UNIDAD",
  
  ## Variables para calcular con logaritmo
  var.log = c("CONTEO_TIENDAS","DIS_MERCADO"), # En caso de que no haya ninguna, dejar vacío, ej: c("")
  
  ## Variables para raíz cuadrada
  var.sqrt = NULL, # En caso de que no haya ninguna, dejar vacío, es decir: NULL
  
  ## Variables categóricas u otras que no entran con transformación para añadir al modelo 
  # Nota: Las categóricas tienen que ser de tipo caracter
  var.otras = c("CLASE_FUENTE","MARCA_HOMOLOGADA","DEPARTAMENTO"),
  
  ## Variables para estandarizar sin incluir las variables de log o sqrt
  var.e = c("ANOS_ANADAS","GRADOS_ALCOHOL"),
  
  ## Variable para modelo multinivel
  var.multinivel = "PAIS_HOMOLOGADO",
  
  ##============== Otros
  ## Nombre para crear carpeta principal con resultados
  .folder = "01_ejemplo_whisky",
  
  ## Sufijo para guardar resultados
  .suffix = "05"
)
