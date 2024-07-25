# AutoRegression

En este proyecto se ponen a disponibilidad dos funciones de R que permiten ajustar modelos de regresión lineal, cuantílica, multinivel, random forest, XGBoost y redes neuronales para cualquier conjunto de datos que se ingrese, se realizan la comparación de los resultados según distintas métricas y se exportan gráficas que permiten analizar los resultados obtenidos.

La función principal que realiza el entrenamiento de los distintos modelos se llama `AutoRegression` y se encuentra en el código `00_funciones.R`. Posterior al entrenamiento de los modelos si se desean obtener las predicciones para un conjunto de datos que no fue usado para el entrenamiento o validación de las métricas de los modelos, se usa la función `AutoRegressionPredict` que tiene parámetros muy similares a `AutoRegression` y realiza la estimación de la variable objetivo para los modelos que se hayan entrenado.

## Explicación proceso

## Ejemplo de uso para entrenamiento de modelos

```r
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
  .folder = "ejemplo_precios",
  
  ## Sufijo para guardar resultados
  .suffix = "01"
)
```

## Ejemplo de uso para predicción
```r
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
  .folder = "ejemplo_precios",
  
  ## Sufijo para guardar resultados
  .suffix = "01"
)

```
