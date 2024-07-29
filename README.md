# AutoRegression

En este proyecto se ponen a disponibilidad dos funciones de R que permiten ajustar modelos de regresión lineal, cuantílica, multinivel, random forest, XGBoost y redes neuronales para cualquier conjunto de datos que se ingrese, se realizan la comparación de los resultados según distintas métricas y se exportan gráficas que permiten analizar los resultados obtenidos.

La función principal que realiza el entrenamiento de los distintos modelos se llama `AutoRegression` y se encuentra en el código `00_funciones.R`. Posterior al entrenamiento de los modelos si se desean obtener las predicciones para un conjunto de datos que no fue usado para el entrenamiento o validación de las métricas de los modelos, se usa la función `AutoRegressionPredict` que tiene parámetros muy similares a `AutoRegression` y realiza la estimación de la variable objetivo para los modelos que se hayan entrenado.

## Explicación proceso

_Gráfico del proceso_

* Preparación de las variables
* Entrenamiento de los modelos
  * LM: Intervalos de predicción común y corriente
  * CUANTILICA: Intervalos con los cuantiles
  * RF: Intervalos con los resultados de todos los árboles
  * XGBoost: Intervalos con múltiples modelos generados boostraping
  * Multinivel: Intervalos común y corriente
  * NN: Intervalos con múltiples modelos generados con boostraping
* Explicación resultados exportados
  * Modelos
  * Gráficas de residuales
  * Obs vs Pred
  * Coeficientes
  * Métricas: exactitud, sesgo, etc.
* Comparación modelos

## Ejemplo de uso para entrenamiento de modelos

Los parámetros que tiene la función para ajustar los diversos modelos son los siguientes:

* **df.train**: Dataframe con los datos para entrenar y ajustar los modelos, en caso que no se tenga la partición de los datos de esta forma se puede utilizar la función `sample` de R. Este dataframe debe contener una columna llamada `id` la cual debe ser un identificador único de cada uno de los registros, esto con el fin de poder realizar los cruces entre los resultados de los modelos y hacer la comparación de los mismos.
* **df.test**: Dataframe con los datos para validar los resultados de los modelos. Igual que en el caso de `df.train` este data.frame debe contener una variable identificadora nombrada `id`.
* **modelo.lm**: Valor lógico (TRUE o FALSE) dependiendo si se desea ajustar un modelo de regresión OLS.
* **modelo.cuantilica**: Valor lógico (TRUE o FALSE) dependiendo si se desea ajustar un modelo de regresión cuantílica que modela principalmente la mediana y cuantiles 0.025, 0.05, 0.975 y 0.995 para obtener los intervalos de predicción.
* **modelo.rf**: Valor lógico (TRUE o FALSE) dependiendo si se desea ajustar un modelo random forest.
* **modelo.xgboost**: Valor lógico (TRUE o FALSE) dependiendo si se desea ajustar un modelo XGBoost.
* **modelo.lm.multinivel**: Valor lógico (TRUE o FALSE) dependiendo si se desea ajustar un modelo de regresión OLS jerárquico.
* **modelo.nn**: Valor lógico (TRUE o FALSE) dependiendo si se desea ajustar un modelo de redes neuronales superficiales (de una sola capa).
* **var.y**: Caracter con el nombre de la variable de respuesta a modelar que se encuentra presente en el conjunto de entrenamiento y prueba.
* **var.log**: Vector de valores de tipo caracter con el nombre de las variables explicativas que se les desea aplicar una tansformación logarítmica. Esto suele ser de utilidad cuando se tienen variables con distribuciones sesgadas, por ejemplo, variables relacionadas con conteos o dinero. En caso de que no haya ninguna variable que se le desee aplicar esta transformación, se deja `NULL`.
* **var.sqrt**: Vector de valores de tipo caracter con el nombre de las variables explicativas que se les desea aplicar una tansformación de raíz cuadrada. Similar al caso de la transformación logarítmica esto se suele usar cuando se tienen variables con distribuciones sesgadas. En caso de que no haya ninguna variable que se le desee aplicar esta transformación, se deja `NULL`.
* **var.otras**: Vector de valores tipo caracter con el nombre de las variables explicativas que no se les desea aplicar alguna transformación de las mencionadas o estandarización. En este caso se suelen ingresar variables categóricas, donde estas deben ser de tipo `character` y no `factor`. En caso de que no haya ninguna variable que se le desee aplicar esta transformación, se deja `NULL`.
* **var.e**: Vector de valores de tipo caracter con el nombre de las variables explicativas continuas que se desean estandarizar y no aplicar alguna transformación logarítmica o de raíz cuadrada. En caso de que no haya ninguna variable que se le desee aplicar esta transformación, se deja `NULL`.
* **var.multinivel**: Valor de tipo caracter con la variable explicativa que se desea ingresar para obtener el nivel o jerarquía en la regresión multinivel. En caso de que no haya ninguna variable que se le desee aplicar esta transformación, se deja `NULL`.
* **.folder**: Valor de tipo caracter con el nombre de la carpeta principal donde se desean guardar los resultados exportados por los modelos.
* **.suffix**: Valor de tipo caracter con el nombre de la carpeta secundaria donde se desean guardar los resultados exportados por los modelos. Esto puede ser de utilidad si se desean ajustar modelos haciendo cambios en las variables ingresadas o alguno de los parámetros de la función, con el fin de tener variables versiones del modelamiento en una misma carpeta principal.

Una vez se establecen todos los parámetros mencionados anteriormente a gusto, se puede ejecutar la función, la cual generara métricas de evaluación y gráficas para analizar el desempeño de los modelos seleccionados. De esta forma, esta función no retorna algún objeto en el ambiente de R, sino que exporta los objetos que son de interés.

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

Una vez se entrenan los modelos, si se desean usar los modelos entrenados para obtener las predicciones en un conjunto de datos que no fue ingresado en el entrenamiento o validación del mismo, se usa la función `AutoRegressionPredict` la cual tiene los mismos parámetros que la función `AutoRegression` pero en este caso se puede seleccionar un subconjunto de los modelos de los que se desea obtener una predicción y el data.frame `df.test` contiene los datos donde se deben ingresar las predicciones. 

Es importante verificar que los parámetros ingresados en esta función, a excepción de los parámetros de los modelos entrenados, deben ser iguales que en la función `AutoRegression` ya que esto permite que se logren cargar y usar los resultados exportados.

```r
df.estimaciones = AutoRegressionPredict(
  ##=========== Datos
  df.test = df.prueba,
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
  var.log = c("CONTEO_TIENDAS","DIS_MERCADO"), # En caso de que no haya ninguna, dejar vacío (NULL)
  
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
