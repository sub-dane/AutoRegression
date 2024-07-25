# AutoRegression

En este proyecto se ponen a disponibilidad dos funciones de R que permiten ajustar modelos de regresión lineal, cuantílica, multinivel, random forest, XGBoost y redes neuronales para cualquier conjunto de datos que se ingrese, se realizan la comparación de los resultados según distintas métricas y se exportan gráficas que permiten analizar los resultados obtenidos.

La función principal que realiza el entrenamiento de los distintos modelos se llama \verbatim{AutoRegression} y se encuentra en el código \verbatim{00_funciones.R}. Posterior al entrenamiento de los modelos si se desean obtener las predicciones para un conjunto de datos que no fue usado para el entrenamiento o validación de las métricas de los modelos, se usa la función \verbatim{AutoRegressionPredict} que tiene parámetros muy similares a \verbatim{AutoRegression} y realiza la estimación de la variable objetivo para los modelos que se hayan entrenado.
