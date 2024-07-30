library(stringr)
library(dplyr)
library(stringr)
library(quantreg) # Regresión Cuantílica
library(xgboost) # XGBoost
library(randomForest) # Random Forest
library(ggplot2)
library(multilevel)
library(nnet)

#==================================================#
#    Función para limpieza de caracteres       #####
#==================================================#
CleanChar = function(x){
  stop_words = stopwords::stopwords(language = "spanish") %>% toupper
  stop_words = str_replace_all(stop_words, "Á", "A")
  stop_words = str_replace_all(stop_words, "É", "E")
  stop_words = str_replace_all(stop_words, "Í", "I")
  stop_words = str_replace_all(stop_words, "Ó", "O")
  stop_words = str_replace_all(stop_words, "Ú", "U")
  stop_words = paste0("\\b",stop_words, "\\b",collapse = "|")
  
  y = toupper(x)
  
  ## Corregir tildes
  y = str_replace_all(y, "Á", "A")
  y = str_replace_all(y, "É", "E")
  y = str_replace_all(y, "Í", "I")
  y = str_replace_all(y, "Ó", "O")
  y = str_replace_all(y, "Ú", "U")
  
  y = str_replace_all(y, "À", "A")
  y = str_replace_all(y, "È", "E")
  y = str_replace_all(y, "Ì", "I")
  y = str_replace_all(y, "Ò", "O")
  y = str_replace_all(y, "Ù", "U")
  
  y = str_replace_all(y, "Ñ", "N")
  y = str_replace_all(y, "Ä", "A")
  y = str_replace_all(y, "Ë", "E")
  y = str_replace_all(y, "Ï", "I")
  y = str_replace_all(y, "Ö", "O")
  y = str_replace_all(y, "Ü", "U")
  y = str_replace_all(y, "Â", "A")
  y = str_replace_all(y, "Ç", "C")
  
  y = str_replace_all(y, "&", " & ")
  y = str_replace_all(y, " & ", " Y ")
  
  y = str_replace_all(y, ":", " ")
  y = str_replace_all(y, "-", " ")
  y = str_replace_all(y, "'", " ")
  y = str_replace_all(y, "\\.", " ")
  y = str_replace_all(y, "´", " ")
  y = str_replace_all(y, "`", " ")
  
  ## Eliminar stopwords y palabras que no aportan
  y = str_replace_all(y, stop_words, " ")
  y = str_replace_all(y, "[:punct:]", " ")
  y = str_replace_all(y, paste0("\\b", c("VINO", "RESERVA", "BODEGA","FAMILIA", "RESERVE", "VIN", "MARCA", "CHAMPAGNE", "APERITIVO", "VINOS", "WHISKY","WHISKEY"), "\\b", collapse = "|"), " ")
  
  y = str_squish(y)
  
  return(y)
}

CleanCharMatrix = function(X){
  colnames(X) = str_remove_all(colnames(X), "\\(|\\)|\\.|\\/") %>% tolower() %>% str_squish()
  colnames(X) = str_replace_all(colnames(X), " ", "_")
  colnames(X) =  str_replace_all(colnames(X), ":", "_")
  colnames(X) = str_replace_all(colnames(X), "á", "a")
  colnames(X) = str_replace_all(colnames(X), "é", "e")
  colnames(X) = str_replace_all(colnames(X), "í", "i")
  colnames(X) = str_replace_all(colnames(X), "ó", "o")
  colnames(X) = str_replace_all(colnames(X), "ú", "u")
  
  return(X)
}

#=============================#
# Función cálculo métricas ####
#=============================#
Metrics = function(obs, est, lower, upper){
  df.res = data.frame(
    ## Sesgo
    bias = mean( (est - obs)/est ),
    
    ## Imprecisión
    impre = sd((est - obs)/est ),
    
    ## Inexactitud
    inacc = mean( abs((est - obs)/est) ),
    
    ## Error en logaritmo
    rmse = mean( (obs - est)^2) %>% sqrt,
    
    ## Pseudo-R2
    r2 = 1 - sum((est - obs)^2)/sum((obs - mean(obs))^2),
    # r2 = 1 - sum((est - mean(obs))^2)/sum((obs - mean(obs))^2),
    
    ## Porcentaje valores dentro de intervalo
    porc.int = sum(between(obs, lower, upper))/length(obs)
  )
  
  return(df.res)
}

MetricsFull = function(df, df.entrenamiento, centralidad){
  #============================================================#
  # df: dataframe con datos de prueba
  # df.entrenamiento: dataframe con datos de entrenamiento
  #============================================================#
  
  alpha = 1 - centralidad
  
  # Definir observaciones centrales
  df = df %>% 
    group_by(KEY_PROD) %>% 
    mutate(
      CENTRAL = case_when(
        PRECIO_UNITARIO >= quantile(PRECIO_UNITARIO, probs = alpha/2) & 
          PRECIO_UNITARIO <= quantile(PRECIO_UNITARIO, probs = 1 - alpha/2) ~ "CENTRAL",
        TRUE ~ "NO CENTRAL"
      )
    ) %>% 
    as.data.frame()
  
  rbind(
    cbind("tipo" = "log train", Metrics(
      obs = df.entrenamiento$PRECIO_UNIDAD %>% log, 
      est = df.entrenamiento$fitted,
      lower = df.entrenamiento$lower,
      upper = df.entrenamiento$upper )),
    
    cbind("tipo" = "log", Metrics(
      obs = df$PRECIO_UNIDAD %>% log, 
      est = df$fitted,
      lower = df$lower,
      upper = df$upper )),
    
    cbind("tipo" = "exp", Metrics(
      obs = df$PRECIO_UNIDAD,
      est = df$fitted %>% exp, 
      lower = df$lower %>% exp,
      upper = df$upper %>% exp)),
    
    cbind("tipo" = "final", Metrics(
      obs = df$PRECIO_UNIDAD*df$CANTIDAD_HOMOLOGADA,
      est = exp(df$fitted)*df$CANTIDAD_HOMOLOGADA, 
      lower = exp(df$lower)*df$CANTIDAD_HOMOLOGADA,
      upper = exp(df$upper)*df$CANTIDAD_HOMOLOGADA )),
    
    cbind("tipo" = "final <100k", Metrics(
      obs = (df$PRECIO_UNIDAD*df$CANTIDAD_HOMOLOGADA)[df$PRECIO_UNITARIO < 100000] ,
      est = (exp(df$fitted)*df$CANTIDAD_HOMOLOGADA)[df$PRECIO_UNITARIO < 100000] , 
      lower = (exp(df$lower)*df$CANTIDAD_HOMOLOGADA)[df$PRECIO_UNITARIO < 100000] ,
      upper = (exp(df$upper)*df$CANTIDAD_HOMOLOGADA)[df$PRECIO_UNITARIO < 100000]) ),
    
    cbind("tipo" = paste0("final central ", 100*centralidad), Metrics(
      obs = (df$PRECIO_UNIDAD*df$CANTIDAD_HOMOLOGADA)[df$CENTRAL == "CENTRAL"] ,
      est = (exp(df$fitted)*df$CANTIDAD_HOMOLOGADA)[df$CENTRAL == "CENTRAL"] , 
      lower = (exp(df$lower)*df$CANTIDAD_HOMOLOGADA)[df$CENTRAL == "CENTRAL"] ,
      upper = (exp(df$upper)*df$CANTIDAD_HOMOLOGADA)[df$CENTRAL == "CENTRAL"]) ),
    
    cbind("tipo" = paste0("final NO central ", 100*centralidad), Metrics(
      obs = (df$PRECIO_UNIDAD*df$CANTIDAD_HOMOLOGADA)[df$CENTRAL != "CENTRAL"] ,
      est = (exp(df$fitted)*df$CANTIDAD_HOMOLOGADA)[df$CENTRAL != "CENTRAL"] , 
      lower = (exp(df$lower)*df$CANTIDAD_HOMOLOGADA)[df$CENTRAL != "CENTRAL"] ,
      upper = (exp(df$upper)*df$CANTIDAD_HOMOLOGADA)[df$CENTRAL != "CENTRAL"]) )
  )
}

MetricsAuto = function(df, df.entrenamiento, y.name){
  #============================================================#
  # df: dataframe con datos de prueba
  # df.entrenamiento: dataframe con datos de entrenamiento
  #============================================================#
  
  cbind("tipo" = "escala original", Metrics(
    obs = df.entrenamiento[,y.name], 
    est = df.entrenamiento$fitted,
    lower = df.entrenamiento$lower,
    upper = df.entrenamiento$upper ))
}

#===================================================================#
#    Función para obtener intervalos de confianza LME         #######
#===================================================================#
PredLME = function(modelo, df, subset = NULL, zvalue = 1.96, formula.str, var.random){
  # browser()
  ## Matriz de diseño
  # pred = predict(modelo, df)
  # browser()
  pred = ManualPredictLME(modelo, df, formula.str, var.random)
  Designmat <- model.matrix(as.formula(formula.str), df)
  
  if(!is.null(subset)){
    pred = pred[subset:length(pred)]
    Designmat = Designmat[subset:nrow(Designmat),]
  }
  
  ## Predicciones de a 10K para evitar problemas de memoria
  s1 = seq(1, nrow(Designmat), by = min(10000, nrow(Designmat)) ) 
  s2 = seq(min(10000, nrow(Designmat)), nrow(Designmat), by = min(10000, nrow(Designmat)) ) %>% c(.,nrow(Designmat)) 
  
  lower = NULL
  upper = NULL
  
  for(i in 1:length(s1) ){
    Designmat.temp = Designmat[s1[i]:s2[i],]
    
    ## Varianza de las predicciones
    predvar <- diag(Designmat.temp %*% modelo$varFix %*% t(Designmat.temp))
    # se <- sqrt(predvar) 
    se2 <- sqrt(predvar + modelo$sigma^2)
    
    ## Intervalos de confianza
    # lower = pred[s1[i]:s2[i]] - 1.96*se
    # upper = pred[s1[i]:s2[i]] + 1.96*se
    
    ## Intervalos de la predicción
    lower = c(lower, pred[s1[i]:s2[i]] - zvalue*se2)
    upper = c(upper, pred[s1[i]:s2[i]] + zvalue*se2)
    
    # cat("Realizado: ", i, " de ", length(s1), "\n")
    
  }
  
  return(list(pred = pred, lower = lower, upper = upper))
}

## Función para calcular la moda #######
FMode <- function(v) {
  if(is.null(v) | sum(!is.na(v)) == 0){
    return(NA)
  } else{
    uniqv <- unique(v)
    return(uniqv[which.max(tabulate(match(v, uniqv)))])
  }
}

## Función para organizar datos de entrenamiento y prueba #######
PrepVariables = function(data.train, data.test, var.e, var.sqrt, var.otras, var.log, .folder, .suffix){
  #=============================================================================#
  # Función para organizar, estandarizar y transformar las variables ingresadas
  # posteriormente a los modelos
  #=============================================================================#
  if(!is.null(data.train)){
    ## Crear copia de los datos
    df.train.lm = data.train
    df.test.lm = data.test
    
    ## Variables para estandarizar
    var.e = c(paste0("LOG_", var.log),paste0("SQRT_", var.sqrt), var.e)
    var.e = var.e[!str_detect(var.e, "^LOG_$") & !str_detect(var.e, "^SQRT_$") ]
    
    ## Variables modelo
    var.modelo = c(var.otras, var.e)
    
    ## Calcular logaritmo
    if(length(var.log) > 0){
      for(i in var.log){
        df.train.lm[,paste0("LOG_",i)] = df.train.lm[,i] 
        df.test.lm[,paste0("LOG_",i)] = df.test.lm[,i] 
      }
    }
    
    ## Calcular raíz cuadrada
    if(length(var.sqrt) > 0){
      for(i in var.sqrt){
        df.train.lm[,paste0("SQRT_",i)] = df.train.lm[,i] 
        df.test.lm[,paste0("SQRT_",i)] = df.test.lm[,i] 
      }
    }
    
    ## Estandarizar variables
    mu_e = apply(df.train.lm[,var.e], 2, mean)
    sd_e = apply(df.train.lm[,var.e], 2, sd)
    
    df.train.lm[,var.e] = sweep(df.train.lm[,var.e], MARGIN = 2, STATS = mu_e, FUN = "-")
    df.train.lm[,var.e] = sweep(df.train.lm[,var.e], MARGIN = 2, STATS = sd_e, FUN = "/")
    df.test.lm[,var.e] = sweep(df.test.lm[,var.e], MARGIN = 2, STATS = mu_e, FUN = "-")
    df.test.lm[,var.e] = sweep(df.test.lm[,var.e], MARGIN = 2, STATS = sd_e, FUN = "/")
    
    ## Guardar media y desviación de entrenamiento 
    list.out = list(mu_train = mu_e, sigma_train = sd_e)
    
    ## Guardar categorías de entrenamiento y prueba
    for(i in var.modelo){
      if(is.character(df.train.lm[,i]) | is.factor(df.train.lm[,i])){
        list.out[[i]] = unique(df.train.lm[,i] %>% sort %>% as.character())
      }
    }
    
    ## Exportar media y desviación
    saveRDS(list.out, paste0("out/",.folder,"/vt_",.suffix,"/parametros_variables.rds"))
    
    return(list(df.train.lm = df.train.lm, df.test.lm = df.test.lm, var.modelo = var.modelo))
  } else{
    ## Crear copia de los datos
    df.test.lm = data.test
    
    ## Variables para estandarizar
    var.e = c(paste0("LOG_", var.log),paste0("SQRT_", var.sqrt), var.e)
    var.e = var.e[!str_detect(var.e, "^LOG_$") & !str_detect(var.e, "^SQRT_$") ]
    
    ## Variables modelo
    var.modelo = c(var.otras, var.e)
    
    ## Calcular logaritmo
    if(length(var.log) > 0){
      for(i in var.log){
        df.test.lm[,paste0("LOG_",i)] = df.test.lm[,i] 
      }
    }
    
    ## Calcular raíz cuadrada
    if(length(var.sqrt) > 0){
      for(i in var.sqrt){
        df.test.lm[,paste0("SQRT_",i)] = df.test.lm[,i] 
      }
    }
    
    ## Estandarizar variables
    parametros = readRDS(paste0("out/",.folder,"/vt_",.suffix,"/parametros_variables.rds"))
    
    mu_e = parametros$mu_train
    sd_e = parametros$sigma_train
    
    df.test.lm[,var.e] = sweep(df.test.lm[,var.e], MARGIN = 2, STATS = mu_e, FUN = "-")
    df.test.lm[,var.e] = sweep(df.test.lm[,var.e], MARGIN = 2, STATS = sd_e, FUN = "/")
    
    ## Crear variables categóricas como factores
    for(i in names(parametros)){
      if(i %in% var.modelo){
        df.test.lm[,i] = factor(df.test.lm[,i] %>% as.character, levels = parametros[[i]])
      }
    }
  
    return(list(df.test.lm = df.test.lm, var.modelo = var.modelo))
  }
  
}

#===========================================#
# Función para entrenar redes neuronales ####
#===========================================#
train_evaluate_nnet <- function(train_data, test_data, x_col, target_col, num_neurons, return_model) {
  # Entrenar el modelo
  set.seed(123)
  model <- nnet(
    as.formula(paste0(target_col, " ~ ", paste0(x_col, collapse = " + "))),
    data = train_data, 
    size = num_neurons, 
    linout = TRUE,
    trace = FALSE, 
    maxit = 300)
  
  if(return_model){
    return(model)
  } else{
    # Predecir en el conjunto de prueba
    predictions <- predict(model, test_data)
    
    # Calcular el error cuadrático medio
    rmse <- sqrt(mean((predictions - test_data[,target_col])^2))
    
    return(rmse)
  }
}

#=====================================================#
# Función boostraping redes neuronales de una capa ####
#=====================================================#
# Función para entrenar y predecir usando nnet con remuestreo (bootstrap)
bootstrap_nnet <- function(train_data, boost_data, x_col, target_col, num_neurons, n_bootstrap = 100, path.mod) {
  
  if(!is.null(train_data)){
    set.seed(123)
    
    n = nrow(train_data)
    predictions <- matrix(NA, nrow = nrow(boost_data), ncol = n_bootstrap)
    
    dir.create(paste0(path.mod, "/boost_models/"), showWarnings = F)
    
    for (i in 1:n_bootstrap) {
      sample_indices <- sample(1:n, n, replace = TRUE)
      train_x_sample <- train_data[sample_indices, ]
      
      model = nnet(
        as.formula(paste0(target_col, " ~ ", paste0(x_col, collapse = " + "))),
        data = train_x_sample, 
        size = num_neurons, 
        linout = TRUE,
        trace = FALSE, 
        maxit = 100)
      
      predictions[, i] <- predict(model, boost_data)
      
      saveRDS(model, paste0(path.mod, "/boost_models/model_", i, ".prds"))
    }
  } else{
    list.models = list.files(paste0(path.mod, "/boost_models/"), full.names = T)
    
    predictions <- matrix(NA, nrow = nrow(boost_data), ncol = length(list.models))
    
    cc = 1
    for (i in list.models) {
      model = readRDS(i)
      predictions[, cc] <- predict(model, boost_data)
      cc = cc + 1
    }
    
    }
  
  
  # Calcular el intervalo de confianza
  mean_pred = apply(predictions, 1, mean, na.rm = T)
  sd_pred = apply(predictions, 1, sd)
  
  lower_bound_95 <- mean_pred - 1.96*sd_pred
  upper_bound_95 <- mean_pred + 1.96*sd_pred
  
  lower_bound_99 <- mean_pred - 2.58*sd_pred
  upper_bound_99 <- mean_pred + 2.58*sd_pred
  
  results <- data.frame(
    Mean_Prediction = mean_pred,
    
    Lower_Bound_95 = lower_bound_95,
    Upper_Bound_95 = upper_bound_95,
    
    Lower_Bound_99 = lower_bound_99,
    Upper_Bound_99 = upper_bound_99
  )
  
  return(results)
}

#=====================================================================================#
## Función para hacer predicción manual en un conjunto de datos de un modelo LME ######
#=====================================================================================#
ManualPredictLME <- function(model, newdata, formula.str2, var.random2) {
  # browser()
  # Extraer los coeficientes fijos del modelo
  fixed_effects <- fixef(model)
  
  # Extraer la matriz de diseño (matriz de modelo) del nuevo conjunto de datos
  X <- model.matrix(as.formula(formula.str2), newdata)
  
  # Calcular las predicciones multiplicando la matriz de diseño por los coeficientes fijos
  predictions <- X %*% fixed_effects
  
  # Aplicar efecto aleatorio
  predictions <- predictions + model$coefficients$random[[1]][newdata[,var.random2],]
  
  # Convertir las predicciones a un vector
  predictions <- as.vector(predictions)
  
  return(predictions)
}



#=====================================================#
#          Función automatización modelos        ######
#=====================================================#
AutoRegression = function(
    df.train, 
    df.test,
    modelo.lm = TRUE,
    modelo.cuantilica = TRUE,
    modelo.rf = TRUE,
    modelo.xgboost = TRUE,
    modelo.lm.multinivel = TRUE,
    modelo.nn = TRUE,
    var.y,
    var.log, 
    var.sqrt,
    var.otras,
    var.e,
    var.multinivel,
    .folder,
    .suffix){
  # browser()
  options(scipen = 999)
  options(warn=-1) # Evitar advertencias

  #=======================================#
  #       1. Datos para los modelos    ####
  #=======================================#
  #=================================================#
  # Estandarización y organización de las variables #
  #=================================================#
  dir.create(paste0("out/",.folder,"/vt_",.suffix), recursive = T)
  datos = PrepVariables(data.train = df.train, data.test = df.test, var.e = var.e, var.sqrt = var.sqrt, var.otras = var.otras, var.log = var.log, .folder, .suffix)
  list2env(datos, .GlobalEnv)
  rm(datos);gc()
  
  #========================================================#
  #                2. Modelo Lineal                     ####
  #========================================================#
  if(modelo.lm) {
    cat("En proceso Regresión OLS......\n")
    
    path.gg = paste0("out/",.folder,"/vt_",.suffix,"/lm/")
    dir.create(path.gg, recursive = T, showWarnings = F)
    
    ## Modelo
    mod = lm(
      as.formula(paste0(var.y, " ~ ", paste0(var.modelo, collapse = " + "))),
      data = df.train.lm)
    
    ## Exportar modelo
    saveRDS(mod, paste0(path.gg, "/modelo_lm.rds") )
    
    ## Exportar parámetros estimados
    xx = summary(mod)
    writexl::write_xlsx(
      xx$coefficients %>% 
        as.data.frame() %>% 
        mutate_if(is.numeric, round, digits = 4) %>% 
        mutate(Parameters = CleanCharMatrix(t(xx$coefficients)) %>% colnames ) %>% 
        relocate(Parameters) %>% 
        `rownames<-`(NULL),
      paste0(path.gg, "/coef_modelo_lm.xlsx")
    )
    
    rm(xx)
    
    #===========#
    # Supuestos #
    #===========#
    ## Normalidad
    set.seed(451)
    png(paste0(path.gg, "qq_plot_residuales.png"), res = 400, units = "in", width = 5, height = 4)
    print(car::qqPlot(mod$residuals[sample(1:length(mod$residuals), min(2000, length(mod$residuals)))], id = FALSE) )
    dev.off()
    
    png(paste0(path.gg, "hist_residuales.png"), res = 400, units = "in", width = 5, height = 4)
    print(
      data.frame(res = mod$residuals) %>%
        ggplot(aes(x = res)) +
        geom_histogram(bins = 30) +
        labs(
          y = "Frecuencia",
          x = "Residuales"
        )
    )
    dev.off()
    
    ## Gráficos supuestos
    set.seed(451)
    rr = sample(1:nrow(df.train.lm), min(10000, nrow(df.train.lm)) )
    
    ## Observados contra residuales
    png(paste0(path.gg, "obs_res_train.png"), res = 400, units = "in", width = 5, height = 4)
    print(
      df.train.lm[rr,] %>% 
        ggplot(aes(x = get(var.y), y = mod$residuals[rr] )) + 
        geom_point() + 
        labs(
          y = "Residuales",
          x = "Observados"
        )
    )
    dev.off()
    
    ## Estimados contra residuales
    png(paste0(path.gg, "est_res_train.png"), res = 400, units = "in", width = 5, height = 4)
    print(
      data.frame(xx = rr) %>% 
        ggplot(aes(x = mod$fitted[rr], y = mod$residuals[rr] )) + 
        geom_point() + 
        labs(
          y = "Ajustados",
          x = "Residuales"
        )
    )
    dev.off()
    
    rm(rr)
    
    ## Predicciones prueba
    # Intervalos
    inter = predict(mod, df.test.lm, interval = "confidence")
    inter99 = predict(mod, df.test.lm, interval = "confidence", level = 0.99)
    
    # Realizar predicción
    df.mod.lm = df.test %>% 
      mutate(
        fitted = inter[,1],
        lower = inter[,2],
        upper = inter[,3],
        
        fitted99 = inter99[,1],
        lower99 = inter99[,2],
        upper99 = inter99[,3]
      ) 
    
    rm(inter, inter99)
    
    ## Predicciones entrenamiento
    # Intervalos
    inter = predict(mod, df.train.lm, interval = "confidence")
    inter99 = predict(mod, df.train.lm, interval = "confidence", level = 0.99)
    
    # Realizar predicción
    df.mod.lm.train = df.train.lm %>% 
      mutate(
        fitted = inter[,1],
        lower = inter[,2],
        upper = inter[,3],
        
        fitted99 = inter99[,1],
        lower99 = inter99[,2],
        upper99 = inter99[,3]
      ) 
    
    rm(inter, inter99)
    
    #=========================#
    # Observados vs Predichos #
    #=========================#
    set.seed(451)
    rr = sample(1:nrow(df.mod.lm), min(10000, nrow(df.mod.lm)))
    
    gg1 = df.mod.lm[rr,] %>% 
      ggplot(aes(x = get(var.y), y = fitted)) + 
      geom_errorbar(aes(ymin = lower, ymax = upper), color = "gray75") +
      geom_point() + 
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 1.2, colour = "gray75") + 
      labs(x = var.y, y = "Estimación" )
    
    png(paste0(path.gg, "fitted_observed_test.png"), width = 5, height = 4, res = 400, units = "in")
    print(gg1)
    dev.off()
    
    #==============#
    # Distribución #
    #==============#
    ## En logaritmo
    gg1 = df.mod.lm %>% 
      ggplot(aes(x = get(var.y), fill = "Observado")) + 
      geom_histogram(alpha = 0.5, bins = 30) + 
      geom_histogram(aes(x = fitted, fill = "Estimado"), alpha = 0.5, bins = 30) + 
      labs(
        x = var.y,
        fill = "",
        y = "Frecuencia") + 
      theme(legend.position = "bottom")
    
    png(paste0(path.gg, "distribucion_test.png"), height = 4, width = 5, res = 400, units = "in")
    print(gg1)
    dev.off()
    
    #==========#
    # Métricas #
    #==========#
    df.metricas = MetricsAuto(df = df.mod.lm, df.entrenamiento = df.mod.lm.train, y.name = var.y)
    
    df.metricas99 = MetricsAuto(
      df = df.mod.lm %>% 
        dplyr::select(-all_of(c("fitted","upper","lower"))) %>% 
        dplyr::rename(fitted = fitted99, upper = upper99, lower = lower99),
      df.entrenamiento = df.mod.lm.train %>% 
        dplyr::select(-all_of(c("fitted","upper","lower"))) %>% 
        dplyr::rename(fitted = fitted99, upper = upper99, lower = lower99),
      y.name = var.y) %>% 
      transmute(tipo, porc.int99 = porc.int)
    
    df.metricas = df.metricas %>% left_join(df.metricas99, by = "tipo")
    
    writexl::write_xlsx(df.metricas, paste0(path.gg, "metricas.xlsx") )
    saveRDS(df.mod.lm, paste0(path.gg, "df_test.rds") )
    
    cat("Exitoso Regresión OLS\n")
  
  }
  
  #==================================#
  #    3. Regresión Cuantílica    ####
  #==================================#
  if(modelo.cuantilica) {
    cat("En proceso Regresión Cuantílica......\n")
    path.gg = paste0("out/",.folder,"/vt_",.suffix,"/cuantilica/")
    dir.create(path.gg, recursive = T, showWarnings = F)
    
    ## Ajuste del modelo
    mod.qr = rq(
      as.formula(paste0(var.y," ~ ", paste0(var.modelo, collapse = " + "))),
      data = df.train.lm,
      tau = c(0.005, 0.025, 0.5, 0.975, 0.995) )
    
    ## Exportar modelo
    saveRDS(mod.qr, paste0(path.gg, "/modelo_qr.rds") )
    
    #===========#
    # Supuestos #
    #===========#
    ## Normalidad
    set.seed(451)
    png(paste0(path.gg, "qq_plot_residuales.png"), res = 400, units = "in", width = 5, height = 4)
    print(car::qqPlot(mod.qr$residuals[sample(1:nrow(mod.qr$residuals), min(2000, nrow(mod.qr$residuals))), 3], id = FALSE ))
    dev.off()
    
    png(paste0(path.gg, "hist_residuales.png"), res = 400, units = "in", width = 5, height = 4)
    print(
      data.frame(res = mod.qr$residuals[,3]) %>%
        ggplot(aes(x = res)) +
        geom_histogram(bins = 30) +
        labs(
          y = "Frecuencia",
          x = "Residuales"
        )
    )
    dev.off()
    
    ## Gráficos supuestos
    set.seed(451)
    rr = sample(1:nrow(df.train.lm), min(10000, nrow(df.train.lm)) )
    
    ## Observados contra residuales
    png(paste0(path.gg, "obs_res_train.png"), res = 400, units = "in", width = 5, height = 4)
    print(
      df.train.lm[rr,] %>% 
        ggplot(aes(x = get(var.y), y = mod.qr$residuals[rr,3] )) + 
        geom_point() + 
        labs(
          y = "Residuales",
          x = "Observados"
        )
    )
    dev.off()
    
    ## Estimados contra residuales
    png(paste0(path.gg, "est_res_train.png"), res = 400, units = "in", width = 5, height = 4)
    print(
      data.frame(xx = rr) %>% 
        ggplot(aes(x = mod.qr$fitted[rr], y = mod.qr$residuals[rr,3] )) + 
        geom_point() + 
        labs(
          y = "Ajustados",
          x = "Residuales"
        )
    )
    dev.off()
    
    rm(rr)
    
    ## Predicciones
    # Intervalos
    inter = predict(mod.qr, df.test.lm)
    
    # Realizar predicción
    df.mod.lm = df.test.lm %>% 
      mutate(
        fitted = inter[,3], 
        lower = inter[,2], 
        upper = inter[,4],
        
        lower99 = inter[,1], 
        upper99 = inter[,5]
      )
    
    ## Predicciones entrenamiento
    # Intervalos
    inter = predict(mod.qr, df.train.lm, interval = "confidence")
    
    # Realizar predicción
    df.mod.lm.train = df.train.lm %>% 
      mutate(
        fitted = inter[,3], 
        lower = inter[,2], 
        upper = inter[,4],
        
        lower99 = inter[,1], 
        upper99 = inter[,5]
      ) 
    
    rm(inter)
    
    #=========================#
    # Observados vs Predichos #
    #=========================#
    set.seed(451)
    rr = sample(1:nrow(df.mod.lm), min(10000, nrow(df.mod.lm)) )
    
    gg1 = df.mod.lm[rr,] %>% 
      ggplot(aes(x = get(var.y), y = fitted)) + 
      geom_errorbar(aes(ymin = lower, ymax = upper), color = "gray75") +
      geom_point() + 
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 1.2, colour = "gray75") + 
      labs(x = var.y, y = "Estimación" )
    
    png(paste0(path.gg, "fitted_observed_test.png"), width = 5, height = 4, res = 400, units = "in")
    print(gg1)
    dev.off()
    
    #==============#
    # Distribución #
    #==============#
    ## En logaritmo
    gg1 = df.mod.lm %>% 
      ggplot(aes(x = get(var.y), fill = "Observado")) + 
      geom_histogram(alpha = 0.5, bins = 30) + 
      geom_histogram(aes(x = fitted, fill = "Estimado"), alpha = 0.5, bins = 30) + 
      labs(
        x = var.y,
        fill = "",
        y = "Frecuencia") + 
      theme(legend.position = "bottom")
    
    png(paste0(path.gg, "distribucion_test.png"), height = 4, width = 5, res = 400, units = "in")
    print(gg1)
    dev.off()
    
    #==========#
    # Métricas #
    #==========#
    df.metricas = MetricsAuto(df = df.mod.lm, df.entrenamiento = df.mod.lm.train, y.name = var.y)
    
    df.metricas99 = MetricsAuto(
      df = df.mod.lm %>% 
        dplyr::select(-all_of(c("upper","lower"))) %>% 
        dplyr::rename(upper = upper99, lower = lower99),
      df.entrenamiento = df.mod.lm.train %>% 
        dplyr::select(-all_of(c("upper","lower"))) %>% 
        dplyr::rename(upper = upper99, lower = lower99),
      y.name = var.y) %>% 
      transmute(tipo, porc.int99 = porc.int)
    
    df.metricas = df.metricas %>% left_join(df.metricas99, by = "tipo")
    
    writexl::write_xlsx(df.metricas, paste0(path.gg, "metricas.xlsx") )
    saveRDS(df.mod.lm, paste0(path.gg, "df_test.rds") )
    
    cat("Exitoso Cuantílica\n")
  }
  
  #==============================================#
  #         4. XGboost - Regresión            ####
  #==============================================#
  if(modelo.xgboost) { 
    cat("En proceso XGBoost......\n")
    path.gg = paste0("out/",.folder,"/vt_",.suffix,"/xgboost/")
    dir.create(path.gg, recursive = T, showWarnings = F)
    
    ## Datos
    X <- model.matrix(
      as.formula(paste0(var.y, " ~ ", paste0(var.modelo, collapse = " + "))),
      data = rbind(df.train.lm, df.test.lm))
    
    X.train = X[1:nrow(df.train.lm),]
    X.test = X[(nrow(df.train.lm) + 1):nrow(X),]
    
    rm(X)
    
    data.table::setDTthreads(3)
    xgb_train = xgb.DMatrix(data = X.train, label = df.train.lm[,var.y], nthread = 3)
    xgb_test = xgb.DMatrix(data = X.test, label = df.test.lm[,var.y], nthread = 3 )
    
    watchlist = list(train=xgb_train, test=xgb_test)
    
    ## Modelos para intervalos de confianza
    dir.create(paste0(path.gg, "models_ci/"))
    
    set.seed(451)
    rnum = unique(round(runif(200, 100, 999999)))[1:100]
    
    cc = 1
    for(i in rnum){
      set.seed(i)
      model = xgb.train(
        data = xgb_train, 
        max.depth = 6, 
        nrounds = 50, 
        params = list(eta = 0.15, colsample_bytree = 0.75, subsample = 0.75, nthread = 3), verbose = 0)
      saveRDS(model, paste0(path.gg, "models_ci/modelo_xgb_", i, ".rds") ) 
      # cat("Realizado: ", cc ," de 100\n")
      cc = cc + 1
    }
    rm(i, model, cc)
    
    ## Obtener varianza de las predicciones test
    pred_test = NULL
    
    for(p_mm in list.files(paste0(path.gg, "models_ci/"), full.names = T)){
      mm = readRDS(p_mm)
      pred_test = cbind(pred_test, predict(mm, X.test))
      rm(mm)
      # cat("Realizado: ", p_mm, "\n")
      gc()
    }
    
    # Desviación predicciones
    sd_test = apply(pred_test, 1, sd)
    saveRDS(sd_test, paste0(path.gg, "/sd_test.rds"))

    ## Obtener varianza de las predicciones train
    pred_train = NULL
    
    for(p_mm in list.files(paste0(path.gg, "models_ci/"), full.names = T)){
      mm = readRDS(p_mm)
      pred_train = cbind(pred_train, predict(mm, X.train))
      rm(mm)
      # cat("Realizado: ", p_mm, "\n")
      gc()
    }
    
    # Desviación predicciones
    sd_train = apply(pred_train, 1, sd)
    
    saveRDS(sd_train, paste0(path.gg, "/sd_train.rds"))
    
    ## Modelo final
    model = xgb.train(
      data = xgb_train,
      max.depth = 6,
      watchlist = watchlist,
      nrounds = 30, 
      params = list(eta = 0.15, colsample_bytree = 0.9, subsample = 0.9, nthread = 3), 
      verbose = 0 )
    
    ## Exportar modelo
    saveRDS(model, paste0(path.gg, "/modelo_xgboost.rds") )

    ## Importancia
    df.import = xgb.importance(feature_names = colnames(X.train), model = model)
    
    png(paste0(path.gg, "/importance.png"), res = 400, units = "in", height = 4, width = 5)
    print(
      df.import %>% 
        arrange(desc(Gain)) %>% 
        head(15) %>% 
        mutate(Feature = factor(Feature, levels = rev(Feature) ) ) %>% 
        ggplot(aes(x = Gain, y = Feature)) + 
        geom_bar(stat = "identity") + 
        labs(
          y = "",
          x = "Ganancia",
          title = "XGBoost"
        )
    )
    dev.off()
    
    ## Exportar history
    png(paste0(path.gg, "/epochs_train.png"), res = 400, units = "in", height = 4, width = 5)
    print(
      model$evaluation_log %>% 
        ggplot(aes(x = iter, y = train_rmse, group = 1, colour = "Train")) + 
        geom_line() + 
        geom_line(aes(y = test_rmse, group = 1, colour = "Test"))
    )
    dev.off()
    
    ## Predicciones
    df.mod.lm = df.test.lm %>% 
      mutate(
        fitted = predict(model, X.test), 
        lower99 = fitted - 2.58*sd_test,  # Intervalo del 99%
        upper99 = fitted + 2.58*sd_test,
        
        lower = fitted - 1.96*sd_test,  # Intervalo del 99%
        upper = fitted + 1.96*sd_test
      )
    
    df.mod.lm.train = df.train.lm %>% 
      mutate(
        fitted = predict(model, X.train), 
        lower99 = fitted - 2.58*sd_train,  # Intervalo del 99%
        upper99 = fitted + 2.58*sd_train,
        
        lower = fitted - 1.96*sd_train,  # Intervalo del 99%
        upper = fitted + 1.96*sd_train
      )
    
    #=========================#
    # Observados vs Predichos #
    #=========================#
    set.seed(451)
    rr = sample(1:nrow(df.mod.lm), min(10000, nrow(df.mod.lm)) )
    
    gg1 = df.mod.lm[rr,] %>% 
      ggplot(aes(x = get(var.y), y = fitted)) + 
      geom_errorbar(aes(ymin = lower, ymax = upper), color = "gray75") +
      geom_point() + 
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 1.2, colour = "gray75") + 
      labs(x = var.y, y = "Estimación" )
    
    png(paste0(path.gg, "fitted_observed_test.png"), width = 5, height = 4, res = 400, units = "in")
    print(gg1)
    dev.off()
    
    #==============#
    # Distribución #
    #==============#
    ## En logaritmo
    gg1 = df.mod.lm %>% 
      ggplot(aes(x = get(var.y), fill = "Observado")) + 
      geom_histogram(alpha = 0.5, bins = 30) + 
      geom_histogram(aes(x = fitted, fill = "Estimado"), alpha = 0.5, bins = 30) + 
      labs(
        x = var.y,
        fill = "",
        y = "Frecuencia") + 
      theme(legend.position = "bottom")
    
    png(paste0(path.gg, "distribucion_test.png"), height = 4, width = 5, res = 400, units = "in")
    print(gg1)
    dev.off()
    
    #==========#
    # Métricas #
    #==========#
    df.metricas = MetricsAuto(df = df.mod.lm, df.entrenamiento = df.mod.lm.train, y.name = var.y)
    
    df.metricas99 = MetricsAuto(
      df = df.mod.lm %>% 
        dplyr::select(-all_of(c("upper","lower"))) %>% 
        dplyr::rename(upper = upper99, lower = lower99),
      df.entrenamiento = df.mod.lm.train %>% 
        dplyr::select(-all_of(c("upper","lower"))) %>% 
        dplyr::rename(upper = upper99, lower = lower99),
      y.name = var.y) %>% 
      transmute(tipo, porc.int99 = porc.int)
    
    df.metricas = df.metricas %>% left_join(df.metricas99, by = "tipo")
    
    writexl::write_xlsx(df.metricas, paste0(path.gg, "metricas.xlsx") )
    saveRDS(df.mod.lm, paste0(path.gg, "df_test.rds") )
    
    cat("Exitoso XGBoost\n")
  }
  
  #====================================================#
  #      5. Random Forest - Regresión              #####
  #====================================================#
  if(modelo.rf) {
    cat("En proceso Random Forest......\n")
    path.gg = paste0("out/",.folder,"/vt_",.suffix,"/random_forest/")
    dir.create(path.gg, recursive = T, showWarnings = F)
    
    ## Datos
    X <- model.matrix(
      as.formula(paste0(var.y, " ~ ", paste0(var.modelo, collapse = " + "))),
      data = rbind(df.train.lm, df.test.lm))
    
    X.train = X[1:nrow(df.train.lm),]
    X.test = X[(nrow(df.train.lm) + 1):nrow(X),]
    
    rm(X)
    
    ## Modelo
    model = randomForest(
      x = X.train,
      y = df.train.lm[,var.y],
      xtest = X.test,
      ytest = df.test.lm[,var.y],
      maxnodes = 1000, 
      keep.forest = T,
      ntree = 50, 
      do.trace = F)
    
    ## Exportar modelo
    saveRDS(model, paste0(path.gg, "/modelo_rf.rds") )

    ## Importancia
    df.import = model$importance %>% as.data.frame()
    df.import$Feature = rownames(df.import)
    rownames(df.import) = NULL
    
    png(paste0(path.gg, "/importance.png"), res = 400, units = "in", height = 4, width = 5)
    print(
      df.import %>% 
        arrange(desc(IncNodePurity)) %>% 
        head(15) %>% 
        mutate(Feature = factor(Feature, levels = rev(Feature) ) ) %>% 
        ggplot(aes(x = IncNodePurity, y = Feature)) + 
        geom_bar(stat = "identity") + 
        labs(
          y = "",
          x = "Incremento en pureza del nodo",
          title = "Random Forest"
        )
    )
    dev.off()
    
    ## Exportar history
    png(paste0(path.gg, "/ntrees_train.png"), res = 400, units = "in", height = 4, width = 5)
    print(
      data.frame(ntree = 1:length(model$mse), mse = model$mse, mse_test = model$test$mse) %>% 
        ggplot(aes(x = ntree, y = mse, group = 1, colour = "Train")) + 
        geom_line() + 
        geom_line(aes(y = mse_test, group = 1, colour = "Test"))
    )
    dev.off()
    
    ## Predicciones
    pred.rf = predict(model, X.test, predict.all=TRUE)
    
    pred.rf.int <- t(apply( pred.rf$individual, 1, function(x){ 
      c( mean(x) + c(-1.96,1.96)*sd(x)) }))
    
    df.mod.lm = df.test.lm %>% 
      mutate(
        fitted = pred.rf$aggregate, 
        lower = pred.rf.int[,1],  # Intervalo del 99%
        upper = pred.rf.int[,2])
    
    pred.rf.int <- t(apply( pred.rf$individual, 1, function(x){
      c( mean(x) + c(-2.58,2.58)*sd(x)) }))
    
    df.mod.lm = df.mod.lm %>% 
      mutate(
        lower99 = pred.rf.int[,1],  # Intervalo del 99%
        upper99 = pred.rf.int[,2])
    
    
    rm(pred.rf, pred.rf.int)
    
    # Entrenamiento
    pred.rf = predict(model, X.train, predict.all=TRUE)
    
    pred.rf.int <- t(apply( pred.rf$individual, 1, function(x){ 
      c( mean(x) + c(-1.96,1.96)*sd(x)) }))
    
    df.mod.lm.train = df.train.lm %>% 
      mutate(
        fitted = pred.rf$aggregate, 
        lower = pred.rf.int[,1],  # Intervalo del 99%
        upper = pred.rf.int[,2])
    
    pred.rf.int <- t(apply( pred.rf$individual, 1, function(x){
      c( mean(x) + c(-2.58,2.58)*sd(x)) }))
    
    df.mod.lm.train = df.mod.lm.train %>% 
      mutate(
        lower99 = pred.rf.int[,1],  # Intervalo del 99%
        upper99 = pred.rf.int[,2])
    
    rm(pred.rf, pred.rf.int)
    
    #=========================#
    # Observados vs Predichos #
    #=========================#
    set.seed(451)
    rr = sample(1:nrow(df.mod.lm), min(10000, nrow(df.mod.lm)) )
    
    gg1 = df.mod.lm[rr,] %>% 
      ggplot(aes(x = get(var.y), y = fitted)) + 
      geom_errorbar(aes(ymin = lower, ymax = upper), color = "gray75") +
      geom_point() + 
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 1.2, colour = "gray75") + 
      labs(x = var.y, y = "Estimación" )
    
    png(paste0(path.gg, "fitted_observed_test.png"), width = 5, height = 4, res = 400, units = "in")
    print(gg1)
    dev.off()
    
    #==============#
    # Distribución #
    #==============#
    ## En logaritmo
    gg1 = df.mod.lm %>% 
      ggplot(aes(x = get(var.y), fill = "Observado")) + 
      geom_histogram(alpha = 0.5, bins = 30) + 
      geom_histogram(aes(x = fitted, fill = "Estimado"), alpha = 0.5, bins = 30) + 
      labs(
        x = var.y,
        fill = "",
        y = "Frecuencia") + 
      theme(legend.position = "bottom")
    
    png(paste0(path.gg, "distribucion_test.png"), height = 4, width = 5, res = 400, units = "in")
    print(gg1)
    dev.off()
    
    #==========#
    # Métricas #
    #==========#
    df.metricas = MetricsAuto(df = df.mod.lm, df.entrenamiento = df.mod.lm.train, y.name = var.y)
    
    df.metricas99 = MetricsAuto(
      df = df.mod.lm %>% 
        dplyr::select(-all_of(c("upper","lower"))) %>% 
        dplyr::rename(upper = upper99, lower = lower99),
      df.entrenamiento = df.mod.lm.train %>% 
        dplyr::select(-all_of(c("upper","lower"))) %>% 
        dplyr::rename(upper = upper99, lower = lower99),
      y.name = var.y) %>% 
      transmute(tipo, porc.int99 = porc.int)
    
    df.metricas = df.metricas %>% left_join(df.metricas99, by = "tipo")
    
    writexl::write_xlsx(df.metricas, paste0(path.gg, "metricas.xlsx") )
    saveRDS(df.mod.lm, paste0(path.gg, "df_test.rds") )
    
    cat("Exitoso Random Forest\n")
  }
  
  #====================================================================#
  #                6. Modelo Lineal Multinivel                      ####
  #====================================================================#
  # browser()
  if(modelo.lm.multinivel) {
    cat("En proceso Regresión Multinivel......\n")
    path.gg = paste0("out/",.folder,"/vt_",.suffix,"/lm_multinivel/")
    dir.create(path.gg, recursive = T, showWarnings = F)
    
    ## Formula para el modelo
    ff = paste0(var.y, " ~ ", paste0(var.modelo[var.modelo != var.multinivel], collapse = " + "))
    
    ## Ajuste modelo con todas las variables
    Model.full = lme(
      as.formula(ff),
      random = as.formula(paste0("~ 1|",var.multinivel)), 
      data = df.train.lm, control = list(opt = "optim")) 
    
    ## Exportar modelo
    saveRDS(Model.full, paste0(path.gg, "/modelo_lm.rds") )
    
    #===========#
    # Supuestos #
    #===========#
    ## Normalidad
    set.seed(451)
    png(paste0(path.gg, "qq_plot_residuales.png"), res = 400, units = "in", width = 5, height = 4)
    print(car::qqPlot(Model.full$residuals[sample(1:nrow(Model.full$residuals), min(2000, nrow(Model.full$residuals))),1], id = FALSE))
    dev.off()
    
    png(paste0(path.gg, "hist_residuales.png"), res = 400, units = "in", width = 5, height = 4)
    print(
      data.frame(res = Model.full$residuals[,1]) %>%
        ggplot(aes(x = res)) +
        geom_histogram(bins = 30) +
        labs(
          y = "Frecuencia",
          x = "Residuales"
        )
    )
    dev.off()
    
    ## Gráficos supuestos
    set.seed(451)
    rr = sample(1:nrow(df.train.lm), min(10000, nrow(df.train.lm)) )
    
    ## Observados contra residuales
    png(paste0(path.gg, "obs_res_train.png"), res = 400, units = "in", width = 5, height = 4)
    print(
      df.train.lm[rr,] %>% 
        ggplot(aes(x = get(var.y), y = Model.full$residuals[rr,1] )) + 
        geom_point() + 
        labs(
          y = "Residuales",
          x = "Observados"
        )
    )
    dev.off()
    
    ## Estimados contra residuales
    png(paste0(path.gg, "est_res_train.png"), res = 400, units = "in", width = 5, height = 4)
    print(
      data.frame(xx = rr) %>% 
        ggplot(aes(x = Model.full$fitted[rr,2], y = Model.full$residuals[rr,2] )) + 
        geom_point() + 
        labs(
          y = "Ajustados",
          x = "Residuales"
        )
    )
    dev.off()
    
    rm(rr)
    
    ## Predicciones en test
    # Intervalos
    inter = PredLME(modelo = Model.full, df = rbind(df.train.lm, df.test.lm), subset = nrow(df.train.lm) + 1, formula.str = ff, var.random = var.multinivel)
    inter99 = PredLME(modelo = Model.full, df = rbind(df.train.lm, df.test.lm), subset = nrow(df.train.lm) + 1, zvalue = 2.58, formula.str = ff, var.random = var.multinivel)
    
    # Realizar predicción
    df.mod.lm = df.test.lm %>% 
      mutate(
        fitted = inter$pred, lower = inter$lower, upper = inter$upper,
        lower99 = inter99$lower, upper99 = inter99$upper) 
    
    rm(inter, inter99)
    
    ## Entrenamiento
    inter = PredLME(modelo = Model.full, df = rbind(df.test.lm, df.train.lm), subset = nrow(df.test.lm) + 1, formula.str = ff, var.random = var.multinivel)
    inter99 = PredLME(modelo = Model.full, df = rbind(df.test.lm, df.train.lm), subset = nrow(df.test.lm) + 1, zvalue = 2.58, formula.str = ff, var.random = var.multinivel)
    
    df.mod.lm.train = df.train.lm %>% 
      mutate(
        fitted = inter$pred, lower = inter$lower, upper = inter$upper,
        lower99 = inter99$lower, upper99 = inter99$upper) 
    
    rm(inter, inter99)
    
    #=========================#
    # Observados vs Predichos #
    #=========================#
    set.seed(451)
    rr = sample(1:nrow(df.mod.lm), min(10000, nrow(df.mod.lm)) )
    
    gg1 = df.mod.lm[rr,] %>% 
      ggplot(aes(x = get(var.y), y = fitted)) + 
      geom_errorbar(aes(ymin = lower, ymax = upper), color = "gray75") +
      geom_point() + 
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 1.2, colour = "gray75") + 
      labs(x = var.y, y = "Estimación" )
    
    png(paste0(path.gg, "fitted_observed_test.png"), width = 5, height = 4, res = 400, units = "in")
    print(gg1)
    dev.off()
    
    #==============#
    # Distribución #
    #==============#
    ## En logaritmo
    gg1 = df.mod.lm %>% 
      ggplot(aes(x = get(var.y), fill = "Observado")) + 
      geom_histogram(alpha = 0.5, bins = 30) + 
      geom_histogram(aes(x = fitted, fill = "Estimado"), alpha = 0.5, bins = 30) + 
      labs(
        x = var.y,
        fill = "",
        y = "Frecuencia") + 
      theme(legend.position = "bottom")
    
    png(paste0(path.gg, "distribucion_test.png"), height = 4, width = 5, res = 400, units = "in")
    print(gg1)
    dev.off()
    
    #==========#
    # Métricas #
    #==========#
    df.metricas = MetricsAuto(df = df.mod.lm, df.entrenamiento = df.mod.lm.train, y.name = var.y)
    
    df.metricas99 = MetricsAuto(
      df = df.mod.lm %>% 
        dplyr::select(-all_of(c("upper","lower"))) %>% 
        dplyr::rename(upper = upper99, lower = lower99),
      df.entrenamiento = df.mod.lm.train %>% 
        dplyr::select(-all_of(c("upper","lower"))) %>% 
        dplyr::rename(upper = upper99, lower = lower99),
      y.name = var.y) %>% 
      transmute(tipo, porc.int99 = porc.int)
    
    df.metricas = df.metricas %>% left_join(df.metricas99, by = "tipo")
    
    writexl::write_xlsx(df.metricas, paste0(path.gg, "metricas.xlsx") )
    saveRDS(df.mod.lm, paste0(path.gg, "df_test.rds"))
    
    cat("Exitoso Regresión Multinivel\n")
  }
  
  #==========================================================#
  #          7. Modelo de redes neuronales             #######
  #==========================================================#
  if(modelo.nn) {
    cat("En proceso Redes Neuronales......\n")
    
    #=====================================#
    # Encontrar número de neuronas óptimo #
    #=====================================#
    # Definir el rango de neuronas a probar
    num_neurons_list <- c(4, 8, 16)
    
    # Inicializar un dataframe para almacenar los resultados
    results <- data.frame(NumNeurons = integer(), RMSE = numeric())
    
    # Probar cada número de neuronas
    cc = 1
    for (num_neurons in num_neurons_list) {
      rmse <- train_evaluate_nnet(
        train_data = df.train.lm[,c(var.modelo, var.y)],
        test_data = df.test.lm[,c(var.modelo, var.y)], 
        x_col = var.modelo, 
        target_col = var.y, 
        num_neurons = num_neurons, 
        return_model = F)
      
      results <- rbind(results, data.frame(NumNeurons = num_neurons, RMSE = rmse))
      
      # cat("Realizado ", cc, " de ", length(num_neurons_list), "\n")
      cc = cc + 1
    }
    rm(cc, num_neurons)
    
    # Seleccionar el mejor número de neuronas
    best_num_neurons <- results$NumNeurons[which.min(results$RMSE)]
    
    parametros = readRDS(paste0("out/",.folder,"/vt_",.suffix,"/parametros_variables.rds"))
    
    parametros[["NN_BEST"]] = best_num_neurons
    
    saveRDS(parametros, paste0("out/",.folder,"/vt_",.suffix,"/parametros_variables.rds"))
    
    #======================================#
    # Ajustar con mejor número de neuronas #
    #======================================#
    path.gg = paste0("out/",.folder,"/vt_",.suffix,"/neural_net/")
    dir.create(path.gg, recursive = T, showWarnings = F)
    
    ## Ajuste modelo con todas las variables
    mod = train_evaluate_nnet(
      train_data = df.train.lm,
      test_data = df.test.lm, 
      x_col = var.modelo, 
      target_col = var.y, 
      num_neurons = best_num_neurons, 
      return_model = T)
    
    ## Exportar modelo
    saveRDS(mod, paste0(path.gg, "/modelo_nn.rds") )
    
    #====================#
    # Gráficos generales #
    #====================#
    set.seed(451)
    rr = sample(1:nrow(df.train.lm), min(10000, nrow(df.train.lm)) )
    
    ## Observados contra residuales
    png(paste0(path.gg, "obs_res_train.png"), res = 400, units = "in", width = 5, height = 4)
    print(
      df.train.lm[rr,] %>% 
        ggplot(aes(x = get(var.y), y = mod$residuals[rr,1] )) + 
        geom_point() + 
        labs(
          y = "Residuales",
          x = "Observados"
        )
    )
    dev.off()
    
    ## Estimados contra residuales
    png(paste0(path.gg, "est_res_train.png"), res = 400, units = "in", width = 5, height = 4)
    print(
      data.frame(xx = rr) %>% 
        ggplot(aes(x = mod$fitted.values[rr,1], y = mod$residuals[rr,1] )) + 
        geom_point() + 
        labs(
          y = "Ajustados",
          x = "Residuales"
        )
    )
    dev.off()
    
    rm(rr)
    
    ## Predicciones e intervalos de confianza
    inter = bootstrap_nnet(
      train_data = df.train.lm,
      boost_data = rbind(df.train.lm, df.test.lm),
      x_col = var.modelo,
      target_col = var.y,
      num_neurons = best_num_neurons,
      n_bootstrap = 100 ,
      path.mod = path.gg)
    
    # Obtener index entrenamiento y prueba
    index.train = 1:nrow(df.train.lm)
    index.test = (nrow(df.train.lm) + 1):nrow(inter)
    
    # Realizar predicción
    df.mod.lm = df.test.lm %>% 
      mutate(
        fitted = predict(mod, df.test.lm), 
        lower = inter$Lower_Bound_95[index.test], 
        upper = inter$Upper_Bound_95[index.test],
        lower99 = inter$Lower_Bound_99[index.test],
        upper99 = inter$Upper_Bound_99[index.test]
      ) 
    
    df.mod.lm.train = df.train.lm %>% 
      mutate(
        fitted = predict(mod, df.train.lm), 
        lower = inter$Lower_Bound_95[index.train], 
        upper = inter$Upper_Bound_95[index.train],
        lower99 = inter$Lower_Bound_99[index.train],
        upper99 = inter$Upper_Bound_99[index.train]
      ) 
    
    rm(inter)
    
    #=========================#
    # Observados vs Predichos #
    #=========================#
    set.seed(451)
    rr = sample(1:nrow(df.mod.lm), min(10000, nrow(df.mod.lm)) )
    
    gg1 = df.mod.lm[rr,] %>% 
      ggplot(aes(x = get(var.y), y = fitted)) + 
      geom_errorbar(aes(ymin = lower, ymax = upper), color = "gray75") +
      geom_point() + 
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 1.2, colour = "gray75") + 
      labs(x = var.y, y = "Estimación" )
    
    png(paste0(path.gg, "fitted_observed_test.png"), width = 5, height = 4, res = 400, units = "in")
    print(gg1)
    dev.off()
    
    #==============#
    # Distribución #
    #==============#
    ## En logaritmo
    gg1 = df.mod.lm %>% 
      ggplot(aes(x = get(var.y), fill = "Observado")) + 
      geom_histogram(alpha = 0.5, bins = 30) + 
      geom_histogram(aes(x = fitted, fill = "Estimado"), alpha = 0.5, bins = 30) + 
      labs(
        x = var.y,
        fill = "",
        y = "Frecuencia") + 
      theme(legend.position = "bottom")
    
    png(paste0(path.gg, "distribucion_test.png"), height = 4, width = 5, res = 400, units = "in")
    print(gg1)
    dev.off()
    
    #==========#
    # Métricas #
    #==========#
    df.metricas = MetricsAuto(df = df.mod.lm, df.entrenamiento = df.mod.lm.train, y.name = var.y)
    
    df.metricas99 = MetricsAuto(
      df = df.mod.lm %>% 
        dplyr::select(-all_of(c("upper","lower"))) %>% 
        dplyr::rename(upper = upper99, lower = lower99),
      df.entrenamiento = df.mod.lm.train %>% 
        dplyr::select(-all_of(c("upper","lower"))) %>% 
        dplyr::rename(upper = upper99, lower = lower99),
      y.name = var.y) %>% 
      transmute(tipo, porc.int99 = porc.int)
    
    df.metricas = df.metricas %>% left_join(df.metricas99, by = "tipo")
    
    writexl::write_xlsx(df.metricas, paste0(path.gg, "metricas.xlsx") )
    saveRDS(df.mod.lm, paste0(path.gg, "df_test.rds"))
    
    cat("Exitoso Redes Neuronales\n")
  }
  
  #======================================================#
  #       8. Concatenar todos los resultados          #### 
  #======================================================#
  #==========#
  # Metricas #
  #==========#
  ll = list.files(paste0("out/",.folder,"/vt_",.suffix,"/"), pattern = "metricas.xlsx", recursive = TRUE)
  
  df.metricas = NULL
  for(l in ll){
    df.metricas = rbind(
      df.metricas,
      readxl::read_excel(paste0("out/",.folder,"/vt_",.suffix,"/",l)) %>%
        mutate(modelo = str_split(l, "/", simplify = T)[,1]) %>%
        relocate(modelo) %>% 
        as.data.frame()
    )
    
  }
  
  writexl::write_xlsx(df.metricas, paste0("out/",.folder,"/vt_",.suffix,"/metricas_full.xlsx"))
  
  #=====================#
  # Gráfica comparativa #
  #=====================#
  ll = list.files(paste0("out/",.folder,"/vt_",.suffix,"/"), pattern = "df_test.rds", recursive = TRUE)
  
  df.test = NULL
  
  for(l in ll){
    df.temp = readRDS(paste0("out/",.folder,"/vt_",.suffix,"/",l))
    
    df.temp = df.temp %>% 
      dplyr::select(id, fitted, upper, lower, all_of(c(var.y)) ) %>% 
      mutate(MODELO = str_split(l, "/", simplify = T)[1,1])
    
    df.test = rbind(df.test, df.temp)
    rm(df.temp)
    # cat("Realizado: ", l, "\n")
  }
  
  ## IDS que aparecen en todos los modelos
  id.full = df.test %>% 
    group_by(id) %>% 
    summarise(nn = n()) %>% 
    as.data.frame() %>% 
    filter(nn == max(nn)) %>% 
    dplyr::select(id) %>% 
    pull
  
  ## Muestrear
  set.seed(451)
  id.sample = sample(id.full, min(10000, nrow(df.test)) )
  
  ## Graficar
  gg3 = df.test %>% 
    filter(id %in% id.sample) %>% 
    mutate(MODELO = toupper(MODELO) ) %>% 
    ggplot(aes(x = get(var.y), y = fitted )) + 
    geom_errorbar(aes(ymin = lower, ymax = upper), color = "gray75") +
    geom_point() + 
    facet_wrap(~MODELO, ncol = min(3, length(unique(df.test$MODELO)) ) ) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", linewidth = 1.5, colour = "gray50") + 
    labs(x = "Observado")+
    labs(y = "Estimación" ) + 
    theme(axis.text.x = element_text(angle = 90))
  
  
  png(paste0("out/",.folder,"/vt_",.suffix,"/fitted_observed_full.png"), width = 14, height = 8, res = 400, units = "in")
  print(gg3)
  dev.off()
  
  rm(gg3)
  
  cat("Exportación de métricas y consolidado exitoso\n")
  options(warn = 0) # Dejar advertencias
  
}

#================================================================#
#          Función predicción automatización modelos        ######
#================================================================#
AutoRegressionPredict = function(
    df.test,
    modelo.lm = TRUE,
    modelo.cuantilica = TRUE,
    modelo.rf = TRUE,
    modelo.xgboost = TRUE,
    modelo.lm.multinivel = TRUE,
    modelo.nn = TRUE,
    var.y,
    var.log, 
    var.sqrt,
    var.otras,
    var.e,
    var.multinivel,
    .folder,
    .suffix){
  # browser()
  options(scipen = 999)
  options(warn=-1) # Evitar advertencias
  
  #=======================================#
  #       1. Datos para los modelos    ####
  #=======================================#
  #=================================================#
  # Estandarización y organización de las variables #
  #=================================================#
  datos = PrepVariables(data.train = NULL, data.test = df.test, var.e = var.e, var.sqrt = var.sqrt, var.otras = var.otras, var.log = var.log, .folder, .suffix)
  list2env(datos, .GlobalEnv)
  rm(datos);gc()
  
  #========================================================#
  #                2. Modelo Lineal                     ####
  #========================================================#
  if(modelo.lm) {
    cat("En proceso Regresión OLS......\n")
    
    path.gg = paste0("out/",.folder,"/vt_",.suffix,"/lm/")
    
    ## Exportar modelo
    mod = readRDS(paste0(path.gg, "/modelo_lm.rds") )
    
    ## Predicciones prueba
    # Intervalos
    inter = predict(mod, df.test.lm, interval = "confidence")
    inter99 = predict(mod, df.test.lm, interval = "confidence", level = 0.99)
    
    # Realizar predicción
    df.test.lm = df.test.lm %>% 
      mutate(
        fitted_lm = inter[,1],
        lower95_lm = inter[,2],
        upper95_lm = inter[,3],
        lower99_lm = inter99[,2],
        upper99_lm = inter99[,3]
      ) 
    
    rm(inter, inter99)
    
    cat("Exitoso Regresión OLS\n")
    
  }
  
  #==================================#
  #    3. Regresión Cuantílica    ####
  #==================================#
  if(modelo.cuantilica) {
    cat("En proceso Regresión Cuantílica......\n")
    path.gg = paste0("out/",.folder,"/vt_",.suffix,"/cuantilica/")
    
    ## Exportar modelo
    mod.qr = readRDS(paste0(path.gg, "/modelo_qr.rds") )
    
    ## Predicciones
    # Intervalos
    inter = predict(mod.qr, df.test.lm)
    
    # Realizar predicción
    df.test.lm = df.test.lm %>% 
      mutate(
        fitted_qr = inter[,3], 
        lower95_qr = inter[,2], 
        upper95_qr = inter[,4],
        
        lower99_qr = inter[,1], 
        upper99_qr = inter[,5]
      )
    
    cat("Exitoso Cuantílica\n")
  }
  
  #==============================================#
  #         4. XGboost - Regresión            ####
  #==============================================#
  if(modelo.xgboost) { 
    cat("En proceso XGBoost......\n")
    path.gg = paste0("out/",.folder,"/vt_",.suffix,"/xgboost/")
    
    ## Datos
    X.test <- model.matrix(
      as.formula(paste0(var.y, " ~ ", paste0(var.modelo, collapse = " + "))),
      data = rbind(df.test.lm))
    
    ## Obtener varianza de las predicciones test
    pred_test = NULL
    
    for(p_mm in list.files(paste0(path.gg, "models_ci/"), full.names = T)){
      mm = readRDS(p_mm)
      pred_test = cbind(pred_test, predict(mm, X.test))
      rm(mm)
      gc()
    }
    
    # Desviación predicciones
    sd_test = apply(pred_test, 1, sd)
    
    ## Modelo final
    model = readRDS(paste0(path.gg, "/modelo_xgboost.rds") )
    
    ## Predicciones
    df.test.lm = df.test.lm %>% 
      mutate(
        fitted_xgb = predict(model, X.test), 
        lower95_xgb = fitted_xgb - 1.96*sd_test,  # Intervalo del 99%
        upper95_xgb = fitted_xgb + 1.96*sd_test,
        
        lower99_xgb = fitted_xgb - 2.58*sd_test,  # Intervalo del 99%
        upper99_xgb = fitted_xgb + 2.58*sd_test
      )
    
    cat("Exitoso XGBoost\n")
  }
  
  #====================================================#
  #      5. Random Forest - Regresión              #####
  #====================================================#
  if(modelo.rf) {
    cat("En proceso Random Forest......\n")
    path.gg = paste0("out/",.folder,"/vt_",.suffix,"/random_forest/")
    
    ## Datos
    X.test <- model.matrix(
      as.formula(paste0(var.y, " ~ ", paste0(var.modelo, collapse = " + "))),
      data = rbind(df.test.lm))
    
    ## Modelo
    model = readRDS(paste0(path.gg, "/modelo_rf.rds") )
    
    ## Predicciones
    pred.rf = predict(model, X.test, predict.all=TRUE)
    
    pred.rf.int <- t(apply( pred.rf$individual, 1, function(x){ 
      c( mean(x) + c(-1.96,1.96)*sd(x)) }))
    
    df.test.lm = df.test.lm %>% 
      mutate(
        fitted_rf = pred.rf$aggregate, 
        lower95_rf = pred.rf.int[,1],  
        upper95_rf = pred.rf.int[,2])
    
    pred.rf.int <- t(apply( pred.rf$individual, 1, function(x){
      c( mean(x) + c(-2.58,2.58)*sd(x)) }))
    
    df.test.lm = df.test.lm %>% 
      mutate(
        lower99_rf = pred.rf.int[,1],  # Intervalo del 99%
        upper99_rf = pred.rf.int[,2])
    
    
    rm(pred.rf, pred.rf.int)
    
    cat("Exitoso Random Forest\n")
  }
  
  #====================================================================#
  #                6. Modelo Lineal Multinivel                      ####
  #====================================================================#
  # browser()
  if(modelo.lm.multinivel) {
    cat("En proceso Regresión Multinivel......\n")
    path.gg = paste0("out/",.folder,"/vt_",.suffix,"/lm_multinivel/")
    
    ## Formula para el modelo
    ff = paste0(var.y, " ~ ", paste0(var.modelo[var.modelo != var.multinivel], collapse = " + "))
    
    ## Modelo
    Model.full = readRDS(paste0(path.gg, "/modelo_lm.rds") )
    
    ## Predicciones en test
    # Intervalos
    inter = PredLME(modelo = Model.full, df = df.test.lm, subset = NULL, formula.str = ff, var.random = var.multinivel)
    inter99 = PredLME(modelo = Model.full, df = df.test.lm, subset = NULL, zvalue = 2.58, formula.str = ff, var.random = var.multinivel)
    
    # Realizar predicción
    df.test.lm = df.test.lm %>% 
      mutate(
        fitted_lme = inter$pred, 
        lower95_lme = inter$lower,
        upper95_lme = inter$upper,
        lower99_lme = inter99$lower,
        upper99_lme = inter99$upper) 
    
    rm(inter, inter99)
    
    cat("Exitoso Regresión Multinivel\n")
  }
  
  #==========================================================#
  #          7. Modelo de redes neuronales             #######
  #==========================================================#
  if(modelo.nn) {
    cat("En proceso Redes Neuronales......\n")
    
    ## Seleccionar el mejor número de neuronas
    parametros = readRDS(paste0("out/",.folder,"/vt_",.suffix,"/parametros_variables.rds"))
    best_num_neurons = parametros[["NN_BEST"]]
    
    #======================================#
    # Ajustar con mejor número de neuronas #
    #======================================#
    path.gg = paste0("out/",.folder,"/vt_",.suffix,"/neural_net/")
    
    ## Exportar modelo
    mod = readRDS(paste0(path.gg, "/modelo_nn.rds") )
    
    ## Predicciones e intervalos de confianza
    inter = bootstrap_nnet(
      train_data = NULL,
      boost_data = df.test.lm,
      x_col = var.modelo,
      target_col = var.y,
      num_neurons = best_num_neurons,
      n_bootstrap = 100, 
      path.mod = path.gg)
    
    # Realizar predicción
    df.test.lm = df.test.lm %>% 
      mutate(
        fitted_nn = inter$Mean_Prediction, 
        lower95_nn = inter$Lower_Bound_95, 
        upper95_nn = inter$Upper_Bound_95,
        lower99_nn = inter$Lower_Bound_99,
        upper99_nn = inter$Upper_Bound_99
      ) 
    
    cat("Exitoso Redes Neuronales\n")
  }
  
  return(df.test.lm)
  
}
