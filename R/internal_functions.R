## mafs internal functions

# remover theil da matrix gerada pelo accuracy()
removeTheil <- function(mat) {
  rows <- rownames(mat)
  cols <- colnames(mat)[1:7]

  m <- matrix(mat[,1:7], ncol = 7)
  rownames(m) <- rows
  colnames(m) <- cols
  return(m)
}

separateTS <- function(dataframe, numvar, datevar, n){
  # Internal Function
  # separara df de um sku em training e test sets

  # TODO: testar implementaçao da funcao splitTrainTest

  newdf <- dataframe
  #newdf <- subset(dataframe, cod_sku == sku) # O SUBSET VAI SER FEITO ANTES DE RODAR O multiForecast()
  # criar subset com df excluindo os primeiros n volumes iguais a 0

  # datas de inicio da serie
  start_date <- newdf[[datevar]] %>% min
  st_year <- year(start_date)
  st_month <- month(start_date)

  # criar ano e mes do training e test sets
  training <- head(newdf, nrow(newdf) - n)
  test <- tail(newdf, n)

  start_date_test <- test[[datevar]] %>% min
  st_year_test <- year(start_date_test)
  st_month_test <- month(start_date_test)


  start_training_date <- c(st_year, st_month)
  start_test_date <- c(st_year_test, st_month_test)
  # transformar training e test em objetos TS
  return(list(
    training = ts(training[[numvar]], start = start_training_date, frequency = 12),
    test = ts(test[[numvar]], start = start_test_date, frequency = 12)
  ))

}

# aplica forecast aos modelos criados em aplicarModelos()
applyForecast <- function(model_list, ts, training_set, test_set, n, error) {
  # model_list: list of forecast models created by apply_all_models()
  # ts: the original time series object (former tscompleto)
  # training_set: training set used to fit the forecast models
  # test_set: test set used to measure the accuracy of the forecast models
  # n: forecast horizon (former periodo)
  # error: error metric to be used to select the best model.
#
#   nomesModelos <- c("auto_arima", "ETS", "NN", "tbats", "bats", "stl_ets",
#                     "stl_arima", "sts", "meanf", "naive", "snaive", "random_walk",
#                     "random_walk_com_drift", "spline", "thetaf", "croston", "lm", "hybrid")
  available_models <- available_models()
  num <- length(available_models)

  # the forecast horizon depends on the test set length
  test.size = length(test_set)

  # it does some error handling.
  # for each element in model_list, if the model was not created (model_list[i] == NULL),
  # it replaces the element with a too big numeric vector of the same
  # characteristics as the test set.
  # This procedure, of course, needs to be improved, but it does the work for now.
  forecasts <- lapply(model_list, function(i) tryCatch({forecast(i, h = test.size)},
                                                       error = function(e) {
                                                         x <- rep(1e9, test.size)
                                                         x <- ts(x, start = start(test_set) - test.size/12,
                                                                 frequency = frequency(test_set))
                                                         x <- naive(x)
                                                         x <- forecast(x, h = test.size)
                                                         return(x)}))

  # measures the accuracy of all forecast models against the test set
  acc <- lapply(forecasts, function(f) accuracy(f, test_set)[2,,drop=FALSE])
  # remove Theil's U (in case it exists) from matrix
  acc <- lapply(acc, removeTheil)
  acc <- Reduce(rbind, acc)
  row.names(acc) <- NULL
  acc %<>% as.data.frame

  # Adds a column to acc to indicate the model name of the forecast row.
  # Depending the characteristics of the time series object, the hybridModel()
  # outputs nothing, which makes acc object have 17 instead of 18 rows.
  # Therefore, the line below is necessary to handle this situation
  acc$model <- if (nrow(acc) == 18) available_models else available_models[-18]

  # Selects row of minimum error. In case the error defined is MAPE and the
  # time series is intermitent, the MAPE might be Inf. To handle this, if MAPE
  # is Inf in all columns, it uses MAE as the error metric to select the best
  # forecast model.
  ind_best_model <- if (mean(acc[[error]]) != Inf) which.min(acc[[error]]) else which.min(acc[["MAE"]])

  best_model_name <- available_models[ind_best_model]
  acc$best_model <- best_model_name

  # Applys apply_selected_model using the best forecast model from the previous lines
  best_forecast <- forecast(apply_selected_model(training_set, best_model_name), h = n)

  best_training_forecast <- aplicarMelhorModelo(training_set, best_model_name)
  best_training_forecast %<>% forecast(h = test_size)


  ###  ===============
  # creates data.frame to output the forecast from the best model


  #time <- as.Date.cal.yr(time(test))
  #forecasted <- as.numeric(melhor_previsao_training$mean)[1:test.size]
  #observed <- as.numeric(test_set)

  df_comparison <- data.frame(
    time = as.Date.cal.yr(time(test)),
    forecasted = as.numeric(melhor_previsao_training$mean)[1:test.size],
    observed = as.numeric(test_set)
    )

  # ### ===============


  ###  =============== data frame de previsoes
  replaceNull <- function(x) ifelse(!is.null(x), x, NA)
#
#   melhor_previsao <- aplicarMelhorModelo(tscompleto, nome_melhor_modelo_str)
#   melhor_previsao %<>% forecast(h = periodo)
#
#   previsto_futuro <- melhor_previsao$mean %>% as.numeric()
#   low80 <- melhor_previsao$lower[,1] %>% replaceNull
#   hi80 <- melhor_previsao$upper[, 1] %>% replaceNull
#   data_fim_teste <- test_set %>% time %>% as.numeric %>% max %>% as.Date.cal.yr
#   meses_futuro <- seq.Date(from = data_fim_teste, by = "month", length.out = periodo)



  return(list(df_models = acc,
              best_forecast = best_forecast))
}
