## mafs main functions: apply_selected_model(), apply_all_models(), multi_forecast(), ggplot_fit()

#' @import forecast
#' @import ggplot2

#' @title Select a model to forecast a time series object.
#' @description
#' Apply a chosen forecast model to a time series object. Basically a wrapper for many functions from the forecast package.
#' Please run avaiableModels() to see the list of available modes to use as the model.name argument of this function.
#' @param x A ts object.
#' @param model_name A string indicating the name of the forecast model.
#' @return A forecast object
#' @examples
#' apply_selected_model(AirPassengers, "auto.arima")
#' @export
apply_selected_model <- function(x, model_name) {

  available_models <- available_models()

  if (!(model_name %in% available_models)) stop("Your model is not available. Please run avaiableModels() to see the list of available models")
  # former aplicarMelhorModelo()
  switch(model_name,
         "auto.arima" = auto.arima(x, ic='aicc', stepwise=FALSE), # 1
         "ets" = ets(x, ic='aicc', restrict=FALSE, opt.crit = "mae"), #2
         "nnetar" = nnetar(x, p=12, size=12, repeats = 24), # 3
         "tbats"  = tbats(x, ic='aicc', seasonal.periods=12), # 4
         "bats" = bats(x, ic='aicc', seasonal.periods=12), # 5
         "stlm_ets"  = stlm(x, s.window=12, ic='aicc', robust=TRUE, method='ets'), # 6
         "stlm_arima"  = stlm(x, s.window=12, ic='aicc', robust=TRUE, method='arima'), # 7
         "StructTS"  = StructTS(x), #8
         "meanf"  = meanf(x), #9
         "naive"  = naive(x), #10
         "snaive"  = snaive(x), #11
         "rwf"  = rwf(x), #12
         "rwf_drift"  = rwf(x, drift = TRUE),  #13 ### NOVO
         "splinef"  = splinef(x), #14
         "thetaf"  = thetaf(x), #15
         "croston"  = croston(x), #16
         "tslm"  = tslm(x ~ trend + season), #17
         "hybrid" = forecastHybrid::hybridModel(x) #18
  )
}

#' @title list of available models in mafs package
#' @return A character vector of the forecast models that can be used in this package.
#' @examples
#' available_models()
#' @export
available_models <- function() {
  return(c("auto.arima", "ets", "nnetar", "tbats", "bats","stlm_ets",
           "stlm_arima", "StructTS", "meanf", "naive", "snaive", "rwf",
           "rwf_drift", "splinef", "thetaf", "croston", "tslm", "hybrid"))
}


#' @title list of available error metrics in mafs package
#' @description See forecast::accuracy() for more details.
#' @details
#' There is an internal function in this package (removeTheil()) that
#' removes Theil'U metric from the output. This was necessary because for some
#' time series, forecast::accuracy() does not output the value for this
#' metric.
#' @return A character vector of the error metrics that can be used in this package.
#' @examples
#' error_metrics()
#' @export
error_metrics <- function(){
  return(c("ME", "RMSE", "MAE", "MAPE", "MASE"))
}

#' @title Fit several forecast models
#' @description
#' Create a list of all possible forecast models for the inputed time series object.
#' @details
#' This functions loops the output from available_models(), uses it as the
#' model.name argument for apply_selected_model() and return a list of length
#' 18 in which each element is a forecast model.
#' Depending on some of the characteristics of the time series object used as
#' the input for this function, the model might not be created. For example,
#' if you try to fit a neural network model to a short time series, it will
#' return an error and fail to create the fit. In order to overcome this issue,
#' if the model returns an error, it will return a NA as the list element
#' instead.
#'
#' @param x A ts object.
#' @return A list of forecast objects from apply_selected_model()
#' @examples
#' apply_all_models(austres)
#' @export
apply_all_models <- function(x) {
  # former aplicarTodosModelos

  mods <- available_models()
  models <- list() # initiates empty list to be filled by forecast models


  for (i in 1:length(mods)) {
    mod <- mods[i]
    fit <- try(apply_selected_model(x, mod), silent = TRUE)
    if (class(fit) != "try-error") models[[i]] <- fit
  }
  return(models)
}

#' @title Selects best forecast model
#' @description
#' multiForecast is the main function of this package. It uses
#'   apply_all_models() and other internal functions of this package to generate
#'   generate multiple forecasts for the same time series object.
#' @details
#' TODO
#' @param x A ts object.
#' @param test_size The desired length of the test set object to be used
#'   to measure the accuracy of the forecast models.
#' @param horizon The forecast horizon length
#' @param error The accuracy metric to be used to select the best forecast
#'   model from apply_all_models(). See error_metrics() for the available metrics.
#' @return A list of three objects:
#' @section df_models:
#'  A data.frame with the accuracy metrics of all models applied to x
#' @section best_forecast:
#'  A forecast object created by applying the best forecast method to x
#' @section df_comparison:
#'  A dataframe showing both the forecasted and the observed test set
#'
#' @examples
#' select_forecast(austres, 6, 12, "MAPE")
#' @export
select_forecast <- function(x, test_size, horizon = 3, error) {
  # Checks if defined error metric is available
  error_metrics <- error_metrics()
  if (!(error %in% error_metrics)) stop("Your error metric is not available. Please run error_metrics() to see the list of available metrics.")

  x_split <- CombMSC::splitTrainTest(x, length(x) - test_size)
  training <- x_split$train
  test <- x_split$test
  models_list <- apply_all_models(training)

  available_models <- available_models()
  num <- length(available_models)

  # Apply forecast() function to created models
  # some error handling:
  # for each element in model_list, if the model was not created (model_list[i] == NULL),
  # it replaces the element with a too big numeric vector of the same
  # characteristics as the test set.
  # This procedure, of course, needs to be improved, but it does the work for now.
  forecasts <- lapply(models_list, function(i) tryCatch({forecast(i, h = test_size)},
                                                       error = function(e) {
                                                         x <- rep(1e9, test_size)
                                                         x <- ts(x, start = start(test) - test_size/12,
                                                                 frequency = frequency(test))
                                                         x <- naive(x)
                                                         x <- forecast(x, h = test_size)
                                                         return(x)}))

  # measures the accuracy of all forecast models against the test set
  acc <- lapply(forecasts, function(f) accuracy(f, test)[2,,drop=FALSE])
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
  best_forecast <- forecast(apply_selected_model(x, best_model_name), h = horizon)

  best_training_forecast <- apply_selected_model(training, best_model_name)
  best_training_forecast %<>% forecast(h = test_size)


  ###  ===============
  # creates data.frame to output the forecast from the best model
  df_comparison <- data.frame(
    time = as.Date.cal.yr(time(test)),
    forecasted = as.numeric(best_training_forecast$mean)[1:test_size],
    observed = as.numeric(test)
  )

  return(
    list(
      df_models = acc,
      best_forecast = best_forecast,
      df_comparison = df_comparison)
    )


}



#' @title Plot a time series and its fitted and forecasted values
#' @description
#' Uses ggplot to plot a given time series as well as its fitted and
#' forecasted values obtained with one of the forecast functions available
#' in mafs package.
#' @details
#' TODO
#' @param x A ts object.
#' @param test_size The desired length of the test set object to be used
#'   to measure the accuracy of the forecast models.
#' @param horizon The forecast horizon length
#' @param Name of the forecast model. Please run available_models() to see the full list.
#' @return A ggplot object
#' @examples
#' ggplotarFit(AirPassengers, 6, 6, "ets")
#' @export
ggplotFit <- function(x, test_size, horizon, model_name) {

  available_models <- available_models()
  if (!(model_name %in% available_models)) stop("Your model is not available. Please run avaiableModels() to see the list of available models")

  # splits x into training and test sets
  x_split <- CombMSC::splitTrainTest(x, length(x) - test_size)
  training <- x_split$train
  test <- x_split$test

  fcast <- apply_selected_model(training, model_name)
  fcast <- forecast(fcast, test_size)

  fit <- fitted(fcast)
  forecasted <- fcast$mean

  z <- ggseas::tsdf(cbind(x, fit, forecasted))
  names(z) <- c("time", "observed.series", "fit", "forecasts")

  z <- tidyr::gather(z, series, value, 2:4)
  z <- dplyr::arrange(z, time)
  #z.pontos <- tail(z, 3 * hprev)

  p <- ggplot(z, aes(time, value, color = series, linetype = series)) +
         geom_line() +
         scale_color_manual(values = c("red", "red4", "gray")) +
         scale_linetype_manual(values = c(2, 2, 1)) +
         theme_bw() +
         theme(legend.position = "bottom", legend.title = element_blank())
return(p)

}
