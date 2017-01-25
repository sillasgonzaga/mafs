---
title: "MAFS - Multiple Automatic Forecast Selection"
output: github_document
---

Current version: 0.0.3

## Description

`mafs` is basically a wrapper for the [forecast](https://github.com/robjhyndman/forecast) package. Its main function is `select_forecast`, which takes a time series object as an input, splits it into training and test sets, fits up to (currently) 18 forecast models into the training set, measure their accuracy against the test set, chooses the best model according to the error metric defined by the user and outputs the results of the models and the forecasted future values.   

`mafs` is still at a very early phase with lots of room for improvement. I kindly invite all forecast analysts and practitioners to test my package and contribute to it, either by reporting issues or by pull requests. 

## Installation

For the time being, the development version is available only at Github.

``` r
# install.packages("devtools")
devtools::install_github("sillasgonzaga/mafs")
```

## Example

You can run a simple test the package with the code below

``` r
library(mafs)
select_forecast(AirPassengers, test_size = 6, horizon = 12, error = "MAPE")
```
## Shiny App

[Shiny app](https://sillasgonzaga.shinyapps.io/shiny-mafs/) demonstration of the models included in `mafs`.
