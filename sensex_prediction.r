# Individual Assignment SENSEX_Price_Forecast_Prophet ----

# by VINEET MAHESHWARI

#Libraries ----

# API
library(Quandl)

# Time Series
library(lubridate)
library(timetk)

# Visualization
library(plotly)

# Core
library(tidyverse)
library(tidyquant)
library(data.table)

# Model
library(tidymodels)
library(modeltime)
library(prophet)


# Loading and Visualizing the Data ----

sensex_p <- Quandl("BSE/SENSEX", 
                     start_date = "2020-04-01", 
                     end_date = today(), 
                     order = "asc",api_key="###########") %>% as_tibble()

sensex_p %>% plot_time_series(Date, 
                                Close,
                                .smooth = T,
                                .smooth_size = 0.5,
                                .smooth_alpha = 0.7,
                                .smooth_degree = 2,
                                .smooth_period = "1 year",
                                .interactive = T, 
                                .title = "SENSEX (in Rs.)",
                                .color_lab = "Year",
                                .plotly_slider = F)


# Converting days to weeks----

set_time_unit <- function(data, time_unit = "week", .fun = mean) {
  
  output <- data %>%
    mutate(Date = floor_date(Date, unit = time_unit)) %>%
    
    group_by(Date) %>%
    summarize(Value = .fun(Close)) %>%
    ungroup()
  
  return(output)
}

ssex_new_time_unit <- sensex_p %>% 
  set_time_unit(time_unit = "week",
                .fun = median)

ssex_new_time_unit %>% 
  plot_time_series(Date, 
                   Value,
                   .smooth = FALSE,
                   .interactive = T, 
                   .title = "BSE SENSEX (in Rs.)",
                   .color_lab = "Year",
                   .plotly_slider = F)


# Splitting the data into train/test ----

plot_splits <- function(data, assess = "2 months") {
  
  data %>% 
    tk_time_series_cv_plan() %>%
    plot_time_series_cv_plan(Date, 
                             Value, 
                             .interactive = T, 
                             .title = "BSE SENSEX (in Rs.)",
                             .color_lab = "Year",
                             .plotly_slider = F)
  
}

sensex_p %>% 
  set_time_unit() %>% 
  time_series_split(assess = "2 months", cumulative = TRUE) %>% 
  plot_splits()

splits <- ssex_new_time_unit %>% 
  time_series_split(assess = "2 months", cumulative = TRUE)


# Fitting Prophet Model ----
model_prophet <- prophet_reg(mode = "regression",
                             growth = "linear",
                             season = "additive") %>%
  set_engine("prophet") %>%
  fit(Value ~ Date, training(splits))

model_prophet


# Calibrating the Models ----

models_table <- modeltime_table(
  model_prophet
)

models_table


calibration_table <- models_table %>% 
  modeltime_calibrate(testing(splits))

calibration_table


# Forecasting over Testing Set ----

calibration_table %>%
  modeltime_forecast(actual_data = ssex_new_time_unit) %>%
  plot_modeltime_forecast(.interactive = TRUE)

calibration_table %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = FALSE)

## Function for training and testing prediction ----

fit_test <- function(data, growth = "linear", seasonality = "additive") {
  
  model_prophet <- prophet_reg(mode = "regression",
                               growth = growth,
                               season = seasonality) %>%
    set_engine("prophet") %>%
    fit(Value ~ Date, training(data))
  
  return(model_prophet)
}


splits %>% 
  fit_test(seasonality = "multiplicative") %>% 
  modeltime_calibrate(testing(splits)) %>% 
  modeltime_forecast(actual_data = ssex_new_time_unit) %>%
  plot_modeltime_forecast(.interactive = TRUE)

splits %>% 
  fit_test(seasonality = "additive") %>% 
  modeltime_calibrate(testing(splits)) %>% 
  modeltime_forecast(actual_data = ssex_new_time_unit) %>%
  plot_modeltime_forecast(.interactive = TRUE)


# Refiting Model and FINAL Forecasting ----

calibration_table %>%
  modeltime_refit(ssex_new_time_unit) %>%
  modeltime_forecast(h = "12 months", actual_data = ssex_new_time_unit) %>%
  plot_modeltime_forecast(.interactive = TRUE,
                          .plotly_slider = FALSE)
