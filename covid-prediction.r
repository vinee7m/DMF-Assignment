#DMF PROJECT - by GROUP 5

# LIBRARIES ----
# Core
library(tidyverse)
library(tibble)
library(lubridate)

# Models
library(modeltime)
library(tidymodels)

# Visualization
library(plotly)
library(timetk)


# EXTRACTING THE DATA ----

# Data downloaded from the COVID-19 Data Repository by the Center for Systems
# Science and Engineering (CSSE) at Johns Hopkins University

# Daily number of cases (cumulative)
df_cases <- read.csv(
  "https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",
  sep = ","
) %>%
  as_tibble()

# Daily number of death (cumulative)
df_death <- read.csv(
  "https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv",
  sep = ","
) %>%
  as_tibble()


# GETTING DATA FOR SPECIFIED COUNTRY ----

country <- "India"

df_country <- df_cases[df_cases$Country.Region == country, ] %>%
  select(-c(Province.State, Lat, Long, Country.Region)) %>%
  select(-c(X1.22.20:X11.22.21)) %>%
  select(-c(X1.22.22:X6.20.22))

## changing date header format ----

dates <- colnames(df_country) %>%
  str_replace("X", " ") %>%
  mdy()

## Cases: cumulative to new daily ----
stacked_cases <- df_country %>%
  colSums()

diff_cases <- diff(c(0, stacked_cases))

daily <- tibble(Date = dates,
                Cases = diff_cases)

## Death: cumulative to new daily ----
df_country <- df_death[df_death$Country.Region == country, ] %>%
  select(-c(Province.State, Lat, Long, Country.Region))  %>%
  select(-c(X1.22.20:X11.22.21)) %>%
  select(-c(X1.22.22:X6.20.22))

## changing date header format ----

dates <- colnames(df_country) %>%
  str_replace("X", " ") %>%
  mdy()

stacked_death <- df_country %>%
  colSums()

diff_death <- diff(c(0, stacked_death))

daily$Death <- diff_death

## Deleting first row ----
daily <- daily[-1, ]

## Plotting both data ----

plot_ly(data = daily) %>%
  add_lines(
    x = ~ Date,
    y = ~ Cases,
    name = "New Cases",
    fill = 'tozeroy',
    alpha = 0.3,
    line = list(width = 1)
  ) %>%
  add_lines(
    x = ~ Date,
    y = ~ Death,
    name = "Death",
    yaxis = "y2",
    fill = 'tozeroy',
    alpha = 0.3,
    line = list(width = 1)
  ) %>%
  layout(
    title = str_glue("Daily COVID-19 New Cases and Deaths ({country})"),
    yaxis2 = list(
      overlaying = "y",
      side = "right",
      title = "Death",
      range = c(0, 2.5 * max(daily$Death)),
      color = "orange"
    ),
    yaxis = list(
      title = "New Cases",
      range = c(0, 1.1 * max(daily$Cases)),
      color = "darkblue"
    ),
    xaxis = list(title = "Date"),
    legend = list(
      x = 0.01,
      y = 0.99,
      title = list(text = '<b> Legend </b>')
    )
  )


# DEFINING TRAIN/TEST DATASET ----

cases <- c("Date", "Cases")

splits <- daily[, cases] %>%
  time_series_split(assess = "2 weeks", cumulative = TRUE)

splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(
    Date,
    Cases,
    .interactive = T,
    .title = str_glue("Daily COVID-19 New Cases ({country})"),
    .x_lab = "Date",
    .y_lab = "Number of Cases",
    .plotly_slider = T
  ) %>%
  layout(legend = list(
    x = 0.01,
    y = 0.99,
    title = list(text = '<b> Legend </b>')
  ))


# FITTING MODELS ----

## Auto-Arima ----

model_arima <- arima_reg() %>%
  set_engine(engine = "auto_arima") %>%
  fit(Cases ~ Date, data = training(splits))

model_arima

## Facebook Prophet ----

model_prophet <- prophet_reg(
  mode = "regression",
  growth = "linear",
  changepoint_num = 10,
  changepoint_range = 0.9,
  seasonality_yearly = FALSE,
  seasonality_weekly = TRUE,
  seasonality_daily = FALSE,
  season = "multiplicative",
  prior_scale_changepoints = 1,
  prior_scale_seasonality = 1
) %>%
  set_engine("prophet") %>%
  fit(Cases ~ Date, training(splits))

model_prophet


# CALIBRATING THE MODELS ----

models_table <- modeltime_table(model_prophet,
                                model_arima)

models_table

calibration_table <- models_table %>%
  modeltime_calibrate(testing(splits))

calibration_table


# FORECASTING OVER TEST SET ----

calibration_table %>%
  modeltime_forecast(actual_data = daily[, cases]) %>%
  plot_modeltime_forecast(
    .interactive = TRUE,
    .x_lab = "Date",
    .y_lab = "Number of Cases",
    .title = str_glue("Daily COVID-19 New Cases ({country})"),
    .line_size = 0.3,
    .plotly_slider = TRUE
  ) %>%
  layout(legend = list(
    x = 0.01,
    y = 0.99,
    title = list(text = '<b> Legend </b>')
  ))

calibration_table %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = FALSE)


# FINAL FORECAST ----

calibration_table %>%
  modeltime_refit(daily[, cases]) %>%
  modeltime_forecast(h = "10 days", actual_data = daily[, cases]) %>%
  plot_modeltime_forecast(
    .interactive = TRUE,
    .x_lab = "Date",
    .y_lab = "Number of Cases",
    .title = str_glue("Daily COVID-19 New Cases ({country})"),
    .line_size = 0.3,
    .plotly_slider = TRUE
  ) %>%
  layout(legend = list(
    x = 0.01,
    y = 0.99,
    title = list(text = '<b> Legend </b>')
  ))

# END - by VINEET, JAYITA and SHRISH
