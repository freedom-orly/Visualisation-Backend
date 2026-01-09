# ==============================================================================
# LIBRARIES
# ==============================================================================
library(tidyverse)
library(lubridate)
library(xgboost)
library(ggplot2)
library(parallel)
library(forecast) 
library(jsonlite)
library(httr)


# ==============================================================================
# 3. FUTURE FORECASTING (IMPROVED WITH LIVE WEATHER)
# ==============================================================================
forecast_future_sales <- function(config, forecast_days = 7) {
  xgb_model <- readRDS(config$MODEL_FILE)
  data_list <- readRDS(config$PREP_DATA_FILE)
  
  # FETCH LIVE WEATHER
  live_weather <- fetch_emmen_forecast()
  
  temp_history_df <- data_list$full_df
  last_date <- max(temp_history_df$Primary_Time_Key)
  all_stores <- unique(temp_history_df$StoreId)
  final_forecast <- list()
  
  for (i in 1:forecast_days) {
    target_date <- last_date + days(i)
    
    # Get weather for the day BEFORE the target (to create Temp_lag_1)
    weather_info <- live_weather %>% filter(Date == (target_date - days(1)))
    # If API doesn't have it, fallback to historical average
    if(nrow(weather_info) == 0) {
      weather_info <- data.frame(Daily_Avg_Temperature = 15, Daily_Sum_Precipitation = 0)
    }
    
    current_day_df <- temp_history_df %>%
      group_by(StoreId) %>%
      slice_tail(n = 1) %>%
      mutate(
        Primary_Time_Key = target_date,
        day_of_week = wday(target_date),
        day_of_year = yday(target_date),
        month_of_year = month(target_date),
        # SET WEATHER FROM LIVE FORECAST
        Temp_lag_1 = weather_info$Daily_Avg_Temperature,
        Precip_lag_1 = weather_info$Daily_Sum_Precipitation,
        log_daily_total_lag_1 = log_daily_total,
        arima_residual_lag_1 = arima_residual
      ) %>% ungroup()
    
    xgb_mat <- as.matrix(current_day_df[data_list$features])
    preds_res <- predict(xgb_model, xgb.DMatrix(xgb_mat))
    
    current_day_df <- current_day_df %>%
      mutate(arima_residual = preds_res,
             log_predicted = arima_fit + arima_residual,
             predicted_sales = pmax(0, exp(log_predicted) - 1),
             log_daily_total = log_predicted,
             daily_total = predicted_sales)
    
    temp_history_df <- bind_rows(temp_history_df, current_day_df)
    final_forecast[[i]] <- current_day_df %>% select(Primary_Time_Key, StoreId, predicted_sales)
  }
  
  return(bind_rows(final_forecast))
}


# ==============================================================================
# EXECUTION
# ==============================================================================

# 2. Forecast with Live Weather
future_results <- forecast_future_sales(CONFIG, forecast_days = 7)
print(head(future_results))

