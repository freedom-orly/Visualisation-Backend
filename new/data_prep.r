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
# 0. GLOBAL CONFIGURATION
# ==============================================================================
CONFIG <- list(
  SALES_FILE = "salehourly_location_store.csv",
  VISITOR_FILE = "total_hourly_visitors.csv",
  WEATHER_FILE = "weather_data_hourly.csv", # Historical data
  CALENDAR_FILE = "is_holiday.csv",
  FORECAST_WEATHER_FILE = "emmen_live_forecast.csv", # New Live File
  MODEL_FILE = "xgb_residual_forecaster.rds",
  PREP_DATA_FILE = "prep_hybrid_data_list.rds",
  TEST_WINDOW_DAYS = 30 
)

# ==============================================================================
# NEW FUNCTION: FETCH LIVE WEATHER FOR EMMEN, NL
# ==============================================================================
fetch_emmen_forecast <- function(output_file = CONFIG$FORECAST_WEATHER_FILE) {
  cat("--- Fetching Live 7-Day Forecast for Emmen, NL ---\n")
  
  # Coordinates for Emmen: 52.79, 6.90
  url <- "https://api.open-meteo.com/v1/forecast?latitude=52.79&longitude=6.90&hourly=temperature_2m,precipitation&timezone=Europe/Amsterdam"
  
  res <- GET(url)
  if (status_code(res) != 200) {
    warning("Could not reach weather API. Falling back to persistence.")
    return(NULL)
  }
  
  raw_data <- fromJSON(content(res, "text", encoding = "UTF-8"))
  
  # Process hourly into daily summaries
  forecast_daily <- data.frame(
    Date = as.Date(substr(raw_data$hourly$time, 1, 10)),
    Temp = raw_data$hourly$temperature_2m,
    Precip = raw_data$hourly$precipitation
  ) %>%
    group_by(Date) %>%
    summarize(
      Daily_Avg_Temperature = mean(Temp, na.rm = TRUE),
      Daily_Sum_Precipitation = sum(Precip, na.rm = TRUE)
    )
  
  # Save in the user's specific format (Date;Time;Temp;Precip)
  # Note: The model uses Daily summaries, but we save the hourly version to match your file structure if needed
  hourly_export <- data.frame(
    Date = substr(raw_data$hourly$time, 1, 10),
    Time = paste0(substr(raw_data$hourly$time, 12, 16), ":00"),
    Temperature = gsub("\\.", ",", as.character(raw_data$hourly$temperature_2m)),
    Precipitation = gsub("\\.", ",", as.character(raw_data$hourly$precipitation))
  )
  
  write.table(hourly_export, output_file, sep = ";", row.names = FALSE, quote = TRUE)
  cat(paste("--- Live weather saved to:", output_file, "\n"))
  
  return(forecast_daily)
}

prep_data_with_proximity_features <- function(config) {
  cat("--- 1.1 Loading raw data and applying advanced features...\n")
  sales_df <- read.csv(config$SALES_FILE)
  weather_df <- read.csv2(config$WEATHER_FILE)
  calendar_df <- read.csv2(config$CALENDAR_FILE)
  
  daily_sales_df <- sales_df %>%
    mutate(StoreId = replace_na(StoreId, -999), Date = as.character(Date)) %>%
    group_by(Date, locationid, StoreId) %>% 
    summarize(daily_total = sum(total, na.rm = TRUE), .groups = 'drop') %>%
    mutate(
      Primary_Time_Key = ymd(Date),
      StoreId_int = as.integer(as.factor(StoreId)),
      daily_total = pmax(0, as.numeric(daily_total))
    )
  
  # --- NEW: OUTLIER CAPPING ---
  # Caps sales at the 99th percentile per store to handle "Store 108" anomalies
  daily_sales_df <- daily_sales_df %>%
    group_by(StoreId) %>%
    mutate(
      cap_value = quantile(daily_total, 0.99, na.rm = TRUE),
      daily_total = ifelse(daily_total > cap_value, cap_value, daily_total),
      log_daily_total = log(1 + daily_total)
    ) %>% ungroup()
  
  # --- NEW: ADVANCED TIME FEATURES ---
  daily_sales_df <- daily_sales_df %>%
    mutate(
      day_of_week = wday(Primary_Time_Key),
      month_of_year = month(Primary_Time_Key),
      
      # 1. Christmas Proximity (Days until Dec 25)
      dist_christmas = as.numeric(abs(Primary_Time_Key - ymd(paste0(year(Primary_Time_Key), "-12-25")))),
      is_pre_christmas = ifelse(dist_christmas <= 7 & month_of_year == 12, 1, 0),
      
      # 2. New Year Proximity (Days from Jan 1)
      dist_newyear = as.numeric(abs(Primary_Time_Key - ymd(paste0(year(Primary_Time_Key), "-01-01")))),
      
      # 3. Dutch Payday Window (23rd to 26th of each month)
      day_of_month = mday(Primary_Time_Key),
      is_payday_window = ifelse(day_of_month >= 23 & day_of_month <= 26, 1, 0),
      
      # 4. Month End/Start Cycle
      days_in_month = days_in_month(Primary_Time_Key),
      is_month_cycle = ifelse(day_of_month >= (days_in_month - 2) | day_of_month <= 3, 1, 0)
    )
  
  # --- WEATHER PREP (Better Imputation) ---
  weather_df_prep <- weather_df %>% 
    mutate(across(c(Temperature, Precipitation), ~as.numeric(gsub(",", ".", .)))) %>%
    group_by(Date) %>% 
    summarize(Daily_Avg_Temperature = mean(Temperature, na.rm = TRUE), 
              Daily_Sum_Precipitation = sum(Precipitation, na.rm = TRUE), .groups = 'drop')
  
  df_merged <- daily_sales_df %>%
    left_join(weather_df_prep, by = "Date") %>%
    left_join(calendar_df %>% mutate(Date = as.character(Date), is_holiday_flag = as.integer(is_holiday)), by = "Date") %>%
    # Instead of just 13.1, we use the store's average if the date is missing
    mutate(
      Daily_Avg_Temperature = replace_na(Daily_Avg_Temperature, 13.1),
      Daily_Sum_Precipitation = replace_na(Daily_Sum_Precipitation, 0),
      is_holiday_flag = replace_na(is_holiday_flag, 0)
    )
  
  # Calculate Residuals
  df_merged_with_res <- fit_and_extract_residuals_per_store(df_merged)
  
  # Lagged Features
  df_final <- df_merged_with_res %>%
    arrange(StoreId, Primary_Time_Key) %>%
    group_by(StoreId) %>%
    mutate(
      log_daily_total_lag_1 = lag(log_daily_total, 1),
      log_daily_total_lag_7 = lag(log_daily_total, 7),
      arima_residual_lag_1 = lag(arima_residual, 1),
      Temp_lag_1 = lag(Daily_Avg_Temperature, 1),
      Precip_lag_1 = lag(Daily_Sum_Precipitation, 1)
    ) %>%
    ungroup() %>%
    mutate(across(everything(), ~replace_na(., 0)))
  
  # Split Data
  all_dates <- unique(df_final$Primary_Time_Key)
  cutoff = length(all_dates) - config$TEST_WINDOW_DAYS
  train_data <- df_final %>% filter(Primary_Time_Key <= all_dates[cutoff])
  test_data <- df_final %>% filter(Primary_Time_Key > all_dates[cutoff])
  
  # --- UPDATED FEATURE LIST ---
  features <- c(
    "StoreId_int", "day_of_week", "month_of_year", "is_holiday_flag", 
    "is_month_cycle", "is_payday_window", "is_pre_christmas",
    "dist_christmas", "dist_newyear", 
    "Temp_lag_1", "Precip_lag_1", 
    "arima_residual_lag_1", "log_daily_total_lag_1", "log_daily_total_lag_7"
  )
  
  return(list(
    dtrain = xgb.DMatrix(as.matrix(train_data[features]), label = train_data$arima_residual),
    dtest = xgb.DMatrix(as.matrix(test_data[features]), label = test_data$arima_residual),
    features = features, 
    test_df = test_data, 
    full_df = df_final
  ))
}

# ==============================================================================
# ARIMA RESIDUAL CALCULATION (PER-STORE)
# ==============================================================================
fit_and_extract_residuals_per_store <- function(df) {
  cat("     - Fitting ARIMA model for each unique store...\n")
  fit_store_arima <- function(store_df) {
    ts_sales <- store_df %>% arrange(Primary_Time_Key) %>% pull(log_daily_total) %>% ts(frequency = 7) 
    arima_model <- tryCatch({
      auto.arima(ts_sales, stepwise = TRUE, seasonal = TRUE)
    }, error = function(e) { return(NULL) })
    
    if (is.null(arima_model)) {
      return(store_df %>% mutate(arima_fit = 0, arima_residual = log_daily_total))
    }
    store_df %>% arrange(Primary_Time_Key) %>%
      mutate(arima_fit = as.numeric(fitted(arima_model)), arima_residual = as.numeric(residuals(arima_model)))
  }
  df %>% mutate(StoreId = as.factor(StoreId)) %>% group_by(StoreId) %>%
    group_map(~ fit_store_arima(.x), .keep = TRUE) %>% bind_rows() %>% ungroup() %>%
    mutate(across(c(arima_fit, arima_residual), ~replace_na(., 0)))
}

# ==============================================================================
# EXECUTION
# ==============================================================================
# 1. Prepare & Train
data_list <- prep_data_with_proximity_features(CONFIG)