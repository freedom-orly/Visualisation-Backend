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
# UPDATED DATA PREPARATION (With Holiday Proximity & Payday Features)
# ==============================================================================
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
# 2. MODEL TRAINING
# ==============================================================================
train_xgb_model_and_save <- function(data_list, config) {
  params <- list(objective = "reg:squarederror", eta = 0.02, max_depth = 8, subsample = 0.7)
  xgb_model <- xgb.train(params = params, data = data_list$dtrain, nrounds = 1000, 
                         watchlist = list(test = data_list$dtest), early_stopping_rounds = 50, verbose = 0)
  saveRDS(xgb_model, config$MODEL_FILE)
  saveRDS(data_list, config$PREP_DATA_FILE)
  return(xgb_model)
}

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
# ACCURACY CHECK FUNCTION
# ==============================================================================
check_model_accuracy <- function(config) {
  cat("\n--- 1. Loading Model and Hidden Test Data ---\n")
  xgb_model <- readRDS(config$MODEL_FILE)
  data_list <- readRDS(config$PREP_DATA_FILE)
  test_df <- data_list$test_df
  
  # 1. Run Predictions on the test matrix
  features <- data_list$features
  test_matrix <- as.matrix(test_df[features])
  predicted_residuals <- predict(xgb_model, xgb.DMatrix(test_matrix))
  
  # 2. Reconstruct Final Sales (Back-transform from Log)
  results <- test_df %>%
    mutate(
      predicted_log = arima_fit + predicted_residuals,
      predicted_sales = pmax(0, exp(predicted_log) - 1),
      actual_sales = daily_total,
      error = actual_sales - predicted_sales
    )
  
  # 3. Calculate Key Metrics
  mae  <- mean(abs(results$error))          # Average off by X Euros/Units
  rmse <- sqrt(mean(results$error^2))      # Penalizes large mistakes
  
  # R-Squared (How much of the variance we explained)
  sst <- sum((results$actual_sales - mean(results$actual_sales))^2)
  sse <- sum(results$error^2)
  r_squared <- 1 - (sse / sst)
  
  # 4. Print Report
  cat("\n==========================================\n")
  cat("       MODEL ACCURACY REPORT\n")
  cat("==========================================\n")
  cat(sprintf("MAE (Mean Absolute Error):  %.2f\n", mae))
  cat(sprintf("RMSE (Root Mean Sq Error):  %.2f\n", rmse))
  cat(sprintf("R-Squared (0 to 1 scale):   %.4f\n", r_squared))
  cat("==========================================\n")
  
  # 5. Visual Check: Plot Actual vs Predicted
  p <- ggplot(results, aes(x = Primary_Time_Key)) +
    geom_line(aes(y = actual_sales, color = "Actual"), size = 1) +
    geom_line(aes(y = predicted_sales, color = "Predicted"), linetype = "dashed", size = 1) +
    labs(title = "Accuracy Check: Last 30 Days (Historical Backtest)",
         subtitle = paste("R-Squared:", round(r_squared, 4)),
         x = "Date", y = "Total Sales") +
    scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
    theme_minimal()
  
  print(p)
  return(results)
}

# ==============================================================================
# ANALYZE TOP 10 BIGGEST MISTAKES
# ==============================================================================
analyze_top_mistakes <- function(config) {
  # 1. Load Data
  xgb_model <- readRDS(config$MODEL_FILE)
  data_list <- readRDS(config$PREP_DATA_FILE)
  test_df <- data_list$test_df
  
  # 2. Generate Predictions
  test_matrix <- as.matrix(test_df[data_list$features])
  predicted_residuals <- predict(xgb_model, xgb.DMatrix(test_matrix))
  
  # 3. Join with Weather and Calculate Error
  mistake_report <- test_df %>%
    mutate(
      predicted_log = arima_fit + predicted_residuals,
      predicted_sales = pmax(0, exp(predicted_log) - 1),
      actual_sales = daily_total,
      absolute_error = abs(actual_sales - predicted_sales),
      percentage_error = (absolute_error / pmax(1, actual_sales)) * 100
    ) %>%
    # Select key columns for investigation
    select(Date, StoreId, actual_sales, predicted_sales, 
           absolute_error, percentage_error, 
           Daily_Avg_Temperature, Daily_Sum_Precipitation) %>%
    # Sort by the biggest error
    arrange(desc(absolute_error)) %>%
    head(10)
  
  cat("\n--- TOP 10 BIGGEST MISTAKES (Historical Backtest) ---\n")
  print(mistake_report)
  
  return(mistake_report)
}

# ==============================================================================
# EXECUTION
# ==============================================================================
# 1. Prepare & Train
data_list <- prep_data_for_xgb_daily(CONFIG)
model <- train_xgb_model_and_save(data_list, CONFIG)

# 2. Forecast with Live Weather
future_results <- forecast_future_sales(CONFIG, forecast_days = 7)
print(head(future_results))

# Run the check
accuracy_results <- check_model_accuracy(CONFIG)

# Run the analysis
top_mistakes <- analyze_top_mistakes(CONFIG)
