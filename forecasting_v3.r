library(tidyverse)
library(lubridate)
library(xgboost)
library(ggplot2)
library(parallel)
library(forecast) # REQUIRED FOR ARIMA

# ==============================================================================
# 0. GLOBAL CONFIGURATION AND FILE PATHS
# ==============================================================================
CONFIG <- list(
  # Input files
  SALES_FILE = "salehourly_location_store.csv",
  VISITOR_FILE = "total_hourly_visitors.csv",
  WEATHER_FILE = "weather_data_hourly.csv",
  CALENDAR_FILE = "is_holiday.csv",
  
  # Output files
  MODEL_FILE = "xgb_residual_forecaster.rds", #  model file
  PREP_DATA_FILE = "prep_hybrid_data_list.rds", # data file
  
  # Time windows
  TEST_WINDOW_DAYS = 30 
)

# ==============================================================================
# 0.5. ARIMA RESIDUAL CALCULATION FUNCTION (PER-STORE HYBRID)
# NOTE: This fits an ARIMA model to the individual sales time series for each store.
# This ensures the ARIMA fit is correctly scaled for the individual store.
# ==============================================================================
fit_and_extract_residuals_per_store <- function(df) {
  
  cat("     - Fitting ARIMA model for each unique store...\n")
  
  # Function to fit ARIMA to a single store's data
  fit_store_arima <- function(store_df) {
    
    # Sort and convert to time series
    ts_sales <- store_df %>%
      arrange(Primary_Time_Key) %>%
      pull(log_daily_total) %>%
      # Ensure frequency is set to 7 for weekly seasonality
      ts(frequency = 7) 
    
    # Fit the ARIMA model, suppressing complex warnings
    arima_model <- tryCatch({
      auto.arima(ts_sales, stepwise = TRUE, parallel = FALSE, seasonal = TRUE, trace = FALSE)
    }, error = function(e) {
      # Fallback for stores with insufficient or zero variance data (returns NA fit/res)
      return(NULL)
    })
    
    if (is.null(arima_model)) {
      return(store_df %>% mutate(arima_fit = 0, arima_residual = log_daily_total)) # Treat entire sales as residual
    }
    
    # Extract fitted values and residuals
    store_df_results <- store_df %>%
      arrange(Primary_Time_Key) %>%
      mutate(
        arima_fit = as.numeric(fitted(arima_model)),
        arima_residual = as.numeric(residuals(arima_model))
      )
    
    return(store_df_results)
  }
  
  # Group the entire dataset by StoreId and apply the fitting function
  df_hybrid <- df %>%
    # Ensure data types are compatible for grouping and processing
    mutate(StoreId = as.factor(StoreId)) %>%
    group_by(StoreId) %>%
    # Use nest/map to safely apply the function store by store
    group_map(~ fit_store_arima(.x), .keep = TRUE) %>%
    bind_rows() %>%
    ungroup()
  
  # Clean up potential NAs introduced by the fit for the first few rows
  df_hybrid <- df_hybrid %>%
    mutate(
      across(c(arima_fit, arima_residual), ~replace_na(., 0))
    )
  
  return(df_hybrid)
}

# ==============================================================================
# 1. DATA PREPARATION FUNCTION (prep_data_for_xgb - HYBRID DAILY)
# ==============================================================================
prep_data_for_xgb_daily <- function(config) {
  
  cat("--- 1.1 Loading raw data...\n")
  sales_df <- read.csv(config$SALES_FILE)
  visitor_df <- read.csv(config$VISITOR_FILE)
  weather_df <- read.csv2(config$WEATHER_FILE)
  calendar_df <- read.csv2(config$CALENDAR_FILE)
  
  # --- Steps 1, 2, 3 (Aggregation and Merging) ---
  daily_sales_df <- sales_df %>%
    mutate(
      StoreId = replace_na(StoreId, -999), locationid = replace_na(locationid, 999), Date = as.character(Date)
    ) %>%
    group_by(Date, locationid, StoreId) %>% summarize(daily_total = sum(total, na.rm = TRUE), .groups = 'drop') %>%
    mutate(
      Primary_Time_Key = ymd(Date), StoreId_int = as.integer(as.factor(StoreId)), daily_total = as.numeric(daily_total)
    ) %>%
    mutate(day_of_week = wday(Primary_Time_Key, label = FALSE), day_of_year = yday(Primary_Time_Key), month_of_year = month(Primary_Time_Key)
    )
  
  # ************ FIX: ADD MISSING FEATURE CREATION HERE ************
  # *** NEW FEATURE: Month-End/Start Cycle Flag ***
  daily_sales_df <- daily_sales_df %>%
    mutate(
      day_of_month = mday(Primary_Time_Key),
      days_in_month = days_in_month(Primary_Time_Key),
      # Flag if within the last 3 days of the month or first 3 days of the month
      is_month_cycle = ifelse(
        day_of_month >= (days_in_month - 2) | day_of_month <= 3, 
        1, 0)
    )
  # ***************************************************************
  
  # *** CRITICAL FIX: Ensure Sales are Non-Negative before Log Transformation ***
  daily_sales_df <- daily_sales_df %>%
    mutate(daily_total = ifelse(daily_total < 0, 0, daily_total)) # Set any negative sales to 0
  
  # *** NEW CODE: Apply Log(1 + y) Transformation ***
  daily_sales_df <- daily_sales_df %>%
    mutate(log_daily_total = log(1 + daily_total))
  
  weather_df_prep_daily <- weather_df %>% mutate(Date = as.character(Date)) %>%
    mutate(across(c(Temperature, Precipitation), ~as.numeric(gsub(",", ".", .)))) %>%
    group_by(Date) %>% summarize(Daily_Avg_Temperature = mean(Temperature, na.rm = TRUE), Daily_Sum_Precipitation = sum(Precipitation, na.rm = TRUE), .groups = 'drop')
  
  visitor_df_prep_daily <- visitor_df %>% mutate(Date = as.character(Date)) %>% rename(hourly_visitor_count = total_visitors) %>%
    group_by(Date) %>% summarize(Daily_Visitor_Count = sum(hourly_visitor_count, na.rm = TRUE), .groups = 'drop')
  
  calendar_df_prep <- calendar_df %>% mutate(Date = as.character(Date), is_holiday_flag = as.integer(is_holiday))
  
  df_merged <- daily_sales_df %>%
    left_join(weather_df_prep_daily, by = c("Date")) %>%
    left_join(visitor_df_prep_daily, by = c("Date")) %>%
    left_join(calendar_df_prep, by = c("Date")) %>%
    mutate(
      across(c(Daily_Avg_Temperature, Daily_Sum_Precipitation), ~replace_na(., mean(., na.rm = TRUE))),
      across(c(Daily_Visitor_Count, is_holiday_flag), ~replace_na(., 0))
    )
  
  # --- Step 3.5: HYBRID: Calculate ARIMA Residuals ---
  cat("--- 1.3a Calculating ARIMA residuals (Hybrid Step)...\n")
  # *** CRITICAL FIX: Calling the per-store function ***
  df_merged_with_res <- fit_and_extract_residuals_per_store(df_merged)
  
  # --- Step 4: Lagged Features (Time Series, Volatility, and ARIMA Residual) ---
  cat("--- 1.3b Creating lagged and volatility features...\n")
  df_final <- df_merged_with_res %>%
    arrange(StoreId, Primary_Time_Key) %>%
    group_by(StoreId) %>%
    mutate(
      
      log_daily_total_lag_1 = lag(log_daily_total, n = 1),
      log_daily_total_lag_7 = lag(log_daily_total, n = 7),
      log_daily_total_lag_30 = lag(log_daily_total, n = 30),
      
      # ** NEW FEATURE: Lagged ARIMA Residual **
      arima_residual_lag_1 = lag(arima_residual, n = 1),
      
      # Sales Volatility (rolling 7-day std deviation)
      sales_std_7d = slider::slide_dbl(daily_total, sd, .before = 6, .after = 0, .complete = FALSE),
      # *** NEW FEATURE: Log Sales Volatility ***
      log_sales_std_7d = slider::slide_dbl(log_daily_total, sd, .before = 6, .after = 0, .complete = FALSE),
      
      # Lagged External Regressors
      Temp_lag_1 = lag(Daily_Avg_Temperature, n = 1),
      # *** ADDED: Lagged Precipitation ***
      Precip_lag_1 = lag(Daily_Sum_Precipitation, n = 1),
      Visitors_lag_1 = lag(Daily_Visitor_Count, n = 1)
    ) %>%
    mutate(
      across(starts_with("log_daily_total_lag_"), ~replace_na(., 0)),
      across(starts_with("arima_residual_lag_"), ~replace_na(., 0)), # Impute residual lag
      
      # *** Imputation for Lagged Weather ***
      across(starts_with("Temp_lag_"), ~replace_na(., mean(., na.rm = TRUE))),
      across(starts_with("Precip_lag_"), ~replace_na(., 0)), # Impute missing (likely first few days) with 0
      
      across(starts_with("Visitors_lag_"), ~replace_na(., 0)),
      across(starts_with("sales_std_7d"), ~replace_na(., mean(., na.rm = TRUE))),
      across(starts_with("log_sales_std_7d"), ~replace_na(., mean(., na.rm = TRUE)))
    ) %>%
    ungroup()
  
  # --- Step 5: Split and Create DMatrix ---
  df_ready_to_slice <- df_final %>% as.data.frame()
  
  # ... (Split logic remains the same) ...
  all_dates <- unique(df_ready_to_slice$Primary_Time_Key)
  num_dates <- length(all_dates)
  test_window_dates <- config$TEST_WINDOW_DAYS
  cutoff_date_index <- num_dates - test_window_dates
  train_end_date <- all_dates[cutoff_date_index]
  test_start_date <- all_dates[cutoff_date_index + 1]
  train_data <- df_ready_to_slice %>% filter(Primary_Time_Key <= train_end_date)
  test_data <- df_ready_to_slice %>% filter(Primary_Time_Key >= test_start_date)
  cat(sprintf("Training Data ends: %s. Testing Data starts: %s\n", train_end_date, test_start_date))
  
  # Final features vector
  features <- c(
    "StoreId_int", "day_of_year", "day_of_week", "month_of_year",
    "is_holiday_flag",
    "is_month_cycle",
    "Visitors_lag_1", "sales_std_7d",
    "log_sales_std_7d",
    "Temp_lag_1",   # *** ADDED WEATHER FEATURE ***
    "Precip_lag_1", # *** ADDED WEATHER FEATURE ***
    "arima_residual_lag_1",
    "log_daily_total_lag_1", "log_daily_total_lag_7", "log_daily_total_lag_30"
  )
  
  train_data <- train_data %>% filter(complete.cases(select(., all_of(features))))
  
  train_matrix_data <- train_data %>% mutate(across(all_of(features), as.numeric)) %>% select(all_of(features)) %>% as.matrix()
  test_matrix_data <- test_data %>% mutate(across(all_of(features), as.numeric)) %>% select(all_of(features)) %>% as.matrix()
  
  # *** CRITICAL CHANGE: XGBoost target is now the ARIMA Residual! ***
  dtrain <- xgb.DMatrix(data = train_matrix_data, label = train_data$arima_residual)
  # The test label is still the residual for evaluation
  dtest <- xgb.DMatrix(data = test_matrix_data, label = test_data$arima_residual)
  
  return(list(
    dtrain = dtrain,
    dtest = dtest,
    features = features,
    test_df = test_data # This contains arima_fit needed for final prediction
  ))
}

# ==============================================================================
# 2. MODEL TRAINING AND SAVING FUNCTION (train_xgb_model_and_save)
# ==============================================================================
train_xgb_model_and_save <- function(data_list, config, nrounds = 3000) {
  cat("\n--- 2.1 Training XGBoost model (Residuals)...\n")
  params <- list(
    objective = "reg:squarederror", # MAE Objective is superior for residual modeling
    eta = 0.02, 
    max_depth = 8, 
    subsample = 0.7,
    colsample_bytree = 0.7,
    nthread = parallel::detectCores() - 1
  )
  
  xgb_model <- xgb.train(
    params = params,
    data = data_list$dtrain,
    nrounds = nrounds,
    watchlist = list(test = data_list$dtest),
    early_stopping_rounds = 50,
    print_every_n = 50,
    verbose = 0
  )
  
  cat(paste("--- Training complete. Best iteration:", xgb_model$best_iteration, "\n"))
  
  # ** ESSENTIAL DIAGNOSTIC STEP: Check Feature Importance **
  importance_matrix <- xgb.importance(feature_names = data_list$features, model = xgb_model)
  cat("\n--- Top 10 Feature Importance (Residuals) ---\n")
  print(head(importance_matrix, 10))
  cat("-----------------------------------\n")
  
  saveRDS(xgb_model, file = config$MODEL_FILE)
  cat(paste("--- Model saved to:", config$MODEL_FILE, "\n"))
  
  saveRDS(data_list, file = config$PREP_DATA_FILE)
  cat(paste("--- Prepared data saved to:", config$PREP_DATA_FILE, "\n"))
  
  return(xgb_model)
}

# ==============================================================================
# 3. MODEL LOADING, TESTING, AND PLOTTING FUNCTION (load_and_test_forecast - HYBRID)
# ==============================================================================
load_and_test_forecast_daily <- function(config, plot_store_id = NULL) {
  
  # --- Step 3.1: Load Model and Data ---
  if (!file.exists(config$MODEL_FILE) || !file.exists(config$PREP_DATA_FILE)) {
    stop("Error: Model or prepared data file not found. Run the 'TRAINING WORKFLOW' first!")
  }
  
  cat("--- 3.1 Loading model and test data from disk...\n")
  xgb_model <- readRDS(config$MODEL_FILE)
  data_list <- readRDS(config$PREP_DATA_FILE)
  
  # *** RECREATE DMATRIX ***
  cat("--- 3.1b Recreating DMatrix for prediction...\n")
  
  features <- data_list$features
  test_matrix_data <- data_list$test_df %>%
    mutate(across(all_of(features), as.numeric)) %>%
    select(all_of(features)) %>%
    as.matrix()
  dtest_recreated <- xgb.DMatrix(data = test_matrix_data)
  
  # --- Step 3.2: Run Predictions and Calculate Metrics ---
  cat("--- 3.2 Running historical backtest...\n")
  # XGBoost predicts the non-linear error component (the residual)
  xgb_residual_predictions <- predict(xgb_model, dtest_recreated)
  
  # *** FINAL HYBRID PREDICTION: ARIMA Fit + XGBoost Residual ***
  results_df <- data_list$test_df %>%
    mutate(
      actual = daily_total, # The original actual sales
      # HYBRID PREDICTION FORMULA (Log Scale):
      log_predicted = arima_fit + xgb_residual_predictions,
      # *** CRITICAL CHANGE: INVERSE TRANSFORMATION & PINNING TO ZERO ***
      predicted = exp(log_predicted) - 1,
      predicted = ifelse(predicted < 0, 0, predicted), # Pin to 0 (cannot have negative sales)
      error = actual - predicted
    ) %>%
    select(Primary_Time_Key, StoreId, actual, predicted, error)
  
  # --- ERROR METRICS CALCULATION (Metrics calculation remains the same) ---
  mae <- mean(abs(results_df$error), na.rm = TRUE)
  rmse <- sqrt(mean(results_df$error^2, na.rm = TRUE))
  smape_df <- results_df %>%
    mutate(smape_element = 2 * abs(actual - predicted) / (abs(actual) + abs(predicted))) %>%
    filter(!is.nan(smape_element))
  smape <- mean(smape_df$smape_element, na.rm = TRUE) * 100
  results_for_mape <- results_df %>% filter(actual > 0)
  mape <- if (nrow(results_for_mape) > 0) {
    mean(abs(results_for_mape$error / results_for_mape$actual), na.rm = TRUE) * 100
  } else { NA }
  
  # *** ADD R-SQUARED CALCULATION HERE ***
  # Sum of Squares Total (SST): Variance in actual sales
  sst <- sum((results_df$actual - mean(results_df$actual, na.rm = TRUE))^2, na.rm = TRUE)
  # Sum of Squares Residual (SSE): Unexplained variance (error squared)
  sse <- sum(results_df$error^2, na.rm = TRUE)
  # R-squared: 1 - (SSE / SST)
  rsquared <- 1 - (sse / sst)
  
  metrics <- list(MAE = mae, RMSE = rmse, Rsquared = rsquared, MAPE = mape, SMAPE = smape) 
  
  cat("\n--- Historical Forecast Performance (HYBRID Daily Sales) ---\n")
  cat(sprintf("MAE (Mean Absolute Error): %.2f\n", mae))
  cat(sprintf("RMSE (Root Mean Square Error): %.2f\n", rmse))
  cat(sprintf("Rsquared: %.4f\n", rsquared)) # <-- NEW PRINT LINE
  cat(sprintf("SMAPE (Symmetric Mean Absolute %% Error): %.2f%%\n", smape))
  # ... (rest of the function)
  zero_sales_count <- results_df %>% filter(actual == 0) %>% nrow()
  cat(sprintf("Number of rows with 0 actual daily sales in test set: %d\n", zero_sales_count))
  
  # --- Step 3.3: Plotting Logic (FIXED FOR LAST 10 DAYS) ---
  
  plot_data <- results_df
  plot_title_suffix <- "Overall"
  
  # 1. Store ID Filtering
  if (!is.null(plot_store_id)) {
    plot_data <- plot_data %>% filter(StoreId == as.numeric(plot_store_id))
    plot_title_suffix <- paste("Store ID:", plot_store_id)
  }
  
  # 2. Filter for the Last 10 Unique Dates 
  unique_dates <- unique(plot_data$Primary_Time_Key)
  if(length(unique_dates) > 10) { 
    last_10_dates <- tail(unique_dates, 10)
    plot_data <- plot_data %>% filter(Primary_Time_Key %in% last_10_dates)
    plot_title_suffix <- paste0(plot_title_suffix, " (Last 10 Days)")
  } else {
    plot_title_suffix <- paste0(plot_title_suffix, " (Full Test Window)")
  }
  
  # 3. Create Plot
  if(nrow(plot_data) > 0) {
    cat(paste("\nDEBUG: Plotting", nrow(plot_data), "rows of data.\n"))
    p1 <- ggplot(plot_data, aes(x = Primary_Time_Key)) +
      geom_line(aes(y = actual, color = "Actual Daily Sales"), linewidth = 1.0) + # Increased line thickness
      geom_point(aes(y = actual), color = "blue", size = 2) + # Add points for clarity
      geom_line(aes(y = predicted, color = "Predicted Daily Sales"), linewidth = 1.0, linetype = "dashed") +
      geom_point(aes(y = predicted), color = "red", size = 2, shape = 1) +
      labs(
        title = paste("Actual vs. Predicted Daily Sales (", plot_title_suffix, ")"),
        x = "Date (Month/Day)",
        y = "Total Daily Sales",
        color = "Legend"
      ) +
      scale_color_manual(values = c("Actual Daily Sales" = "blue", "Predicted Daily Sales" = "red")) +
      # FIX: Set breaks to 1 day and aggressively rotate labels for readability
      scale_x_date(date_breaks = "1 day", date_labels = "%m/%d") +
      theme_minimal() +
      theme(
        legend.position = "bottom", 
        axis.text.x = element_text(angle = 65, hjust = 1) # Increased angle for readability
      )
    
    print(p1)
  } else {
    warning("Plot data is empty after filtering. Check your Store ID. (0 rows found)")
  }
  
  return(list(metrics = metrics))
}



# ==============================================================================
# 5. FUTURE FORECASTING FUNCTION (forecast_future_sales - HYBRID) - CORRECTED
# ==============================================================================
forecast_future_sales <- function(config, forecast_days = 7) {
  
  # --- 5.1: Load Model and Data ---
  if (!file.exists(config$MODEL_FILE) || !file.exists(config$PREP_DATA_FILE)) {
    stop("Error: Model or prepared data file not found. Run the 'FULL TRAINING WORKFLOW' first!")
  }
  
  cat("--- 5.1 Loading model and prepared data from disk...\n")
  xgb_model <- readRDS(config$MODEL_FILE)
  data_list <- readRDS(config$PREP_DATA_FILE)
  
  # We start the forecast from the last known historical date (the last day of the test set)
  historical_df <- data_list$test_df
  
  # CRITICAL: To calculate lags correctly, we need to append the training data to the test data.
  # The original script does not save the training data, so we'll re-load the raw data,
  # re-run data prep, and use the full df_final before the split as the history base.
  # Rerunning prep_data_for_xgb_daily up to the splitting point:
  cat("--- 5.1b Re-preparing full historical data for accurate lag calculation...\n")
  
  # Temporarily call the prep function to get the full df_final *before* the split
  # NOTE: This re-runs the slow ARIMA step! For production, save 'df_final' in 'prep_data_for_xgb_daily'.
  full_prep_df_list <- prep_data_for_xgb_daily(config)
  # We need to use the full data before the test split for complete history
  # Since prep_data_for_xgb_daily only returned split data, we'll use the last known point from test_df
  # and **rely on the training and testing data being in the same directory** to construct the full historical context.
  # For now, we will assume `data_list$test_df` is sufficient for the immediate lags and rely on imputation for older, missing lags.
  # To avoid the R error, we must use the test set as the start of the history.
  
  # --- Use the last known data point (test_df) as the base for the history ---
  temp_history_df <- historical_df
  last_historical_date <- max(temp_history_df$Primary_Time_Key)
  cat(sprintf("Last observed date for forecasting base: %s\n", last_historical_date))
  
  # --- 5.2: Create Future Data Frame (Simulation) ---
  future_dates <- seq(last_historical_date + days(1), by = "day", length.out = forecast_days)
  all_stores <- unique(historical_df$StoreId)
  
  future_forecast_df <- expand.grid(
    Primary_Time_Key = future_dates,
    StoreId = all_stores,
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      Date = format(Primary_Time_Key, "%Y-%m-%d"),
      StoreId_int = as.integer(as.factor(StoreId)) + 1000,
      day_of_week = wday(Primary_Time_Key, label = FALSE),
      day_of_year = yday(Primary_Time_Key),
      month_of_year = month(Primary_Time_Key)
    )
  
  # --- 5.3: Iterative Forecasting (Required for Lagged Features) ---
  final_forecast_list <- list()
  
  cat(sprintf("--- 5.3 Generating %d-day iterative forecast for %d stores...\n", forecast_days, length(all_stores)))
  
  for (i in 1:forecast_days) {
    target_date <- future_dates[i]
    cat(sprintf("   - Forecasting Day %d of %d: %s\n", i, forecast_days, target_date))
    
    # a. Get **all** lagged features from the current `temp_history_df`
    all_lag_features <- temp_history_df %>%
      group_by(StoreId) %>%
      arrange(Primary_Time_Key) %>%
      # Calculate the required lags relative to the current max date in history
      mutate(
        log_daily_total_lag_1 = lag(log_daily_total, n = 1),
        log_daily_total_lag_7 = lag(log_daily_total, n = 7),
        log_daily_total_lag_30 = lag(log_daily_total, n = 30),
        arima_residual_lag_1 = lag(arima_residual, n = 1),
        sales_std_7d = slider::slide_dbl(daily_total, sd, .before = 6, .after = 0, .complete = FALSE),
        log_sales_std_7d = slider::slide_dbl(log_daily_total, sd, .before = 6, .after = 0, .complete = FALSE),
        Temp_lag_1 = lag(Daily_Avg_Temperature, n = 1),
        Precip_lag_1 = lag(Daily_Sum_Precipitation, n = 1),
        Visitors_lag_1 = lag(Daily_Visitor_Count, n = 1),
        arima_fit_for_forecast = arima_fit # Use the last known ARIMA fit value
      ) %>%
      # Select only the row corresponding to the time key *before* the target date
      slice_tail(n = 1) %>%
      ungroup() %>%
      select(StoreId, starts_with("log_daily_total_lag_"), arima_residual_lag_1, sales_std_7d, log_sales_std_7d, Temp_lag_1, Precip_lag_1, Visitors_lag_1, arima_fit_for_forecast)
    
    # b. Merge with future template and add simulated exogenous features
    current_day_df <- future_forecast_df %>%
      filter(Primary_Time_Key == target_date) %>%
      left_join(all_lag_features, by = "StoreId") %>%
      # --- Add Required Simulated Features for the forecast date (not lagged) ---
      mutate(
        is_holiday_flag = ifelse(wday(target_date) %in% c(1, 7), 1, 0), # Simple weekend holiday rule
        day_of_month = mday(target_date),
        days_in_month = lubridate::days_in_month(target_date),
        is_month_cycle = ifelse(day_of_month >= (days_in_month - 2) | day_of_month <= 3, 1, 0)
      )
    
    # c. Impute all missing values (especially older lags) to 0 or mean
    current_day_df <- current_day_df %>%
      mutate(
        across(c("Temp_lag_1", "Precip_lag_1", "Visitors_lag_1"), ~replace_na(., 0)), # Use 0 for weather/visitors when history is too short
        across(starts_with("log_daily_total_lag_"), ~replace_na(., 0)),
        across(starts_with("arima_residual_lag_"), ~replace_na(., 0)),
        across(c(sales_std_7d, log_sales_std_7d), ~replace_na(., mean(., na.rm = TRUE)))
      )
    
    # d. Prepare DMatrix for XGBoost prediction (Residual)
    features <- data_list$features # Use the same features as training
    
    # This step now works because all 'features' are present due to step a/b/c
    xgb_predict_matrix <- current_day_df %>%
      mutate(across(all_of(features), as.numeric)) %>%
      select(all_of(features)) %>%
      as.matrix()
    
    dforecast <- xgb.DMatrix(data = xgb_predict_matrix)
    arima_fit_for_forecast <- current_day_df$arima_fit_for_forecast # Extract the ARIMA fit component
    
    # e. Run XGBoost Prediction (Residual)
    xgb_residual_predictions <- predict(xgb_model, dforecast)
    
    # f. Calculate Final Hybrid Prediction
    current_day_df <- current_day_df %>%
      mutate(
        # HYBRID PREDICTION FORMULA (Log Scale): ARIMA Fit + XGBoost Residual
        log_predicted = arima_fit_for_forecast + xgb_residual_predictions,
        # INVERSE TRANSFORMATION & PINNING TO ZERO
        predicted_sales = exp(log_predicted) - 1,
        predicted_sales = ifelse(predicted_sales < 0, 0, predicted_sales), # Pin to 0
        # NEW values for the next iteration's history
        log_daily_total = log_predicted,
        arima_residual = xgb_residual_predictions,
        daily_total = predicted_sales,
        Primary_Time_Key = target_date,
        Daily_Avg_Temperature = Temp_lag_1, # Use the predicted lag as the new value for the next lag
        Daily_Sum_Precipitation = Precip_lag_1,
        Daily_Visitor_Count = Visitors_lag_1,
        arima_fit = arima_fit_for_forecast # Carry the ARIMA component for the next step's history
      )
    
    # g. Append the current day's prediction to the history for the next lag calculation
    temp_history_df <- temp_history_df %>%
      bind_rows(current_day_df)
    
    # h. Save the final result for the current day
    final_forecast_list[[i]] <- current_day_df %>%
      select(Primary_Time_Key, StoreId, predicted_sales)
  }
  
  # --- 5.4: Final Output and Saving ---
  final_forecast_df <- bind_rows(final_forecast_list)
  
  forecast_filename <- paste0("future_forecast_daily_", Sys.Date(), ".csv")
  #write.csv(final_forecast_df, file = forecast_filename, row.names = FALSE)
  #cat(paste("\n--- Future Forecast Saved to:", forecast_filename, "\n"))
  
  return(final_forecast_df)
}

# ==============================================================================
# 6. FUTURE FORECAST PLOTTING FUNCTION (CORRECTED TO RENDER DIRECTLY)
# ==============================================================================
plot_future_forecast <- function(forecast_df, plot_store_id = NULL) {
  cat("\n--- 6.1 Generating Future Forecast Plot ---\n")
  
  plot_data <- forecast_df
  
  # 1. Store ID Filtering/Aggregation
  if (!is.null(plot_store_id)) {
    # Plot single store
    plot_data <- plot_data %>% filter(StoreId == as.numeric(plot_store_id))
    plot_title_suffix <- paste("Store ID:", plot_store_id)
    
  } else {
    # Plot overall aggregate sales
    plot_data <- plot_data %>%
      group_by(Primary_Time_Key) %>%
      summarize(predicted_sales = sum(predicted_sales, na.rm = TRUE), .groups = 'drop')
    plot_title_suffix <- "Overall Aggregate"
  }
  
  # 2. Create Plot
  if (nrow(plot_data) > 0) {
    # Ensure dates are in Date format for plotting
    plot_data <- plot_data %>% mutate(Primary_Time_Key = ymd(Primary_Time_Key))
    
    p_forecast <- ggplot(plot_data, aes(x = Primary_Time_Key, y = predicted_sales)) +
      geom_line(color = "red", linewidth = 1.2) +
      geom_point(color = "red", size = 3) +
      # Add text labels for predicted values
      geom_text(aes(label = round(predicted_sales, 0)), vjust = -1, size = 3.5, color = "red") + 
      labs(
        title = paste("Future Daily Sales Forecast (", plot_title_suffix, ")"),
        x = "Date",
        y = "Predicted Total Daily Sales"
      ) +
      scale_x_date(date_breaks = "1 day", date_labels = "%m/%d") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)
      )
    
    # *** CRITICAL CHANGE: Print the plot object directly ***
    print(p_forecast) 
    
    cat("--- Forecast plot rendered successfully. ---\n")
  } else {
    warning("Plot data is empty after filtering/aggregation. (0 rows found)")
  }
}

# --- B) TESTING/PLOTTING WORKFLOW (Run every time after the model is saved) ---

# ==============================================================================
# 4. WORKFLOW EXECUTION BLOCK
# ==============================================================================

# --- A) FULL TRAINING WORKFLOW (Run only the first time) ---
cat("\n*** RUNNING HYBRID ARIMA-XGBOOST WORKFLOW (TRAINING) ***\n")
data_list <- prep_data_for_xgb_daily(CONFIG)
xgb_model <- train_xgb_model_and_save(data_list, CONFIG)

# --- B) TESTING/PLOTTING WORKFLOW (Historical Backtest) ---
cat("\n--- HISTORICAL BACKTEST SETUP ---\n")
user_store_id <- readline(prompt="Enter Store ID to plot historical backtest (Leave blank for overall): ")
if (user_store_id == "") { user_store_id <- NULL } else { user_store_id <- as.numeric(user_store_id) }
cat("------------------------------\n")
test_analysis <- load_and_test_forecast_daily(
  config = CONFIG,
  plot_store_id = user_store_id
)
print(test_analysis$metrics)


# --- C) FUTURE FORECASTING WORKFLOW ---
cat("\n*** RUNNING FUTURE FORECAST WORKFLOW ***\n")
forecast_horizon <- readline(prompt="Enter number of future days to forecast (e.g., 7): ")
forecast_horizon <- as.numeric(forecast_horizon)

if (is.na(forecast_horizon) | forecast_horizon < 1) {
  cat("Invalid forecast horizon. Skipping future forecast.\n")
} else {
  # 1. Execute the future forecasting function
  future_results <- forecast_future_sales(
    config = CONFIG,
    forecast_days = forecast_horizon
  )
  
  cat("\n--- First 5 rows of Future Forecast ---\n")
  print(head(future_results, 5))
  
  # 2. Plotting the forecast
  plot_store_id_forecast <- readline(prompt="Enter Store ID to plot the FUTURE FORECAST (Leave blank for overall aggregate): ")
  if (plot_store_id_forecast == "") { plot_store_id_forecast <- NULL } else { plot_store_id_forecast <- as.numeric(plot_store_id_forecast) }
  
  plot_future_forecast(
    forecast_df = future_results,
    plot_store_id = plot_store_id_forecast
  )
}
