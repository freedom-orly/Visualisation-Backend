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
model <- train_xgb_model_and_save(data_list, CONFIG)

# Run the check
accuracy_results <- check_model_accuracy(CONFIG)

# Run the analysis
top_mistakes <- analyze_top_mistakes(CONFIG)