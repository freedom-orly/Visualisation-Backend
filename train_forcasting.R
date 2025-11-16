library(dplyr)       
library(lubridate)   
library(caret)       # For model training 
library(ranger)      # For the fast Random Forest model
library(zoo)         # For rolling averages (rollmean)
library(tidyr)       


set.seed(123)       

sales_df <- read.csv("sales_location_hourly.csv")
visitor_df <- read.csv("total_hourly_visitors.csv")
weather_df <- read.csv2("weather_data_hourly.csv")
calendar_df <- read.csv2("is_holiday.csv")


prepare_forecasting_data <- function(sales_df, visitor_df, weather_df, calendar_df) {
  
  print("Preparing and merging data...")
  
  # Convert all timestamps to simple dates for daily aggregation
  sales_df$Date <- as.Date(sales_df$Date)
  visitor_df$Date <- as.Date(visitor_df$Date)
  weather_df$Date <- as.Date(weather_df$Date)
  calendar_df$Date <- as.Date(calendar_df$Date)

  #Aggregate Sales
  sales_daily <- sales_df %>%
    group_by(Date, locationid) %>% 
    summarise(total_sales = sum(total), .groups = "drop") 
  
  #Aggregate Visitors
  visitors_daily <- visitor_df %>%
    group_by(Date) %>%
    summarise(total_visitors = sum(total_visitors), .groups = "drop") 
  
  #Aggregate Weather
  weather_daily <- weather_df %>%
    group_by(Date) %>%
    summarise(
      avg_temp = mean(Temperature),
      total_precip = sum(Precipitation),
      .groups = "drop"
    )
  
  # --- Create master grid (based on sales locations) ---
  all_locations <- unique(sales_df$locationid)
  all_dates <- seq(min(sales_daily$Date), max(sales_daily$Date), by = "day")
  
  date_location_grid <- expand.grid(Date = all_dates, locationid = all_locations) 
  
  
  # --- Join all data ---
  print("Joining all data sources...")
  daily_data <- date_location_grid %>%
    # Join sales by date and location
    left_join(sales_daily, by = c("Date", "locationid")) %>%
    # Join visitors by date ONLY
    left_join(visitors_daily, by = "Date") %>% # <-- CHANGED
    # Join weather by date ONLY
    left_join(weather_daily, by = "Date") %>% # <-- CHANGED
    # Join calendar by date ONLY
    left_join(calendar_df, by = "Date") 
  
  # --- Clean NAs ---
  daily_data <- daily_data %>%
    mutate(
      total_sales = ifelse(is.na(total_sales), 0, total_sales),
      total_visitors = ifelse(is.na(total_visitors), 0, total_visitors),
      is_holiday = ifelse(is.na(is_holiday), FALSE, is_holiday) 
    ) %>%
    group_by(locationid) %>% 
    mutate(
      avg_temp = na.locf(avg_temp, na.rm = FALSE),
      total_precip = ifelse(is.na(total_precip), 0, total_precip)
    ) %>%
    ungroup() %>%
    mutate(
      avg_temp = na.locf(avg_temp, fromLast = TRUE) 
    )
  
  #View(daily_data)
  print("Data preparation complete.")
  return(daily_data)
}


# --- FEATURE ENGINEERING FUNCTION ---
#This function creates lag and rolling features.

engineer_features <- function(daily_data) {
  
  print("Engineering time-series features...")
  
  feature_df <- daily_data %>%
    #Group by location so lags don't "jump" across locations
    group_by(locationid) %>%
    arrange(Date) %>%
    mutate(
      # --- Lag Features (Past Sales) ---
      sales_lag_7 = lag(total_sales, 7),
      sales_lag_14 = lag(total_sales, 14),
      sales_lag_28 = lag(total_sales, 28),
      
      # --- Rolling Features (Past Sales) ---
      # We must lag 'total_sales' by 1 day before calculating the
      # rolling mean. This prevents using 'NA' from the current day.
      sales_lag_1 = lag(total_sales, 1),
      sales_roll_avg_7 = rollmean(sales_lag_1, k = 7, fill = NA, align = "right"),
      sales_roll_avg_28 = rollmean(sales_lag_1, k = 28, fill = NA, align = "right"),
      
      # --- Calendar Features ---
      day_of_week = as.factor(wday(Date, label = TRUE)),
      month = as.factor(month(Date, label = TRUE)),
      week_of_year = as.integer(week(Date)),
      year = year(Date)
    ) %>%
    ungroup() %>%
    # Drop rows where we couldn't create features (the first ~month of data)
    tidyr::drop_na(sales_lag_7, sales_lag_14, sales_lag_28, 
                   sales_roll_avg_7, sales_roll_avg_28, sales_lag_1) %>%
    # Convert character/logical to factors for Random Forest
    mutate(
      locationid = as.factor(locationid),
      is_holiday = as.factor(is_holiday)
    ) %>%
    # We can drop sales_lag_1 now, it was just a helper
    select(-sales_lag_1)
  
  print("Feature engineering complete.")
  return(feature_df)
}


# --- MODEL TRAINING FUNCTION ---
train_forecasting_model <- function(feature_df) {
  
  print("Starting model training...")
  
  # ---
  # Create Group-Aware Time-Slices
  # ---
  # This creates time-slices for *each location* independently.
  
  all_locations <- unique(feature_df$locationid)
  custom_indices <- list()
  custom_indices_out <- list()
  
  # We'll slide our window every 30 days to speed up training
  slice_skip <- 29 # (skip = 29 means it jumps 30 days)
  
  for (loc in all_locations) {
    # Get the row numbers for this location
    location_indices <- which(feature_df$locationid == loc)
    
    # Check if there's enough data for at least one slice
    if(length(location_indices) < 365 + 90) {
      print(paste("Warning: Not enough data for location", loc, "to create time-slices. Skipping."))
      next
    }
    
    # Create time slices just for these rows
    slices <- createTimeSlices(
      location_indices,
      initialWindow = 365,
      horizon = 90,
      fixedWindow = FALSE,
      skip = slice_skip 
    )
    
    # Add the training and test indices to our master lists
    custom_indices <- c(custom_indices, slices$train)
    custom_indices_out <- c(custom_indices_out, slices$test)
  }
  
  if(length(custom_indices) == 0) {
    print("Error: Not enough data to create any training slices. Stopping.")
    return(NULL)
  }
  
  print(paste("Created", length(custom_indices), "group-aware time-slices for cross-validation."))
  
  #Define the cross-validation
  my_control <- trainControl(
    method = "cv",
    index = custom_indices,       #Training folds
    indexOut = custom_indices_out, #Testing folds
    allowParallel = TRUE,
    verboseIter = TRUE
  )
  
  #Define the hyperparameter grid for ranger
  my_grid <- expand.grid(
    mtry = c(5, 8, 12),
    splitrule = "variance", 
    min.node.size = c(5, 10)
  )
  
  # Train the model
  # We predict 'total_sales' using all other columns.
  # We MUST remove the 'date' column as it's not a predictor.
  model <- train(
    total_sales ~ .,
    data = feature_df %>% select(-Date),
    method = "ranger",
    trControl = my_control,
    tuneGrid = my_grid,
    importance = 'permutation' 
  )
  
  print("Model training complete!")
  print(model)
  print(varImp(model))
  
  return(model)
}

# ----Training plus diagnostics of the model -----
historical_daily_data <- prepare_forecasting_data(
  sales_df, visitor_df, weather_df, calendar_df)

historical_features_df <- engineer_features(historical_daily_data)

split_date <- max(historical_features_df$Date) - months(1)

train_data <- historical_features_df %>% filter(Date <= split_date)
test_data <- historical_features_df %>% filter(Date > split_date)

print(paste("Total rows:", nrow(historical_features_df)))
print(paste("Training rows:", nrow(train_data)))
print(paste("Test rows:", nrow(test_data)))

model <- train_forecasting_model(train_data)

if (!is.null(model)) {
  print("Making predictions on test set...")
  
  predictions <- predict(model, newdata = test_data)
  
  # Combine predictions with actuals
  test_results <- test_data %>%
    select(Date, locationid, total_sales) %>%
    mutate(predicted_sales = predictions)
  
  # ---Evaluate the model ---
  # We'll use caret's postResample to get RMSE and R-squared
  metrics <- postResample(pred = test_results$predicted_sales, 
                          obs = test_results$total_sales)
  
  print("--- MODEL PERFORMANCE ON TEST SET ---")
  print(metrics)
  
  print("--- PREDICTIONS ---")
  print(head(test_results))
  
  # --- Plot the forecast ---
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    library(ggplot2)
    
    # Reshape data for plotting
    plot_data <- test_results %>%
      tidyr::gather(key = "type", value = "sales", total_sales, predicted_sales)
    
    p <- ggplot(plot_data, aes(x = Date, y = sales, color = type)) +
      geom_line(linewidth = 1) +
      geom_point() +
      facet_wrap(~locationid, scales = "free_y") +
      theme_minimal() +
      labs(title = "Model Predictions vs. Actual Sales (Test Set)",
           x = "Date", y = "Total Sales")
    
    print(p)
  }
} else {
  print("Model training failed. Cannot proceed with prediction.")
}

saveRDS(model, "revenue_forcast_v1.rds")
