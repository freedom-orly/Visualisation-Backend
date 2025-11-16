library(dplyr)
library(lubridate)
library(ranger)
library(zoo)
library(tidyr)
library(ranger)
#OPEN_Mateo
library(httr)      
library(jsonlite)  

#-----HOLDS ALL THE FUNCTIONS NEEDED TO RUN THE FORECASTING----

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
    left_join(visitors_daily, by = "Date") %>% 
    # Join weather by date ONLY
    left_join(weather_daily, by = "Date") %>% 
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
  
  print("Data preparation complete.")
  return(daily_data)
}


# --- FEATURE ENGINEERING FUNCTION ---
#This function creates lag and rolling features.

engineer_features <- function(daily_data) {
  
  print("Engineering time-series features...")
  
  feature_df <- daily_data %>%
    #Group by location so lags don't jump across locations
    group_by(locationid) %>%
    arrange(Date) %>%
    mutate(
      #Where total_sales is NA, it will use 0
      sales_for_lags = ifelse(is.na(total_sales), 0, total_sales),
      # --- Lag Features (Past Sales) ---
      sales_lag_7 = lag(sales_for_lags, 7),
      sales_lag_14 = lag(sales_for_lags, 14),
      sales_lag_28 = lag(sales_for_lags, 28),
      
      # --- Rolling Features (Past Sales) ---
      # We must lag 'sales_for_lags' by 1 day before calculating the
      # rolling mean. This prevents using 'NA' from the current day.
      sales_lag_1 = lag(sales_for_lags, 1),
      sales_roll_avg_7 = rollmean(sales_lag_1, k = 7, fill = NA, align = "right"),
      sales_roll_avg_28 = rollmean(sales_lag_1, k = 28, fill = NA, align = "right"),
      
      # --- Calendar Features ---
      day_of_week = as.factor(wday(Date, label = TRUE)),
      month = as.factor(month(Date, label = TRUE)),
      week_of_year = as.integer(week(Date)),
      year = year(Date)
    ) %>%
    ungroup() %>%
    mutate(
      sales_lag_7 = ifelse(is.na(sales_lag_7), 0, sales_lag_7),
      sales_lag_14 = ifelse(is.na(sales_lag_14), 0, sales_lag_14),
      sales_lag_28 = ifelse(is.na(sales_lag_28), 0, sales_lag_28),
      sales_roll_avg_7 = ifelse(is.na(sales_roll_avg_7), 0, sales_roll_avg_7),
      sales_roll_avg_28 = ifelse(is.na(sales_roll_avg_28), 0, sales_roll_avg_28),
      sales_lag_1 = ifelse(is.na(sales_lag_1), 0, sales_lag_1)
    ) %>%
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


aggregate_forecast <- function(forecast_df) {
  
  print("Aggregating forecast into daily and weekly views...")
  
  # --- Daily View ---
  daily_summary_df <- forecast_df %>%
    dplyr::group_by(locationid, Date) %>%
    dplyr::summarize(
      daily_predicted_sales = sum(predicted_sales, na.rm = TRUE)
    ) %>%
    dplyr::arrange(locationid, Date)
  
  # --- Weekly View ---
  weekly_summary_df <- forecast_df %>%
    dplyr::group_by(
      locationid,
      year = lubridate::year(Date),
      week = lubridate::week(Date)
    ) %>%
    dplyr::summarize(
      weekly_predicted_sales = sum(predicted_sales, na.rm = TRUE)
    ) %>%
    dplyr::arrange(locationid, year, week)
  
  print("Aggregation complete.")
  
  return(list(
    daily_summary = daily_summary_df,
    weekly_summary = weekly_summary_df
  ))
}


#LIVE WEATHER FORECAST
get_weather_forcast <- function(start_date, end_date){
  #Location: Emmen Wildlands 
  latitude <- 52.78250998299688
  longitude <- 6.891245551559943
  
  # The API needs dates in YYYY-MM-DD format
  start_date_for <- format(start_date, "%Y-%m-%d")
  end_date_for <- format(end_date, "%Y-%m-%d")
  
  api_url <- sprintf(
    "https://api.open-meteo.com/v1/forecast?latitude=%s&longitude=%s&daily=temperature_2m_mean,precipitation_sum&timezone=Europe/Berlin&start_date=%s&end_date=%s",
    latitude,
    longitude,
    start_date_for,
    end_date_for
  )
  
  tryCatch({
    response <- httr::GET(api_url)
    httr::stop_for_status(response, "call Open-Meteo API")
    json_content <- httr::content(response, "text")
    data <- jsonlite::fromJSON(json_content)
    formatted_data <- data.frame(
      Date = as.Date(data$daily$time),
      avg_temp = data$daily$temperature_2m_mean,
      total_precip = data$daily$precipitation_sum
    )
    print("Weather forecast received")
    return(formatted_data)
  }, error = function(e) {
    print(paste("Weather API failed. Error:", e$message))
  })
}


get_visitor_forcast <- function(file_data, start_date_str, end_date_str){
  start_date <- dmy(start_date_str, quiet = TRUE)
  end_date <- dmy(end_date_str, quiet = TRUE)
  
  # Check if parsing failed for either date.
  if (is.na(start_date)) {
    return(paste("Error: Invalid start date '", start_date_str, "'. Please use 'dd/mm/YYYY' format."))
  }
  if (is.na(end_date)) {
    return(paste("Error: Invalid end date '", end_date_str, "'. Please use 'dd/mm/YYYY' format."))
  }
  
  if (end_date < start_date) {
    return(paste("Error: End date", end_date_str, "is before start date", start_date_str))
  }
  
  result_data <- file_data %>%
    filter(!is.na(Datum)) %>%
    group_by(Datum) %>%
    summarise(
      daily_total = sum(Budget, na.rm = TRUE)
    ) %>%
    filter(
      Datum >= start_date & Datum <= end_date
    ) %>%
    pull(daily_total)
  
  # Check if the final data frame has any rows
  if (length(result_data) == 0) {
    return(paste("No data found for the period:",
                 format(start_date, "%d/%m/%Y"), "to", format(end_date, "%d/%m/%Y")))
  } else {
    return(result_data)
  }
}


#---FORCAST SPECIFIC DAYS OF SALES---

forcast_by_dates <- function(model, 
                                  historical_daily_data, 
                                  future_visitor_forecast, 
                                  all_locations,
                                  calendar_df,
                                  forecast_start_date,
                                  forecast_end_date) {
  
  # --- Define Dates ---
  forecast_dates <- seq(from = forecast_start_date, to = forecast_end_date, by = "day")
  
  if (length(future_visitor_forecast) != length(forecast_dates)) {
    stop("Error: 'future_visitor_forecast' not same length as forcast dates")
  }
  
  print("Starting forecast generation...")
  
  # --- Get Future Inputs ---

  #Get Weather Forecast
  weather_forecast_df <- get_weather_forcast(start_date = forecast_dates[1], 
                                             end_date = forecast_dates[length(forecast_dates)])
  
  #Format Visitor forcast
  visitor_forecast_df <- data.frame(
    Date = forecast_dates,
    total_visitors = future_visitor_forecast
  )
  
  # Check for Holidays
  calendar_df$Date <- as.Date(calendar_df$Date)
  future_holidays_df <- calendar_df %>% 
    filter(Date %in% forecast_dates)
  
  
  # --- Build Future Scaffold ---
  # This creates a "blank" data frame for the days,
  # with all the *driver* columns our model needs.
  
  print("Building future data scaffold...")
  future_scaffold <- expand.grid(Date = forecast_dates, 
                                 locationid = all_locations,
                                 stringsAsFactors = FALSE) %>%
    
    # Join the weather forecast (by Date)
    left_join(weather_forecast_df, by = "Date") %>%
    
    # Join the visitor forecast (by Date)
    left_join(visitor_forecast_df, by = "Date") %>%
    
    # Join the holiday info (by Date)
    left_join(future_holidays_df, by = "Date") %>%
    
    # Add 'total_sales' as NA (This is what we want to predict!)
    mutate(total_sales = NA) %>%
    
    # Clean up NAs from the holiday join
    mutate(is_holiday = ifelse(is.na(is_holiday), FALSE, is_holiday)) %>%
    
    # Select *only* the columns from 'prepare_forecasting_data' function
    select(
      Date, locationid, total_sales, total_visitors, 
      avg_temp, total_precip, is_holiday
    )
  
  
  # --- Get Historical Context ---
  # We need the last ~60 days to build lags and rolling averages
  print("Getting recent historical data for lags...")
  recent_data <- historical_daily_data %>%
    filter(Date > (forecast_start_date - days(60))) %>%
    select(
      Date, locationid, total_sales, total_visitors, 
      avg_temp, total_precip, is_holiday
    )
  
  
  # --- 5. Combine, Engineer, & Predict ---
  
  # "Stack" the recent history and the future scaffold
  combined_data <- bind_rows(recent_data, future_scaffold)
  
  # Run this *combined* data frame through the
  # 'engineer_features' function.
  print("Engineering features for combined data...")
  features_for_prediction <- engineer_features(combined_data)
  
  # Filter only for the timespan provided
  future_rows_to_predict <- features_for_prediction %>%
    filter(Date >= forecast_start_date)
  
  # --- MAKE PREDICTION ---
  print(paste("Making predictions on", nrow(future_rows_to_predict), "future rows..."))
  future_predictions <- predict(model, newdata = future_rows_to_predict)
  
  # --- Format Output ---
  final_forecast <- data.frame(
    Date = future_rows_to_predict$Date,
    locationid = future_rows_to_predict$locationid,
    predicted_sales = future_predictions
  )
  
  print("Forecast complete.")
  return(final_forecast)
}

