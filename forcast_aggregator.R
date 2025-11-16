library(dplyr)
library(lubridate)
library(ranger)
library(zoo)
library(tidyr)
library(readxl)

source("helper_forecast.R")

load_model_and_data_files <- function(){
  print("Loading model and raw data files...")
  
  model <- readRDS("revenue_forcast_v1.rds")
  
  sales_df <- read.csv("sales_location_hourly.csv")
  visitor_df <- read.csv("total_hourly_visitors.csv")
  weather_df <- read.csv2("weather_data_hourly.csv")
  calendar_df <- read.csv2("is_holiday.csv")
  expected_visitors_df <- read_excel("budget.xlsx")
  
  print("Assets loaded.")
  
  print("Preparing historical data...")
  historical_daily_data <- prepare_forecasting_data(
    sales_df, visitor_df, weather_df, calendar_df
  )
  
  all_locations <- unique(historical_daily_data$locationid)
  
  return(list(
    model = model,
    historical_daily_data = historical_daily_data,
    all_locations = all_locations,
    expected_visitors_df = expected_visitors_df,
    calendar_df = calendar_df
  ))
}


make_forcast <- function(assets, start_date_str, end_date_str){
  #Convert dates
  forecast_start_date <- dmy(start_date_str)
  forecast_end_date <- dmy(end_date_str)
  
  print("Defining future inputs...")
  
  forcasted_visitors <- get_visitor_forcast(
    assets$expected_visitors_df, 
    start_date_str, 
    end_date_str
    )
  
  historical_data_for_lags <- assets$historical_daily_data %>%
    filter(Date < forecast_start_date)
  
  print("--- STARTING FORECAST ---")
  
  final_forecast <- forcast_by_dates(
    model = assets$model,
    historical_daily_data = historical_data_for_lags,
    future_visitor_forecast = forcasted_visitors,
    all_locations = assets$all_locations,
    calendar_df = assets$calendar_df,
    forecast_start_date = forecast_start_date,
    forecast_end_date = forecast_end_date
  )
  
  print("--- FORECAST COMPLETE ---")
  return(final_forecast)

}

print_results <- function(final_forecast){
  # --- Aggregate the Results ---
  print("--- AGGREGATING FORECAST ---")
  
  # We can now pass this forecast to our aggregator
  aggregated_views <- aggregate_forecast(final_forecast)
  
  print("Daily Summary:")
  print(aggregated_views$daily_summary, n = Inf)
  
  print("Weekly Summary (Sample):")
  print(aggregated_views$weekly_summary, n = Inf)
  
  print("--- RAW FORECAST DATA ---")
  print(final_forecast)
}


#--- TO RUN FORCST---
#DATES TO FORCAST
start_date_str <- "01/11/2025"
end_date_str <- "09/11/2025"

#ONLY NEEDS TO BE CALLED ONCE
all_assets <- load_model_and_data_files()

#CALL THIS TO MAKE ANY FORCAST
final_forecast <- make_forcast(all_assets, start_date_str, end_date_str)

#print results
print_results(final_forecast)