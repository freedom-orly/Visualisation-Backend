dir.create(Sys.getenv("R_LIBS_USER"), recursive = TRUE)  # create personal library
.libPaths(Sys.getenv("R_LIBS_USER"))

args <- commandArgs(trailingOnly = TRUE)

packages <- c(
  "dplyr",
  "lubridate",
  "ranger",
  "zoo",
  "tidyr",
  "readxl",
  "jsonlite"
)

install_and_load <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    #message(paste("📦 Installing missing package:", pkg))
    install.packages(pkg, dependencies = TRUE, repos='http://cran.us.r-project.org')
    library(pkg, character.only = TRUE)
  } else {
    #message(paste("✅ Package already loaded:", pkg))
  }
}

for (p in packages) {
  install_and_load(p)
}

vis_id <- args[1]
source(file.path(getwd(),"instance", "store",vis_id,"rscripts", "helper_forecast.R"))


load_model_and_data_files <- function(){

  data_dir <- file.path(getwd(),"instance", "store",vis_id,"data")
  #print(getwd())
  #print("Loading model and raw data files...")
  
  model <- readRDS(file.path(data_dir, "revenue_forcast_v1.rds"))
  
  sales_df <- read.csv(file.path(data_dir, "sales_location_hourly.csv"))
  visitor_df <- read.csv(file.path(data_dir, "total_hourly_visitors.csv"))
  weather_df <- read.csv2(file.path(data_dir, "weather_data_hourly.csv"))
  calendar_df <- read.csv2(file.path(data_dir, "is_holiday.csv"))
  expected_visitors_df <- read_excel(file.path(data_dir, "budget.xlsx"))
  
  #print("Assets loaded.")
  
  #print("Preparing historical data...")
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
  
  forecast_start_date <- dmy(start_date_str)
  forecast_end_date <- dmy(end_date_str)
  
  #print("Defining future inputs...")
  
  forcasted_visitors <- get_visitor_forcast(
    assets$expected_visitors_df, 
    start_date_str, 
    end_date_str
    )
  
  historical_data_for_lags <- assets$historical_daily_data %>%
    filter(Date < forecast_start_date)
  
  #print("--- STARTING FORECAST ---")
  
  final_forecast <- forcast_by_dates(
    model = assets$model,
    historical_daily_data = historical_data_for_lags,
    future_visitor_forecast = forcasted_visitors,
    all_locations = assets$all_locations,
    calendar_df = assets$calendar_df,
    forecast_start_date = forecast_start_date,
    forecast_end_date = forecast_end_date
  )
  
  #print("--- FORECAST COMPLETE ---")
  return(final_forecast)

}

print_results <- function(final_forecast){
  # --- Aggregate the Results ---
  #print("--- AGGREGATING FORECAST ---")
  
  # We can now pass this forecast to our aggregator
  aggregated_views <- aggregate_forecast(final_forecast)
  
  #print("Daily Summary:")
  #print(aggregated_views$daily_summary, n = Inf)
  
  #print("Weekly Summary (Sample):")
  #print(aggregated_views$weekly_summary, n = Inf)
  
  #print("--- RAW FORECAST DATA ---")
  #print(final_forecast)
}


#--- TO RUN FORCST---
#DATES TO FORCAST
start_date_str <- args[2]
end_date_str <- args[3]

#ONLY NEEDS TO BE CALLED ONCE
all_assets <- load_model_and_data_files()

#CALL THIS TO MAKE ANY FORCAST
final_forecast <- make_forcast(all_assets, start_date_str, end_date_str)

#transform to correct json format 

formated <- final_forecast %>%
  group_by(locationid) %>%
  summarise(
    name = paste0("Location_", unique(locationid)),
    values = list(
      # create a data frame for each location
      data.frame(
        x = Date,
        y = predicted_sales
      )
    )
  ) %>%
  select(name, values)


#print results
 cat(toJSON(formated, pretty = TRUE))
