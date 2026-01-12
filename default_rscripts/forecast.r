# ==============================================================================
# LIBRARIES
# ==============================================================================
# library(tidyverse)
# library(lubridate)
# library(xgboost)
# library(ggplot2)
# library(parallel)
# library(forecast)
# library(jsonlite)
# library(httr)

dir.create(Sys.getenv("R_LIBS_USER"), recursive = TRUE)  # create personal library
.libPaths(Sys.getenv("R_LIBS_USER"))

args <- commandArgs(trailingOnly = TRUE)

vis_id <- args[1]

data_dir <- file.path(getwd(),"instance", "store",vis_id,"data")
source(file.path(getwd(),"instance", "store",vis_id,"rscripts", "forecast_prep.R"))


parameters_json_str <- args[2]
parameters <- fromJSON(parameters_json_str)
stores <- c(as.integer(parameters$stores))
horizon_days <- as.integer(parameters$horizon_days)


packages <- c(
  "tidyverse", "parallel", "ggplot2",
   "lubridate", "jsonlite", "xgboost",
  "forecast", "httr"
)

# Function to install (if missing) and load each package
install_and_load <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    message(paste("📦 Installing missing package:", pkg))
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  } else {
    message(paste("✅ Package already loaded:", pkg))
  }
}

# Loop through each package
for (p in packages) install_and_load(p)

# xgboots specific version

#https://github.com/dmlc/xgboost/releases/download/v1.7.6/xgboost.tar.gz
# packageurl <- "https://cloud.r-project.org/src/contrib/Archive/xgboost/xgboost_1.7.11.1.tar.gz"
# install.packages(packageurl, repos=NULL, type="source", INSTALL_opts = '--no-lock')

# remotes::install_version(
#   "xgboost",
#   version = "3.0.2",
#   repos = "https://cloud.r-project.org"
# )

# install.packages(
#   "https://cran.r-project.org/bin/windows/contrib/4.3/xgboost_1.7.11.zip",    
#   repos = NULL
# )

data_preperation(vis_id)

CONFIG <- list(
  SALES_FILE = file.path(data_dir, "sales_location_hourly.csv"),
  VISITOR_FILE = file.path(data_dir, "total_hourly_visitors.csv"),
  WEATHER_FILE = file.path(data_dir, "weather_data_hourly.csv"), # Historical data
  CALENDAR_FILE = file.path(data_dir, "is_holiday.csv"),
  FORECAST_WEATHER_FILE = file.path(data_dir, "emmen_live_forecast.csv"), # New Live File
  MODEL_FILE = file.path(data_dir, "xgb_residual_forecaster.rds"),
  PREP_DATA_FILE = file.path(data_dir, "prep_hybrid_data_list.rds"),
  TEST_WINDOW_DAYS = 30
)



fetch_emmen_forecast <- function(output_file = CONFIG$FORECAST_WEATHER_FILE) {
 # cat("--- Fetching Live 7-Day Forecast for Emmen, NL ---\n")

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
  #cat(paste("--- Live weather saved to:", output_file, "\n"))

  return(forecast_daily)
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
# EXECUTION
# ==============================================================================



# 2. Forecast with Live Weather
future_results <- forecast_future_sales(CONFIG, forecast_days = as.integer(horizon_days))

json_list <- list()

for (s in unique(future_results$StoreId)) {


    if(!(s %in% stores)) {
        next
    }

  tmp <- future_results[future_results$StoreId == s, ]

  p <- as.Date(tmp$Primary_Time_Key)
  asd <- format(p, "%d-%m-%Y")

  json_list <- append(
    json_list,
    list(
      list(
        name = as.character(s),
        values = map2(
          asd,
          tmp$predicted_sales,
          ~ list(
            x = as.character(.x),
            y = .y
          )
        )
      )
    )
  )
}

json_output <- toJSON(json_list, auto_unbox = TRUE, pretty = TRUE)

cat(json_output)

