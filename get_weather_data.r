library(readxl)

weather_data <- read_xlsx(path = "weather.xlsx")

#Helper function for checking if date time is correct
is_valid_datetime <- function(x, fmt = "%Y-%m-%d %H:%M:%S") {
  if (x == "None") {
    return(FALSE)  # treat "None" as not a datetime
  }
  !is.na(as.POSIXct(x, format = fmt, tz = "UTC"))
}

#Gets the data for a specific time frame eg. "2025-05-19 12:00:00", "2025-05-19 12:00:00", 
#if none are proivided then it just returns all the data
get_weather_data <- function(first_date = "None", second_date = "None"){
  
  weather_data$Time <- as.POSIXct(weather_data$Time, format = "%Y-%m-%d %H:%M:%S")
  
  #checks if date/time filter was included then if so filter the data
  if(is_valid_datetime(first_date) & is_valid_datetime(second_date)){
    filtered_weather <- subset(weather_data, Time >= first_date & Time <= second_date)
    cat("Filtered data")
    return(filtered_weather)
  }
  cat("No filter")
  return(weather_data)
}

View(get_weather_data("2024-01-01 12:00:00", "2024-01-04 12:00:00"))