dir.create(Sys.getenv("R_LIBS_USER"), recursive = TRUE)  # create personal library
.libPaths(Sys.getenv("R_LIBS_USER"))

args <- commandArgs(trailingOnly = TRUE)

packages <- c(
  "readr",
  "tidyverse",
  "lubridate",
  "jsonlite",
  "readxl"
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

parameters_json_str <- args[2]
parameters <- fromJSON(parameters_json_str)
stores <- c(as.integer(parameters$stores))

vis_id <- args[1]

data_dir <- file.path(getwd(),"instance", "store",vis_id,"data")

sales_data <- read_csv2(file.path(data_dir,"sales.csv"))
weather_data <- read_xlsx(file.path(data_dir,"weather.xlsx"))

  #Clean Data
  sales_cleaned <- sales_data %>%
    mutate(
      ReceiptDateTime = as.POSIXct(ReceiptDateTime),
      Date = floor_date(ReceiptDateTime, "day")
    ) %>%
    filter(NetAmountExcl >= 0.1)
  
  #Define Time Window
  latest_date <- max(sales_cleaned$Date, na.rm = TRUE)
  start_date <- latest_date - days(30)

latest_sales <- function() {
  
  
  #Calculate Daily Totals 
  daily_sales <- sales_cleaned %>%
    filter(Date >= start_date) %>%
    filter(StoreId %in% stores) %>%
    group_by(StoreId, Date) %>%
    summarise(DailyTotal = sum(NetAmountExcl), .groups = "drop")
  
  formatted_data <- daily_sales %>%
    complete(StoreId, Date = seq(start_date, latest_date, by = "day"), fill = list(DailyTotal = 0)) %>%
    
    mutate(Date = format(Date, "%d-%m-%Y"), name = as.character(StoreId)) %>%
    
    select(name, x = Date, y = DailyTotal) %>%
    
    group_by(name) %>%
    nest(values = c(x, y)) %>%
    ungroup()
  
  return(formatted_data)
}


get_weather_data <- function(){
  
  weather_cleaned <- weather_data %>%
    mutate(
      Time = as.POSIXct(Time),
      Date = floor_date(Time, "day")
    )
  
  
  filtered_data <- weather_cleaned %>%
    filter(Date >= start_date) %>%
    group_by(Date) %>%
    summarise(
      Temperature = mean(Temperature, na.rm = TRUE),   # Avg Temp for the day
      Precipitation = sum(Precipitation, na.rm = TRUE) # Total Rain for the day
    ) %>%
    
    pivot_longer(
      cols = c(Temperature, Precipitation),
      names_to = "name",
      values_to = "y"
    ) %>%
    
    mutate(x = format(Date, "%d-%m-%Y")) %>% # Format date as string
    select(name, x, y) %>%
    
    group_by(name) %>%
    nest(values = c(x, y)) %>%
    ungroup()
  
  return(filtered_data) 
  
}

#Get results
sales_result <- latest_sales()
weather_result <- get_weather_data()

#Combine them into one big list 
final_combined_data <- bind_rows(sales_result, weather_result)

#Output
cat(toJSON(final_combined_data, pretty = TRUE, auto_unbox = TRUE))