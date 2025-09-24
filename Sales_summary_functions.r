sales <- read.csv2("sales_sample2.csv")
stores <- read.csv2("store.csv", )

#Puts the total_cost colum as numeric instead of characters
sales$NetAmountExcl <- as.numeric(sales$NetAmountExcl)

#Helper function for checking if date time is correct
is_valid_datetime <- function(x, fmt = "%Y-%m-%d %H:%M:%S") {
  if (x == "None") {
    return(FALSE)  # treat "None" as not a datetime
  }
  !is.na(as.POSIXct(x, format = fmt, tz = "UTC"))
}


#Gets the total costs per store from the whole csv file
total_cost_per_store <- function(first_date = "None", second_date = "None"){
  #checks if date/time filter was included then if so filter the data
  if(is_valid_datetime(first_date) & is_valid_datetime(second_date)){
    sales <- subset(sales, ReceiptDateTime >= first_date & ReceiptDateTime <= second_date)
  }
  
  store_summary <- aggregate(sales$NetAmountExcl ~ StoreId, data = sales, sum, na.rm = TRUE)
  
  #Add heading
  store_summary <- merge(store_summary, stores, by = "StoreId", all.x = TRUE)
  
  #View(store_summary)
  return(store_summary)
}


#Gets the total costs per store per day
#Returns Store id, day of transactions and total transaction amount of store
#Gets the data for a specific time frame eg. "2025-05-19", "2025-05-19"
total_cost_per_day <- function(first_date = "None", second_date = "None"){
  
  # Convert to POSIXct (date-time format)
  sales$ReceiptDateTime <- as.POSIXct(sales$ReceiptDateTime, format = "%Y-%m-%d %H:%M:%S")

  #checks if date/time filter was included then if so filter the data
  if(is_valid_datetime(first_date) & is_valid_datetime(second_date)){
    sales <- subset(sales, ReceiptDateTime >= first_date & ReceiptDateTime <= second_date)
  }
  
  #Gets the date
  sales$ReceiptDateTime <- as.Date(sales$ReceiptDateTime)
  #Sums the Total_cost by Store_id and Date
  daily_store_summary <- aggregate(sales$NetAmountExcl ~ StoreId + ReceiptDateTime, data = sales, sum, na.rm = TRUE)
  
  #Add heading
  daily_store_summary <- merge(daily_store_summary, stores, by = "StoreId", all.x = TRUE)
  
  #View(daily_store_summary)
  return(daily_store_summary)
}


#gets the total costs per store by hour
#(returns data.frame: Store id, Day, Hour, total_cost)
total_cost_per_hour <- function(first_date = "None", second_date = "None"){
  # Convert to POSIXct (date-time format)
  sales$ReceiptDateTime <- as.POSIXct(sales$ReceiptDateTime, format = "%Y-%m-%d %H:%M:%S")
  
  #checks if date/time filter was included then if so filter the data
  if(is_valid_datetime(first_date) & is_valid_datetime(second_date)){
    sales <- subset(sales, ReceiptDateTime >= first_date & ReceiptDateTime <= second_date)
  }
  
  #Gets the day
  sales$Day <- as.Date(sales$ReceiptDateTime)
  #Gets the hour
  sales$Hour <- as.integer(format(sales$ReceiptDateTime, "%H"))
  
  hour_summary <- aggregate(sales$NetAmountExcl ~ StoreId + Day + Hour, data = sales, sum, na.rm = TRUE)
  
  #Add heading
  hour_summary <- merge(hour_summary, stores, by = "StoreId", all.x = TRUE)
  
  View(hour_summary)
  return(hour_summary)
}

View(total_cost_per_hour("2025-01-04 21:00:00", "2025-01-05 21:00:00"))