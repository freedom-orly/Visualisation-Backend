sales <- read.csv2("sales_sample2.csv")
stores <- read.csv2("store.csv", )

#Puts the total_cost colum as numeric instead of characters
sales$NetAmountExcl <- as.numeric(sales$NetAmountExcl)

#Gets the total costs per store from the whole csv file
total_cost_per_store <- function(){
  store_summary <- aggregate(sales$NetAmountExcl ~ StoreId, data = sales, sum, na.rm = TRUE)
  
  #Add heading
  store_summary <- merge(store_summary, stores, by = "StoreId", all.x = TRUE)
  
  #View(store_summary)
  return(store_summary)
}

#Gets the total costs per store per day
#Returns Store id, day of transactions and total transaction amount of store
total_cost_per_day <- function(){
  
  # Convert to POSIXct (date-time format)
  sales$ReceiptDateTime <- as.POSIXct(sales$ReceiptDateTime, format = "%Y-%m-%d %H:%M:%S")

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
total_cost_per_hour <- function(){
  # Convert to POSIXct (date-time format)
  sales$ReceiptDateTime <- as.POSIXct(sales$ReceiptDateTime, format = "%Y-%m-%d %H:%M:%S")
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
