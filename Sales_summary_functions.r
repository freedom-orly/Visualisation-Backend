sales <- read.csv2("sales_sample_head.csv")
stores <- read.csv2("store.csv", )
#View(sales)

#Puts the total_cost colum as numeric instead of characters
sales$Total_cost <- as.numeric(sales$Total_cost)

#Gets the total costs per store from the whole csv file
total_cost_per_store <- function(){
  store_summary <- aggregate(Total_cost ~ Store_id, data = sales, sum, na.rm = TRUE)
  
  #View(store_summary)
  return(store_summary)
}

#Gets the total costs per store per day
#Returns Store id, day of transactions and total transaction amount of store
total_cost_per_day <- function(){
  # Convert to POSIXct (date-time format)
  sales$Date <- as.POSIXct(sales$Date, format = "%Y-%m-%d %H:%M:%OS")
  #Gets the date
  sales$Date <- as.Date(sales$Date)
  #Sums the Total_cost by Store_id and Date
  daily_store_summary <- aggregate(Total_cost ~ Store_id + Date, data = sales, sum, na.rm = TRUE)
  
  #View(daily_store_summary)
  return(daily_store_summary)
}

#gets the total costs per store by hour
#(returns data.frame: Store id, Day, Hour, total_cost)
total_cost_per_hour <- function(){
  # Convert to POSIXct (date-time format)
  sales$Date <- as.POSIXct(sales$Date, format = "%Y-%m-%d %H:%M:%OS")
  #Gets the day
  sales$Day <- as.Date(sales$Date)
  #Gets the hour
  sales$Hour <- as.integer(format(sales$Date, "%H"))
  
  hour_summary <- aggregate(Total_cost ~ Store_id + Day + Hour, data = sales, sum, na.rm = TRUE)
  #View(hour_summary)
  return(hour_summary)
}

