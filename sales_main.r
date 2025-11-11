# ==========================================================
# 📦 1. Load Libraries
# ==========================================================
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(synthpop)
library(lubridate)
library(jsonlite)
library(caret)
library(forecast)
library(purrr)

# ==========================================================
# ⚙️ 2. Utility Functions
# ==========================================================

# List of required packages
packages <- c(
  "readxl", "dplyr", "tidyr", "ggplot2", "scales",
  "synthpop", "lubridate", "jsonlite", "caret",
  "forecast", "purrr"
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

message("\n🎉 All required libraries are installed and loaded successfully!")

# ----------------------------------------------------------
# Generate a time span
# ----------------------------------------------------------
generate_timespan <- function(start_date, end_date, spread) {
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  
  if (end_date <= start_date) stop("End date must be after start date.")
  if (spread < 2) stop("Spread must be at least 2.")
  
  seq(from = start_date, to = end_date, length.out = spread)
}

# ----------------------------------------------------------
# Read file(s) by ID
# ----------------------------------------------------------
read_file_by_id <- function(id_input, base_dir = "visualizations", file_type = "all") {
  folder_path <- file.path(base_dir, as.character(id_input))
  if (!dir.exists(folder_path)) stop("Folder does not exist: ", folder_path)
  
  files <- list.files(folder_path, full.names = TRUE)
  if (length(files) == 0) stop("No files found in folder: ", folder_path)
  
  if (file_type != "all") {
    files <- files[grepl(paste0("\\.", file_type, "$"), files)]
    if (length(files) == 0) stop("No files of type '", file_type, "' found in folder: ", folder_path)
  }
  
  read_single_file <- function(file) {
    ext <- tools::file_ext(file)
    if (ext %in% c("csv")) {
      read_csv(file)
    } else if (ext %in% c("txt")) {
      read_lines(file)
    } else if (ext %in% c("xls", "xlsx")) {
      read_excel(file)
    } else {
      message("Unknown file type, returning path only: ", file)
      return(file)
    }
  }
  
  if (length(files) == 1) return(read_single_file(files))
  setNames(lapply(files, read_single_file), names(files))
}

# ==========================================================
# 📊 3. Yearly Data Functions
# ==========================================================

get_year_data <- function(year_input,
                          visitor_data = visitor_hourly,
                          sales_data = sales_location_nona) {
  
  # --- 1️⃣ Visitors ---
  visitors_year <- visitor_data %>%
    mutate(Date = as.Date(Date)) %>%
    filter(year(Date) == year_input) %>%
    group_by(Date) %>%
    summarise(total_visitors = sum(NumberOfUsedEntrances, na.rm = TRUE)) %>%
    arrange(Date)
  
  # --- 2️⃣ Sales ---
  sales_year <- sales_data %>%
    mutate(Date = as.Date(Date)) %>%
    filter(year(Date) == year_input) %>%
    group_by(Date) %>%
    summarise(total_sales = sum(total, na.rm = TRUE)) %>%
    arrange(Date)
  
  # --- 3️⃣ Combine ---
  combined_year <- visitors_year %>%
    left_join(sales_year, by = "Date")
  
  # --- 4️⃣ Plot Visitors ---
  p1 <- ggplot(visitors_year, aes(x = Date, y = total_visitors)) +
    geom_line(color = "steelblue", linewidth = 0.8) +
    labs(
      title = paste("🧍‍♂️ Total Visitors per Day -", year_input),
      x = "Date", y = "Number of Visitors"
    ) +
    scale_y_continuous(labels = comma) +
    theme_minimal(base_size = 13)
  
  # --- 5️⃣ Plot Sales ---
  p2 <- ggplot(sales_year, aes(x = Date, y = total_sales)) +
    geom_line(color = "darkgreen", linewidth = 0.8) +
    labs(
      title = paste("💰 Total Sales per Day -", year_input),
      x = "Date", y = "Total Sales"
    ) +
    scale_y_continuous(labels = comma) +
    theme_minimal(base_size = 13)
  
  # --- 6️⃣ Output ---
  print(p1)
  print(p2)
  
  list(
    visitors_daily = visitors_year,
    sales_daily = sales_year,
    combined = combined_year,
    visitors_plot = p1,
    sales_plot = p2
  )
}

# ----------------------------------------------------------
# Heatmaps by Location
# ----------------------------------------------------------
heatmaps_by_location <- function(year_input, sales_data = sales_location_nona) {
  
  sales_year <- sales_data %>%
    mutate(
      Date = as.Date(Date),
      Month = month(Date, label = TRUE, abbr = TRUE),
      Hour = as.numeric(substr(Time, 1, 2))
    ) %>%
    filter(year(Date) == year_input, !is.na(locationid))
  
  sales_hourly <- sales_year %>%
    group_by(locationid, Month, Hour) %>%
    summarise(total_sales = sum(total, na.rm = TRUE), .groups = "drop") %>%
    mutate(Month = factor(Month, levels = month.abb))
  
  locations <- unique(sales_hourly$locationid)
  heatmap_list <- lapply(locations, function(loc) {
    data_loc <- sales_hourly %>% filter(locationid == loc)
    ggplot(data_loc, aes(x = Hour, y = Month, fill = total_sales)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "lightyellow", high = "darkred", name = "Sales") +
      scale_x_continuous(breaks = 0:23) +
      labs(
        title = paste("Heatmap of Sales by Hour - Location", loc, "-", year_input),
        x = "Hour of Day", y = "Month"
      ) +
      theme_minimal(base_size = 12)
  })
  
  names(heatmap_list) <- paste0("Location_", locations)
  
  list(aggregated_data = sales_hourly, heatmaps = heatmap_list)
}

# ----------------------------------------------------------
# Average Sales per Visitor
# ----------------------------------------------------------
avg_sales_per_visitor <- function(year_input, location_input,
                                  sales_data, visitors_data) {
  
  sales_daily <- sales_data %>%
    mutate(Date = as.Date(Date)) %>%
    filter(year(Date) == year_input, locationid == location_input) %>%
    group_by(Date, locationid) %>%
    summarise(total_sales = sum(total, na.rm = TRUE), .groups = "drop")
  
  visitors_daily <- visitors_data %>%
    mutate(Date = as.Date(Date)) %>%
    filter(year(Date) == year_input) %>%
    group_by(Date) %>%
    summarise(total_visitors = sum(NumberOfUsedEntrances, na.rm = TRUE), .groups = "drop")
  
  combined <- left_join(sales_daily, visitors_daily, by = "Date") %>%
    mutate(avg_sale_per_visitor = total_sales / total_visitors)
  
  avg_value <- mean(combined$avg_sale_per_visitor, na.rm = TRUE)
  
  p <- ggplot(combined, aes(x = Date, y = avg_sale_per_visitor)) +
    geom_line(color = "steelblue", linewidth = 0.8) +
    labs(
      title = paste("🧾 Avg Sale per Visitor — Location", location_input, "-", year_input),
      subtitle = paste("Average over year:", scales::dollar(round(avg_value, 2))),
      x = "Date", y = "Average Sale per Visitor (€)"
    ) +
    scale_y_continuous(labels = dollar) +
    theme_minimal(base_size = 13)
  
  list(data = combined, avg_value = avg_value, plot = p)
}

# ----------------------------------------------------------
# Conversion Rate (Single Location)
# ----------------------------------------------------------
conversion_rate_single_location <- function(
    sales_data, visitor_data, location_input, year_input = 2025
) {
  visitors_daily <- visitor_data %>%
    mutate(Date = as.Date(Date)) %>%
    filter(year(Date) == year_input) %>%
    group_by(Date) %>%
    summarise(total_visitors = sum(NumberOfUsedEntrances, na.rm = TRUE), .groups = "drop")
  
  sales_transactions <- sales_data %>%
    mutate(Date = as.Date(Date)) %>%
    filter(year(Date) == year_input, locationid == location_input) %>%
    group_by(Date, locationid) %>%
    summarise(num_transactions = n(), .groups = "drop")
  
  conversion_data <- left_join(sales_transactions, visitors_daily, by = "Date") %>%
    mutate(conversion_rate = num_transactions / total_visitors) %>%
    filter(!is.na(conversion_rate) & total_visitors > 0)
  
  p <- ggplot(conversion_data, aes(x = Date, y = conversion_rate)) +
    geom_line(color = "darkorchid", linewidth = 0.8) +
    labs(
      title = paste("📈 Conversion Rate per Day — Location", location_input, year_input),
      subtitle = "Conversion = Sales Transactions ÷ Total Visitors (per day)",
      x = "Date", y = "Conversion Rate"
    ) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    theme_minimal(base_size = 13)
  
  print(p)
  
  list(
    data = conversion_data,
    plot = p,
    avg_conversion = mean(conversion_data$conversion_rate, na.rm = TRUE)
  )
}

# Example run
result_conv_loc1 <- conversion_rate_single_location(
  sales_data = sales_alt,
  visitor_data = visitor_hourly,
  location_input = 1,
  year_input = 2025
)

head(result_conv_loc1$data)
print(result_conv_loc1$plot)
cat("Average conversion rate for location 1:",
    scales::percent(result_conv_loc1$avg_conversion, accuracy = 0.1), "\n")

#--------------------------------
#getting the sales data for either the location/store ID
#--------------------------------
get_sales_data <- function(store_id = NULL,
                           location_id = NULL,
                           start_date = NULL,
                           end_date = NULL,
                           spread = NULL,
                           data = sales_stores_loc) {
  # Validation
  if (is.null(store_id) && is.null(location_id)) {
    stop("Please provide either a store_id or a location_id.")
  }
  
  # Generate timespan if both dates and spread are given
  if (!is.null(start_date) && !is.null(end_date) && !is.null(spread)) {
    timespan <- generate_timespan(start_date, end_date, spread)
  } else if (!is.null(start_date) && !is.null(end_date)) {
    timespan <- seq(as.Date(start_date), as.Date(end_date), by = "day")
  } else {
    timespan <- NULL
  }
  
  # Filter data
  result <- data %>%
    filter(
      (is.null(store_id) | StoreId == store_id),
      (is.null(location_id) | locationid == location_id),
      (is.null(timespan) | Date %in% timespan)
    ) %>%
    select(Date, Time, total, StoreId, locationid) %>%
    arrange(Date, Time)
  
  # Output message if no results
  if (nrow(result) == 0) {
    message("⚠️ No records found for the given filters.")
  }
  
  return(result)
}

# Filter by a specific Store ID
users_store_sales <- get_sales_data(store_id = 145)

# Filter by a specific Location ID
users_location_sales <- get_sales_data(location_id = 3)

# View results
head(users_store_sales)


# ==========================================================
# 🧮 4. Cross-validation
# ==========================================================
ctrl <- trainControl(method = "cv", number = 10)

# ==========================================================
# 📥 5. Data Import
# ==========================================================
sales         <- read.csv("sales.csv", header = TRUE, sep = ";")
hours         <- read_excel("hours.xlsx")
locations     <- read_excel("linktables.xlsx", sheet = "locations")
links         <- read_excel("linktables.xlsx", sheet = "stores")
departments   <- read_excel("departments.xlsx", sheet = "Blad1")
holidays      <- read_excel("holidays.xlsx", sheet = "Blad1")
teams         <- read_excel("teams.xlsx", sheet = "Blad1")
store         <- read.csv("store.csv", header = TRUE, sep = ";")
subgroup      <- read.csv("subgroup.csv", header = TRUE, sep = ";")
maingroup     <- read.csv("maingroup.csv", header = TRUE, sep = ";")
visitor_hourly <- read.csv("visitorhourly.csv", header = TRUE, sep = ";")

# ----------------------------------------------------------
# Derived Data
# ----------------------------------------------------------
total_hourly_visitors <- visitor_hourly %>%
  group_by(Date, Time) %>%
  summarise(total_visitors = sum(NumberOfUsedEntrances, na.rm = TRUE), .groups = "drop") %>%
  arrange(Date, Time)

sales_alt <- sales %>%
  left_join(links %>% select(StoreId, locationid), by = "StoreId") %>%
  mutate(
    ReceiptDateTime = as.character(ReceiptDateTime)
  ) %>%
  separate(ReceiptDateTime, into = c("Date", "Time"), sep = " ") %>%
  mutate(
    NetAmountExcl = as.numeric(gsub(",", ".", NetAmountExcl)),
    Date = as.Date(Date)
  ) %>%
  arrange(Date, Time)

sale_location_dt <- sales_alt %>%
  select(Date, Time, NetAmountExcl, locationid) %>%
  group_by(locationid, Date, Time) %>%
  summarise(total = sum(NetAmountExcl, na.rm = TRUE), .groups = "drop") %>%
  arrange(Date, Time)

sales_location_na    <- sale_location_dt %>% filter(is.na(locationid))
sales_location_nona  <- sale_location_dt %>% filter(!is.na(locationid))

total_sales_location <- sale_location_dt %>%
  group_by(locationid) %>%
  summarise(total = sum(total, na.rm = TRUE), .groups = "drop") %>%
  arrange(locationid)

visitors_daily <- visitor_hourly %>%
  group_by(Date) %>%
  summarise(total_visitors = sum(NumberOfUsedEntrances, na.rm = TRUE)) %>%
  arrange(Date)

sales_daily <- sales_location_nona %>%
  group_by(Date) %>%
  summarise(total_sales = sum(total, na.rm = TRUE)) %>%
  arrange(Date)

sales_stores_loc <- sales_alt %>%
  select(Date, Time, NetAmountExcl, StoreId, locationid) %>%
  group_by(Date, Time, locationid, StoreId) %>%
  summarise(total = sum(NetAmountExcl, na.rm = TRUE), .groups = "drop") %>%
  arrange(Date, Time, locationid, StoreId)

# ==========================================================
# 📈 6. Visualizations
# ==========================================================

## ---- Bar: Total Sales by Location ----
ggplot(total_sales_location, aes(x = factor(locationid), y = total)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Total NetAmountExcl per Location",
    x = "Location ID", y = "Total NetAmountExcl"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## ---- Line: Visitors Over Time ----
ggplot(visitors_daily, aes(x = as.Date(Date), y = total_visitors)) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  labs(
    title = "Daily Total Visitors Over Time",
    x = "Date", y = "Number of Visitors"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal()

## ---- Line: Sales Over Time ----
ggplot(sales_daily, aes(x = as.Date(Date), y = total_sales)) +
  geom_line(color = "darkgreen", linewidth = 0.8) +
  labs(
    title = "Daily Total Sales Over Time",
    x = "Date", y = "Total Sales (€)"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal()

# ==========================================================
# 💾 7. Save Outputs (optional)
# ==========================================================
# write.csv(sales_stores_loc, "sales_stores_loc.csv", row.names = FALSE)
# write.csv(sales_location_nona, "sales_location_hourly.csv", row.names = FALSE)
#
# all_data <- list(
#   total_per_location = sales_location_nona,
#   synthetic_sales = synthetic_sales,
#   forecast_results = forecast_results_loc
# )
# cat(toJSON(all_data, pretty = TRUE))
