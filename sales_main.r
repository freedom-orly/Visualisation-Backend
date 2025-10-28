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
generate_timespan <- function(start_date, end_date, spread) {
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  if (end_date <= start_date) stop("End date must be after start date.")
  if (spread < 2) stop("Spread must be at least 2.")
  seq(from = start_date, to = end_date, length.out = spread)
}

# id_input: numeric ID
# base_dir: the root folder where subfolders are stored
# file_type: optional, e.g. "csv", "xlsx", "all"
read_file_by_id <- function(id_input, base_dir = "visualizations", file_type = "all") {
  
  # 1️⃣ Construct folder path
  folder_path <- file.path(base_dir, as.character(id_input))
  
  if (!dir.exists(folder_path)) {
    stop("Folder does not exist: ", folder_path)
  }
  
  # 2️⃣ List files in folder
  files <- list.files(folder_path, full.names = TRUE)
  
  if (length(files) == 0) {
    stop("No files found in folder: ", folder_path)
  }
  
  # 3️⃣ Optionally filter by file type
  if (file_type != "all") {
    files <- files[grepl(paste0("\\.", file_type, "$"), files)]
    if (length(files) == 0) {
      stop("No files of type '", file_type, "' found in folder: ", folder_path)
    }
  }
  
  # 4️⃣ Read files
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
  
  # 5️⃣ Return single file or list of files
  if (length(files) == 1) {
    return(read_single_file(files))
  } else {
    return(lapply(files, read_single_file))
  }
}
# Reads any file in folder "visualizations/ID_123"
#data <- read_file_by_id(123)

# Reads only CSV files
#data_csv <- read_file_by_id(123, file_type = "csv")


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
  
  # --- 3️⃣ Combine for possible later use ---
  combined_year <- visitors_year %>%
    left_join(sales_year, by = "Date")
  
  # --- 4️⃣ Plot 1: Visitors ---
  p1 <- ggplot(visitors_year, aes(x = Date, y = total_visitors)) +
    geom_line(color = "steelblue", linewidth = 0.8) +
    labs(
      title = paste("🧍‍♂️ Total Visitors per Day -", year_input),
      x = "Date", y = "Number of Visitors"
    ) +
    scale_y_continuous(labels = comma) +
    theme_minimal(base_size = 13)
  
  # --- 5️⃣ Plot 2: Sales ---
  p2 <- ggplot(sales_year, aes(x = Date, y = total_sales)) +
    geom_line(color = "darkgreen", linewidth = 0.8) +
    labs(
      title = paste("💰 Total Sales per Day -", year_input),
      x = "Date", y = "Total Sales"
    ) +
    scale_y_continuous(labels = comma) +
    theme_minimal(base_size = 13)
  
  # --- 6️⃣ Print plots to viewer ---
  print(p1)
  print(p2)
  
  # --- 7️⃣ Return data for further analysis ---
  return(list(
    visitors_daily = visitors_year,
    sales_daily = sales_year,
    combined = combined_year,
    visitors_plot = p1,
    sales_plot = p2
  ))
}

heatmaps_by_location <- function(year_input, sales_data = sales_location_nona) {
  
  # --- 1️⃣ Filter to the specified year and extract month/hour ---
  sales_year <- sales_data %>%
    mutate(
      Date = as.Date(Date),
      Month = month(Date, label = TRUE, abbr = TRUE),
      Hour = as.numeric(substr(Time, 1, 2))
    ) %>%
    filter(year(Date) == year_input) %>%
    filter(!is.na(locationid))  # remove NA locations
  
  # --- 2️⃣ Aggregate by location, month, and hour ---
  sales_hourly <- sales_year %>%
    group_by(locationid, Month, Hour) %>%
    summarise(total_sales = sum(total, na.rm = TRUE), .groups = "drop") %>%
    mutate(Month = factor(Month, levels = month.abb))  # ensure correct month order
  
  # --- 3️⃣ Generate a list of heatmaps, one per location ---
  locations <- unique(sales_hourly$locationid)
  heatmap_list <- lapply(locations, function(loc) {
    data_loc <- sales_hourly %>% filter(locationid == loc)
    
    ggplot(data_loc, aes(x = Hour, y = Month, fill = total_sales)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "lightyellow", high = "darkred", name = "Sales") +
      scale_x_continuous(breaks = 0:23) +
      labs(
        title = paste("Heatmap of Sales by Hour - Location", loc, "-", year_input),
        x = "Hour of Day",
        y = "Month"
      ) +
      theme_minimal(base_size = 12)
  })
  
  names(heatmap_list) <- paste0("Location_", locations)
  
  return(list(
    aggregated_data = sales_hourly,
    heatmaps = heatmap_list
  ))
}
result_2024 <- heatmaps_by_location(2023)
print(result_2024$heatmaps$Location_4)  # heatmap for location 1


# ==========================================================
# 🧮 3. Cross-validation
# ==========================================================
ctrl <- trainControl(method = "cv", number = 10)


# ==========================================================
# 📥 4. Data Import
# ==========================================================
sales <- read.csv("sales.csv", header = TRUE, sep = ";")
hours <- read_excel("hours.xlsx")
locations     <- read_excel("linktables.xlsx", sheet = "locations")
links        <- read_excel("linktables.xlsx", sheet = "stores")
departments   <- read_excel("departments.xlsx", sheet = "Blad1")
holidays      <- read_excel("holidays.xlsx", sheet = "Blad1")
teams         <- read_excel("teams.xlsx", sheet = "Blad1")
store          <- read.csv("store.csv", header = TRUE, sep = ";")
subgroup       <- read.csv("subgroup.csv", header = TRUE, sep = ";")
maingroup      <- read.csv("maingroup.csv", header = TRUE, sep = ";")
visitor_hourly <- read.csv("visitorhourly.csv", header = TRUE, sep = ";")

total_hourly_visitors <- visitor_hourly %>%
  group_by(Date, Time) %>%
  summarise(total_visitors = sum(NumberOfUsedEntrances, na.rm = TRUE), .groups = "drop") %>%
  arrange(Date, Time)

sales_alt <- sales %>%
  left_join(links %>% select(StoreId, locationid),
            by = "StoreId") %>%
  mutate(ReceiptDateTime = as.character(ReceiptDateTime)) %>%  # convert before separating
  separate(ReceiptDateTime, into = c("Date", "Time"), sep = " ") %>%
  mutate(
    NetAmountExcl = as.numeric(gsub(",", ".", NetAmountExcl)),
    Date = as.Date(Date)
  ) %>%
  arrange(Date, Time)

sale_location_dt <- sales_alt %>%
  select(Date, Time,NetAmountExcl, locationid) %>%
  group_by(locationid, Date, Time) %>%
  summarise(total = sum(NetAmountExcl, na.rm = TRUE), .groups = "drop") %>%
  arrange(Date, Time)

sales_location_na <- sale_location_dt %>%
  filter(is.na(locationid))

sales_location_nona <- sale_location_dt %>%
  filter(!is.na(locationid))

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

data_2024 <- get_year_data(2025)
head(data_2024$visitors_daily)
head(data_2024$sales_daily)
head(data_2024$combined)


# ==========================================================
# 📈 9. Other Visualizations
# ==========================================================
## ---- Bar: Real Sales ----
ggplot(total_sales_location, aes(x = factor(locationid), y = total)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Total NetAmountExcl per Location",
       x = "Location ID", y = "Total NetAmountExcl") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(visitors_daily, aes(x = as.Date(Date), y = total_visitors)) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  labs(
    title = "Daily Total Visitors Over Time",
    x = "Date", y = "Number of Visitors"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal()

ggplot(sales_daily, aes(x = as.Date(Date), y = total_sales)) +
  geom_line(color = "darkgreen", linewidth = 0.8) +
  labs(
    title = "Daily Total Sales Over Time",
    x = "Date", y = "Total Sales (€)"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal()


