# ==========================================================
# 📦 1. Load Libraries
# ==========================================================
library(readxl)     # Import Excel files (.xls, .xlsx)
library(dplyr)      # Data manipulation
library(tidyr)      # Data cleaning & reshaping
library(ggplot2)    # Data visualization
library(scales)     # Axis/label formatting
library(synthpop)   # Synthetic data generation
library(lubridate)  # Date/time handling
library(jsonlite)   # JSON handling
library(caret)      # Machine learning & cross-validation


# ==========================================================
# ⚙️ 2. Utility Functions
# ==========================================================

# ---- Generate evenly spaced time values between dates ----
generate_timespan <- function(start_date, end_date, spread) {
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  
  # Validation
  if (end_date <= start_date) stop("End date must be after start date.")
  if (spread < 2) stop("Spread must be at least 2 (start and end points).")
  
  # Generate sequence
  seq(from = start_date, to = end_date, length.out = spread)
}


# ==========================================================
# 🧮 3. Define Cross-validation Scheme
# ==========================================================
ctrl <- trainControl(method = "cv", number = 10)  # 10-fold CV


# ==========================================================
# 📥 4. Data Import
# ==========================================================

# ---- Excel Files ----
locations     <- read_excel("linktables.xlsx", sheet = "locations")
stores        <- read_excel("linktables.xlsx", sheet = "stores")
departments   <- read_excel("departments.xlsx", sheet = "Blad1")
holidays      <- read_excel("holidays.xlsx", sheet = "Blad1")
hours_sample  <- read_excel("hours_sample.xlsx", sheet = "hours")
teams         <- read_excel("teams.xlsx", sheet = "Blad1")
weather       <- read_excel("weather.xlsx", sheet = "Blad1")

# ---- CSV Files ----
sales          <- read.csv("sales_sample2.csv", header = TRUE, sep = ";")
store          <- read.csv("store.csv", header = TRUE, sep = ";")
subgroup       <- read.csv("subgroup.csv", header = TRUE, sep = ";")
maingroup      <- read.csv("maingroup.csv", header = TRUE, sep = ";")
visitor_hourly <- read.csv("visitorhourly_sample2.csv", header = TRUE, sep = ";")


# ==========================================================
# 🧩 5. Data Merging & Cleaning
# ==========================================================

# ---- Merge store data ----
all_stores <- merge(store, stores, by = "StoreId") %>%
  select(-...4)

# ---- Merge all datasets ----
all_sales <- all_stores %>%
  merge(sales,        by = "StoreId") %>%
  merge(departments,  by = "department_id") %>%
  merge(subgroup,     by = "SubgroupId") %>%
  merge(teams,        by = "department_id") %>%
  merge(maingroup,    by = "MaingroupId")

# ---- Clean up columns ----
all_sales <- all_sales %>%
  select(-office_id.y) %>%
  rename(office_id = office_id.x) %>%
  separate(`ReceiptDateTime`, into = c("Date", "Time"), sep = " ") %>%
  select(
    Date, Time, StoreId, locationid, office_id,
    MaingroupId, Maingroup,
    SubgroupId, Subgroup,
    department_id, department_name,
    team_id, team_name,
    ArticleId, Article,
    Quantity, NetAmountExcl,
    everything()
  ) %>%
  mutate(
    NetAmountExcl = as.numeric(gsub(",", ".", NetAmountExcl))
  )


# ==========================================================
# 💰 6. Synthetic Sales Data
# ==========================================================

sales_on_loc <- all_sales %>%
  select(Date, Time, locationid, NetAmountExcl) %>%
  mutate(
    locationid    = as.factor(locationid),
    Date          = as.Date(Date),
    Time          = as.character(Time),
    NetAmountExcl = as.numeric(NetAmountExcl)
  )

# ---- Step 1: Compute weights ----
location_weights <- sales_on_loc %>%
  group_by(locationid) %>%
  summarise(n = n()) %>%
  mutate(prob = n / sum(n))

# ---- Step 2: Define total synthetic rows ----
n_total <- 10000
location_counts <- round(location_weights$prob * n_total)
names(location_counts) <- location_weights$locationid

# ---- Step 3: Generate synthetic transactions ----
synthetic_list <- lapply(names(location_counts), function(loc) {
  n_loc <- location_counts[loc]
  subset_loc <- sales_on_loc %>% filter(locationid == loc)
  
  data.frame(
    locationid    = loc,
    Date          = sample(subset_loc$Date, n_loc, replace = TRUE),
    Time          = sample(subset_loc$Time, n_loc, replace = TRUE),
    NetAmountExcl = round(
      pmax(0, rnorm(
        n_loc,
        mean = mean(subset_loc$NetAmountExcl, na.rm = TRUE),
        sd   = sd(subset_loc$NetAmountExcl, na.rm = TRUE)
      )), 4)
  )
})

synthetic_sales <- bind_rows(synthetic_list)


# ==========================================================
# 📊 7. Aggregations
# ==========================================================

# ---- Total per location ----
total_per_location <- all_sales %>%
  group_by(locationid) %>%
  summarise(total = sum(NetAmountExcl, na.rm = TRUE))

total_per_location_synth <- synthetic_sales %>%
  group_by(locationid) %>%
  summarise(total = sum(NetAmountExcl, na.rm = TRUE))

# ---- Total and average per day ----
total_per_day_loc <- all_sales %>%
  group_by(Date, locationid) %>%
  summarise(total = sum(NetAmountExcl, na.rm = TRUE))

average_per_day_loc <- all_sales %>%
  group_by(Date, locationid) %>%
  summarise(average = mean(NetAmountExcl, na.rm = TRUE))


# ==========================================================
# 📈 8. Visualizations
# ==========================================================

## ---- Bar: Real Sales ----
ggplot(total_per_location, aes(x = factor(locationid), y = total)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Total NetAmountExcl per Location",
    x = "Location ID",
    y = "Total NetAmountExcl"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## ---- Bar: Synthetic Sales ----
ggplot(total_per_location_synth, aes(x = factor(locationid), y = total)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Total NetAmountExcl per Location (Synthetic)",
    x = "Location ID",
    y = "Total NetAmountExcl"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## ---- Heatmap ----
sales_on_loc$hour <- as.numeric(substr(sales_on_loc$Time, 1, 2))
heatmap_data <- sales_on_loc %>%
  group_by(locationid, Date, hour) %>%
  summarise(count = n(), .groups = "drop")

ggplot(heatmap_data, aes(x = hour, y = Date, fill = count)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkred") +
  labs(
    title = "Heatmap of Data Availability",
    x = "Hour of Day",
    y = "Date",
    fill = "Count"
  ) +
  theme_minimal() +
  facet_wrap(~ locationid, ncol = 2)


# ==========================================================
# 🧾 9. Optional JSON Output
# ==========================================================
# all_data <- list(
#   total_per_location = total_per_location,
#   synthetic_sales     = synthetic_sales,
#   average_per_day     = average_per_day_loc
# )
# cat(toJSON(all_data, pretty = TRUE))
