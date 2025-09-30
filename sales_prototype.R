library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)  # make comma() available
library(synthpop)
library(lubridate)


# reading .xlsx files
locations <- read_excel("linktables.xlsx", sheet = "locations")
# Read the "stores" sheet
stores <- read_excel("linktables.xlsx", sheet = "stores")
departments <- read_excel("departments.xlsx", sheet = "Blad1")
holidays <- read_excel("holidays.xlsx", sheet = "Blad1")
hours_sample <- read_excel("hours_sample.xlsx", sheet = "hours")
teams <- read_excel("teams.xlsx", sheet = "Blad1")
weather <- read_excel("weather.xlsx", sheet = "Blad1")

#reading .csv files
sales <- read.csv("sales_sample2.csv", header = TRUE, sep = ";")
store <- read.csv("store.csv", header = TRUE, sep = ";")
subgroup <- read.csv("subgroup.csv", header = TRUE, sep = ";")
maingroup <- read.csv("maingroup.csv", header = TRUE, sep = ";")
visitor_hourly <- read.csv("visitorhourly_sample2.csv", header = TRUE, sep = ";")

unique(sales$StoreId)
unique(store$StoreId)

#store_sort
all_stores <- merge(store, stores, by = "StoreId")
all_stores$...4 <- NULL

#merging the data
all_sales <- merge(all_stores, sales, by = "StoreId")
all_sales <- merge(all_sales,departments , by ="department_id")
all_sales <- merge(all_sales, subgroup, by = "SubgroupId")
all_sales <- merge(all_sales, teams, by = "department_id")
all_sales <- merge(all_sales, maingroup, by = "MaingroupId")

all_sales$office_id.y <- NULL
# Rename column: new_name = old_name
all_sales <- all_sales %>% rename(office_id = office_id.x)

all_sales <- all_sales %>%
  separate(`ReceiptDateTime`, into = c("Date", "Time"), sep = " ")

all_sales <- all_sales %>%
  select(
    Date, Time,
    StoreId, locationid, office_id,
    MaingroupId, Maingroup,
    SubgroupId, Subgroup,
    department_id, department_name,
    team_id, team_name,
    ArticleId, Article,
    Quantity, NetAmountExcl,
    everything()  # catches leftover cols like ...4
  )

#convert the transactions to numerics
all_sales <- all_sales %>%
  mutate(
    NetAmountExcl = gsub(",", ".", NetAmountExcl),  # replace comma with dot
    NetAmountExcl = as.numeric(NetAmountExcl)       # convert to numeric
  )

#View(all_sales)

sales_on_loc <- all_sales %>%
  select(Date, Time, locationid, NetAmountExcl)

# Ensure correct types
sales_on_loc$locationid <- as.factor(sales_on_loc$locationid)
sales_on_loc$Date <- as.Date(sales_on_loc$Date)
sales_on_loc$Time <- as.character(sales_on_loc$Time)
sales_on_loc$NetAmountExcl <- as.numeric(sales_on_loc$NetAmountExcl)

# Step 1: Compute location weights for proportional sampling
location_weights <- sales_on_loc %>%
  group_by(locationid) %>%
  summarise(n = n()) %>%
  mutate(prob = n / sum(n))

# Step 2: Decide total number of synthetic transactions
n_total <- 10000  # adjust as needed

# Step 3: Compute number of synthetic rows per location
location_counts <- round(location_weights$prob * n_total)
names(location_counts) <- location_weights$locationid

# Step 4: Generate synthetic transactions per location
synthetic_list <- lapply(names(location_counts), function(loc) {
  n_loc <- location_counts[loc]
  
  # Subset original data for this location
  subset_loc <- sales_on_loc %>% filter(locationid == loc)
  
  # Sample Date and Time proportionally from original data
  date_sample <- sample(subset_loc$Date, n_loc, replace = TRUE)
  time_sample <- sample(subset_loc$Time, n_loc, replace = TRUE)
  
  # Generate prices independently around mean and SD
  price_sample <- rnorm(n_loc,
                        mean = mean(subset_loc$NetAmountExcl, na.rm = TRUE),
                        sd   = sd(subset_loc$NetAmountExcl, na.rm = TRUE))
  
  # Replace negative prices with 0
  price_sample[price_sample < 0] <- 0
  
  data.frame(locationid = loc,
             Date = date_sample,
             Time = time_sample,
             NetAmountExcl = round(price_sample, 4))
})

# Combine all locations
synthetic_sales <- do.call(rbind, synthetic_list)

#calculate the total sales across the data per store
total_per_location <- all_sales %>%
  group_by(locationid) %>%
  summarise(total = sum(NetAmountExcl, na.rm = TRUE))

total_per_location_synth <- synthetic_sales %>%
  group_by(locationid) %>%
  summarise(total = sum(NetAmountExcl, na.rm = TRUE))

#geom bar graph for the total sales per locationid
ggplot(total_per_location, aes(x = factor(locationid), y = total)) +
  geom_bar(stat = "identity", fill = "steelblue") +  # create bars
  labs(
    title = "Total NetAmountExcl per location",
    x = "location id",
    y = "Total NetAmountExcl"
  ) +
  scale_y_continuous(labels = scales::comma) +  # show full numbers with commas
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # rotate x labels if needed

#geom bar graph for the total sales per locationid
ggplot(total_per_location_synth, aes(x = factor(locationid), y = total)) +
  geom_bar(stat = "identity", fill = "steelblue") +  # create bars
  labs(
    title = "Total NetAmountExcl per location synth",
    x = "location id",
    y = "Total NetAmountExcl"
  ) +
  scale_y_continuous(labels = scales::comma) +  # show full numbers with commas
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # rotate x labels if needed


