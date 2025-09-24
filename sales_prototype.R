library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)  # make comma() available


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

View(all_sales)

#calculate the total sales across the data per store
total_per_store <- all_sales %>%
  group_by(StoreId) %>%
  summarise(total = sum(NetAmountExcl, na.rm = TRUE))

#geom bar graph for the total sales per store
ggplot(total_per_store, aes(x = factor(StoreId), y = total)) +
  geom_bar(stat = "identity", fill = "steelblue") +  # create bars
  labs(
    title = "Total NetAmountExcl per Store",
    x = "Store ID",
    y = "Total NetAmountExcl"
  ) +
  scale_y_continuous(labels = scales::comma) +  # show full numbers with commas
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # rotate x labels if needed


