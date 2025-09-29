# Read CSV
data <- read.csv2("C:/Users/matin/Desktop/visitor_sample.csv")

library(dplyr)

# Convert columns properly
data$date <- as.Date(data$date, format="%Y-%m-%d")

# If hour column is just numbers (0–23), keep as is
# If it's a string like "13:45:00", extract the hour
if (is.character(data$hour)) {
  data$hour <- substr(data$hour, 1, 2)   # take "13" from "13:45:00"
  data$hour <- as.integer(data$hour)
}

# ---- Visitors per day ----
visitors_per_day <- data %>%
  group_by(day = date) %>%
  summarise(visitors = n())

print(visitors_per_day)

# ---- Visitors per day per hour ----
visitors_per_hour <- data %>%
  group_by(day = date, hour = hour) %>%
  summarise(visitors = n())

print(visitors_per_hour)

# ---- Plot example: pick one day ----
one_day <- filter(visitors_per_hour, day == as.Date("2025-01-02"))

plot(one_day$hour, one_day$visitors,
     type = "o", col = "red",
     xlab = "Hour of Day", ylab = "Number of Visitors",
     main = "Visitors Per Hour (2025-01-02)")
