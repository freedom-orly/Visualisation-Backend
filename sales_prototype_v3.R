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

# ==========================================================
# 🧮 3. Cross-validation
# ==========================================================
ctrl <- trainControl(method = "cv", number = 10)

# ==========================================================
# 📥 4. Data Import
# ==========================================================
locations     <- read_excel("linktables.xlsx", sheet = "locations")
stores        <- read_excel("linktables.xlsx", sheet = "stores")
departments   <- read_excel("departments.xlsx", sheet = "Blad1")
holidays      <- read_excel("holidays.xlsx", sheet = "Blad1")
hours_sample  <- read_excel("hours_sample.xlsx", sheet = "hours")
teams         <- read_excel("teams.xlsx", sheet = "Blad1")
weather       <- read_excel("weather.xlsx", sheet = "Blad1")

sales          <- read.csv("sales_sample2.csv", header = TRUE, sep = ";")
store          <- read.csv("store.csv", header = TRUE, sep = ";")
subgroup       <- read.csv("subgroup.csv", header = TRUE, sep = ";")
maingroup      <- read.csv("maingroup.csv", header = TRUE, sep = ";")
visitor_hourly <- read.csv("visitorhourly_sample2.csv", header = TRUE, sep = ";")

# ==========================================================
# 🧩 5. Merge & Clean
# ==========================================================
all_stores <- merge(store, stores, by = "StoreId") %>% select(-...4)

all_sales <- all_stores %>%
  merge(sales,        by = "StoreId") %>%
  merge(departments,  by = "department_id") %>%
  merge(subgroup,     by = "SubgroupId") %>%
  merge(teams,        by = "department_id") %>%
  merge(maingroup,    by = "MaingroupId") %>%
  select(-office_id.y) %>%
  rename(office_id = office_id.x) %>%
  separate(`ReceiptDateTime`, into = c("Date", "Time"), sep = " ") %>%
  mutate(
    NetAmountExcl = as.numeric(gsub(",", ".", NetAmountExcl)),
    Date = as.Date(Date)
  )

# ==========================================================
# 💰 6. Synthetic Sales
# ==========================================================
sales_on_loc <- all_sales %>%
  select(Date, Time, locationid, NetAmountExcl) %>%
  mutate(
    locationid = as.factor(locationid),
    Time = as.character(Time)
  )

location_weights <- sales_on_loc %>%
  group_by(locationid) %>%
  summarise(n = n()) %>%
  mutate(prob = n / sum(n))

n_total <- 10000
location_counts <- round(location_weights$prob * n_total)
names(location_counts) <- location_weights$locationid

synthetic_list <- lapply(names(location_counts), function(loc) {
  n_loc <- location_counts[loc]
  subset_loc <- sales_on_loc %>% filter(locationid == loc)
  
  data.frame(
    locationid = loc,
    Date = sample(subset_loc$Date, n_loc, replace = TRUE),
    Time = sample(subset_loc$Time, n_loc, replace = TRUE),
    NetAmountExcl = round(
      pmax(0, rnorm(n_loc,
                    mean = mean(subset_loc$NetAmountExcl, na.rm = TRUE),
                    sd   = sd(subset_loc$NetAmountExcl, na.rm = TRUE)
      )), 4)
  )
})
synthetic_sales <- bind_rows(synthetic_list)

# ==========================================================
# 📊 7. Aggregations
# ==========================================================
total_per_location <- all_sales %>%
  group_by(locationid) %>%
  summarise(total = sum(NetAmountExcl, na.rm = TRUE), .groups = "drop")

total_per_day_loc <- all_sales %>%
  group_by(Date, locationid) %>%
  summarise(total = sum(NetAmountExcl, na.rm = TRUE), .groups = "drop")

# ==========================================================
# 🔮 8. Forecasting: log-regression + residual bootstrap (dow-matched)
# ==========================================================
forecast_bootstrap_dowmatched <- function(data,
                                          date_col = "Date",
                                          value_col = "total",
                                          location_col = "locationid",
                                          h = 7,
                                          B = 1000,
                                          min_points_for_dow = 5,
                                          seed = 42,
                                          plot_result = TRUE) {
  set.seed(seed)
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  library(scales)
  
  data <- as.data.frame(data)
  data[[date_col]] <- as.Date(data[[date_col]])
  data[[location_col]] <- as.factor(data[[location_col]])
  
  results <- list()
  locs <- unique(data[[location_col]])
  
  for (loc in locs) {
    sd <- data[data[[location_col]] == loc, , drop = FALSE]
    sd <- sd[order(sd[[date_col]]), , drop = FALSE]
    n <- nrow(sd)
    
    if (n < 2) {
      warning("Skipping location ", loc, " — fewer than 2 observations.")
      next
    }
    
    # Features
    sd$day_index <- as.numeric(difftime(sd[[date_col]], min(sd[[date_col]]), units = "days"))
    sd$dow <- factor(as.integer(lubridate::wday(sd[[date_col]], week_start = 1)), levels = 1:7)
    sd$value <- as.numeric(sd[[value_col]])
    
    # Historical min/max for clipping
    hist_min <- min(sd$value, na.rm = TRUE)
    hist_max <- max(sd$value, na.rm = TRUE)
    
    # Choose model complexity
    fit <- NULL
    fit_type <- NULL
    if (n >= min_points_for_dow) {
      fit_type <- "log1p(day_index + dow)"
      fit <- tryCatch(lm(log1p(value) ~ day_index + dow, data = sd),
                      error = function(e) { message("lm failed for loc ", loc, ": ", conditionMessage(e)); NULL })
    }
    if (is.null(fit) && n >= 3) {
      fit_type <- "log1p(day_index)"
      fit <- tryCatch(lm(log1p(value) ~ day_index, data = sd),
                      error = function(e) { message("trend lm failed for loc ", loc, ": ", conditionMessage(e)); NULL })
    }
    
    # Future dates
    last_date <- max(sd[[date_col]])
    future_dates <- seq(last_date + 1, by = "day", length.out = h)
    future_day_index <- as.numeric(difftime(future_dates, min(sd[[date_col]]), units = "days"))
    future_dow <- factor(as.integer(lubridate::wday(future_dates, week_start = 1)), levels = 1:7)
    newdata <- data.frame(day_index = future_day_index, dow = future_dow)
    
    df_fc <- NULL
    sims_local <- NULL
    
    # If model available -> residual bootstrap (dow-matched)
    if (!is.null(fit)) {
      resid_log <- residuals(fit)
      # Clip extreme residuals
      sd_res <- sd(resid_log, na.rm = TRUE)
      mean_res <- mean(resid_log, na.rm = TRUE)
      resid_log <- pmax(pmin(resid_log, mean_res + 2.5*sd_res), mean_res - 2.5*sd_res)
      
      pred_log_point <- tryCatch(predict(fit, newdata = newdata), error = function(e) rep(NA_real_, h))
      if (any(!is.finite(pred_log_point))) {
        message("Non-finite predictions for loc ", loc, " — falling back to all-history sampling.")
        fit <- NULL
      } else {
        resid_pools <- split(resid_log, as.character(sd$dow))
        sims <- matrix(NA_real_, nrow = B, ncol = h)
        resid_scale <- 0.7  # damp residuals
        
        for (b in seq_len(B)) {
          samp_resid <- numeric(h)
          for (j in seq_len(h)) {
            d_chr <- as.character(as.integer(future_dow[j]))
            pool <- resid_pools[[d_chr]]
            if (is.null(pool) || length(pool) == 0) pool <- resid_log
            samp_resid[j] <- sample(pool, size = 1, replace = TRUE)
          }
          sim_log <- pred_log_point + resid_scale * samp_resid
          # --- Clip forecasts to realistic range ---
          sim_val <- pmax(exp(sim_log) - 1, hist_min)
          sim_val <- pmin(sim_val, hist_max * 1.5)
          sims[b, ] <- sim_val
        }
        
        sims_local <- sims
        fc_mean <- colMeans(sims, na.rm = TRUE)
        # 80% CI (10%-90%)
        fc_lower <- apply(sims, 2, quantile, probs = 0.1, na.rm = TRUE)
        fc_upper <- apply(sims, 2, quantile, probs = 0.9, na.rm = TRUE)
        
        df_fc <- data.frame(
          Date = future_dates,
          Forecast = fc_mean,
          Lower = fc_lower,
          Upper = fc_upper,
          locationid = loc,
          stringsAsFactors = FALSE
        )
      }
    }
    
    # Fallback: all-history sampling
    if (is.null(fit)) {
      used_model <- if (is.null(fit_type)) "all-history-sampling" else paste0(fit_type,"-fallback")
      hist_vals <- sd$value
      sims_np <- replicate(B, {
        samp <- pmax(sample(hist_vals, size = h, replace = TRUE), hist_min)
        pmin(samp, hist_max * 1.5)
      })
      if (nrow(sims_np) != h) sims_np <- t(sims_np)
      
      sims_local <- sims_np
      fc_mean <- rowMeans(sims_np)
      fc_lower <- apply(sims_np, 1, quantile, probs = 0.1, na.rm = TRUE)
      fc_upper <- apply(sims_np, 1, quantile, probs = 0.9, na.rm = TRUE)
      
      df_fc <- data.frame(
        Date = future_dates,
        Forecast = fc_mean,
        Lower = fc_lower,
        Upper = fc_upper,
        locationid = loc,
        stringsAsFactors = FALSE
      )
    } else {
      used_model <- fit_type
    }
    
    # Historical frame
    hist_df <- sd[, c(date_col, value_col)]
    names(hist_df) <- c("Date", "Value")
    
    # Plot
    p <- ggplot() +
      geom_line(data = hist_df, aes(x = Date, y = Value), color = "#0072B2", size = 1.1) +
      geom_line(data = df_fc,   aes(x = Date, y = Forecast), color = "#D55E00", size = 1) +
      geom_ribbon(data = df_fc, aes(x = Date, ymin = Lower, ymax = Upper),
                  fill = "#D55E00", alpha = 0.2) +
      scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
      labs(title = paste0("Bootstrap-residual Forecast — Location ", loc),
           subtitle = paste0(used_model, " | B=", B, " | horizon=", h, " days"),
           x = "Date", y = "Sales (NetAmountExcl)") +
      theme_minimal(base_size = 13)
    
    if (plot_result) print(p)
    
    results[[as.character(loc)]] <- list(
      model = if (!is.null(fit)) fit else NULL,
      fit_type = used_model,
      forecast = df_fc,
      sims = sims_local,
      plot = p
    )
  }
  
  return(results)
}




# ---------- Run the forecasting function on your data ----------
fc_all <- forecast_bootstrap_dowmatched(
  data = total_per_day_loc,
  date_col = "Date",
  value_col = "total",
  location_col = "locationid",
  h = 7,
  B = 1000,
  min_points_for_dow = 5,
  seed = 42,
  plot_result = TRUE
)

# ==========================================================
# 📈 9. Other Visualizations
# ==========================================================
## ---- Bar: Real Sales ----
ggplot(total_per_location, aes(x = factor(locationid), y = total)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Total NetAmountExcl per Location",
       x = "Location ID", y = "Total NetAmountExcl") +
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

## ---- Line Chart: Actual Sales Over Time per Location ----
ggplot(total_per_day_loc, aes(x = Date, y = total, color = factor(locationid), group = locationid)) +
  geom_line(size = 1.1) +
  scale_color_brewer(palette = "Set2", name = "Location ID") +
  scale_y_continuous(labels = comma, limits = c(0, NA)) +
  labs(
    title = "Actual Daily Sales per Location",
    subtitle = "Trend of total daily NetAmountExcl for each location",
    x = "Date",
    y = "Total Sales (NetAmountExcl)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# ==========================================================
# 🧾 10. Optional JSON Export
# ==========================================================
# all_data <- list(
#   total_per_location = total_per_location,
#   synthetic_sales = synthetic_sales,
#   forecast_results = forecast_results_loc
# )
# cat(toJSON(all_data, pretty = TRUE))

loc <- unique(total_per_day_loc$locationid)[1]
fc <- fc_all[[as.character(loc)]]$forecast
summary(fc)
