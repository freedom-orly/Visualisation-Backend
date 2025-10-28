# ==========================================================
# SALES FORECASTING PROTOTYPE — IMPROVED & RESTRUCTURED
# - clearer sections, safer joins, continuous daily series (fills missing days)
# - improved forecasting: ARIMA (when enough history) + dow-aware regression + bootstrap
# - better back-transforms, simulation-based prediction intervals
# ==========================================================

# ---------------------------
# 0. CONFIG
# ---------------------------
config <- list(
  data_dir = "E:/code/gp/Visualisation/data",
  n_synthetic_total = 10000,
  min_days_for_arima = 28,    # require at least 4 weeks for auto.arima
  min_pts_for_dow = 5,
  B_sim = 1000,
  h = 7,
  seed = 42,
  plot_result = TRUE
)

# ---------------------------
# 1. LIBRARIES
# ---------------------------
libs <- c("readxl","dplyr","tidyr","ggplot2","scales","lubridate",
          "jsonlite","caret","forecast","purrr","splines")
invisible(lapply(libs, require, character.only = TRUE))

# ---------------------------
# 2. DATA IMPORT (parameterised)
# ---------------------------
read_inputs <- function(base_dir = config$data_dir) {
  # adjust paths as needed
  locations     <- read_excel(file.path(base_dir, "linktables.xlsx"), sheet = "locations")
  stores        <- read_excel(file.path(base_dir, "linktables.xlsx"), sheet = "stores")
  departments   <- read_excel(file.path(base_dir, "departments.xlsx"),  sheet = "Blad1")
  holidays      <- read_excel(file.path(base_dir, "holidays.xlsx"),     sheet = "Blad1")
  hours_sample  <- read_excel(file.path(base_dir, "hours_sample.xlsx"), sheet = "hours")
  teams         <- read_excel(file.path(base_dir, "teams.xlsx"),        sheet = "Blad1")
  weather       <- read_excel(file.path(base_dir, "weather.xlsx"),      sheet = "Blad1")
  
  sales          <- read.csv(file.path(base_dir, "sales_sample2.csv"), header = TRUE, sep = ";", stringsAsFactors = FALSE)
  store          <- read.csv(file.path(base_dir, "store.csv"),         header = TRUE, sep = ";", stringsAsFactors = FALSE)
  subgroup       <- read.csv(file.path(base_dir, "subgroup.csv"),      header = TRUE, sep = ";", stringsAsFactors = FALSE)
  maingroup      <- read.csv(file.path(base_dir, "maingroup.csv"),     header = TRUE, sep = ";", stringsAsFactors = FALSE)
  visitor_hourly <- read.csv(file.path(base_dir, "visitorhourly_sample2.csv"), header = TRUE, sep = ";", stringsAsFactors = FALSE)
  
  list(
    locations = locations, stores = stores, departments = departments, holidays = holidays,
    hours_sample = hours_sample, teams = teams, weather = weather,
    sales = sales, store = store, subgroup = subgroup, maingroup = maingroup,
    visitor_hourly = visitor_hourly
  )
}

# ---------------------------
# 3. CLEAN & MERGE
# ---------------------------
prepare_sales <- function(inputs) {
  all_stores <- inputs$store %>%
    left_join(inputs$stores, by = "StoreId") %>%
    select(-any_of("...4"))
  
  all_sales <- all_stores %>%
    left_join(inputs$sales,       by = "StoreId") %>%
    left_join(inputs$departments, by = c("department_id" = "department_id")) %>%
    left_join(inputs$subgroup,    by = c("SubgroupId" = "SubgroupId")) %>%
    left_join(inputs$teams %>% distinct(department_id, .keep_all = TRUE), by = c("department_id" = "department_id")) %>%
    left_join(inputs$maingroup,   by = c("MaingroupId" = "MaingroupId")) %>%
    select(-ends_with(".y"), -ends_with(".x")) %>%
    separate(`ReceiptDateTime`, into = c("Date", "Time"), sep = " ", remove = TRUE, fill = "right") %>%
    mutate(
      NetAmountExcl = as.numeric(gsub(",", ".", NetAmountExcl)),
      Date = as.Date(Date),
      across(where(is.character), ~trimws(.))
    ) %>%
    filter(!is.na(NetAmountExcl), NetAmountExcl >= 0)
  
  all_sales
}

# ---------------------------
# 4. SYNTHETIC SALES (optional)
# ---------------------------
make_synthetic_sales <- function(all_sales, n_total = config$n_synthetic_total) {
  sales_on_loc <- all_sales %>%
    select(Date, Time, locationid, NetAmountExcl) %>%
    mutate(locationid = as.factor(locationid), Time = as.character(Time)) %>%
    filter(complete.cases(.))
  
  location_weights <- sales_on_loc %>% count(locationid, name = "n") %>% mutate(prob = n / sum(n))
  location_counts <- set_names(round(location_weights$prob * n_total), location_weights$locationid)
  
  synthetic_list <- purrr::imap(location_counts, ~{
    sub <- sales_on_loc %>% filter(locationid == .y)
    if (nrow(sub) == 0) return(NULL)
    
    data.frame(
      locationid = .y,
      Date = sample(sub$Date, .x, replace = TRUE),
      Time = sample(sub$Time, .x, replace = TRUE),
      NetAmountExcl = round(pmax(0, rnorm(.x, mean = mean(sub$NetAmountExcl), sd = sd(sub$NetAmountExcl))), 4),
      stringsAsFactors = FALSE
    )
  }) %>% compact()
  
  bind_rows(synthetic_list)
}

# ---------------------------
# 5. AGGREGATIONS (ensure continuous daily series per location)
# ---------------------------
make_aggregations <- function(all_sales) {
  total_per_location <- all_sales %>% group_by(locationid) %>% summarise(total = sum(NetAmountExcl, na.rm = TRUE), .groups = "drop")
  
  total_per_day_loc <- all_sales %>%
    group_by(locationid, Date) %>%
    summarise(total = sum(NetAmountExcl, na.rm = TRUE), .groups = "drop") %>%
    group_by(locationid) %>%
    tidyr::complete(Date = seq(min(Date), max(Date), by = "day")) %>%
    tidyr::replace_na(list(total = 0)) %>%
    ungroup() %>%
    filter(!is.na(total), is.finite(total), !is.na(locationid))
  
  list(total_per_location = total_per_location, total_per_day_loc = total_per_day_loc)
}

# ---------------------------
# 6. FORECASTING — improved hybrid strategy
# ---------------------------
forecast_bootstrap_improved <- function(data,
                                        date_col = "Date",
                                        value_col = "total",
                                        location_col = "locationid",
                                        h = config$h,
                                        B = config$B_sim,
                                        min_days_for_arima = config$min_days_for_arima,
                                        min_points_for_dow = config$min_pts_for_dow,
                                        seed = config$seed,
                                        plot_result = config$plot_result) {
  set.seed(seed)
  data <- as_tibble(data)
  data[[date_col]]   <- as.Date(data[[date_col]])
  data[[location_col]] <- as.factor(data[[location_col]])
  
  results <- list()
  locs <- unique(data[[location_col]])
  
  for (loc in locs) {
    sd <- data %>% filter(.data[[location_col]] == loc) %>% arrange(.data[[date_col]])
    n  <- nrow(sd)
    if (n < 2) { warning("Skipping location ", loc, " – <2 obs."); next }
    
    # ensure contiguous daily series
    full_dates <- seq(min(sd[[date_col]]), max(sd[[date_col]]), by = "day")
    sd <- tibble(Date = full_dates) %>%
      left_join(sd, by = "Date") %>%
      mutate(locationid = as.character(loc), total = replace_na(.data[[value_col]], 0)) %>%
      mutate(day_index = as.numeric(difftime(Date, min(Date), units = "days")),
             dow = factor(wday(Date, week_start = 1), levels = 1:7),
             value = as.numeric(total)) %>%
      filter(is.finite(value), value >= 0)
    
    n_days <- nrow(sd)
    
    # Prepare forecast dates and newdata for regression-based models
    last_date <- max(sd$Date)
    future_dates <- seq(last_date + 1, by = "day", length.out = h)
    newdata <- tibble(
      Date = future_dates,
      day_index = as.numeric(difftime(future_dates, min(sd$Date), units = "days")),
      dow = factor(wday(future_dates, week_start = 1), levels = levels(sd$dow))
    )
    
    df_fc <- NULL
    sims <- NULL
    model_used <- NULL
    
    # 1) If long enough, try ARIMA with weekly seasonality
    if (n_days >= min_days_for_arima) {
      ts_vals <- ts(sd$value, frequency = 7)
      fit_a <- tryCatch(auto.arima(ts_vals), error = function(e) NULL)
      if (!is.null(fit_a)) {
        model_used <- paste0("ARIMA: ", deparse(fit_a$call))
        # deterministic forecast
        fc <- forecast::forecast(fit_a, h = h, level = 95)
        # simulate B future paths (this captures parameter uncertainty + innovation noise)
        sims_mat <- replicate(B, as.numeric(simulate(fit_a, nsim = h)), simplify = "matrix")
        sims <- t(sims_mat)  # B x h
        sims[sims < 0] <- 0
        
        df_fc <- tibble(
          Date = future_dates,
          Forecast = colMeans(sims),
          Lower = apply(sims, 2, quantile, 0.025),
          Upper = apply(sims, 2, quantile, 0.975),
          locationid = loc
        )
      }
    }
    
    # 2) If ARIMA not used, try dow-aware regression on log1p with spline trend
    if (is.null(df_fc) && n_days >= min_points_for_dow) {
      # Use flexible trend: natural spline on day_index (3 df) + dow
      fit_lm <- tryCatch(lm(log1p(value) ~ ns(day_index, df = 3) + dow, data = sd), error = function(e) NULL)
      if (!is.null(fit_lm)) {
        model_used <- "log1p(ns(day_index,3) + dow)"
        pred <- predict(fit_lm, newdata = newdata, se.fit = TRUE)
        pred_fit <- pred$fit
        # residuals grouped by dow
        resid <- residuals(fit_lm)
        resid_pools <- split(resid, sd$dow)
        
        sims <- matrix(NA, nrow = B, ncol = h)
        for (b in seq_len(B)) {
          samp_resid <- sapply(seq_len(h), function(j) {
            pool <- resid_pools[[as.character(newdata$dow[j])]]
            if (is.null(pool) || length(pool) == 0) pool <- resid
            sample(pool, 1)
          })
          sims[b, ] <- pmax(exp(pred_fit + samp_resid) - 1, 0)
        }
        
        df_fc <- tibble(
          Date = future_dates,
          Forecast = colMeans(sims),
          Lower = apply(sims, 2, quantile, 0.025),
          Upper = apply(sims, 2, quantile, 0.975),
          locationid = loc
        )
      }
    }
    
    # 3) Fallback: non-parametric bootstrap of historical values
    if (is.null(df_fc)) {
      hist_vals <- sd$value
      sims_np <- replicate(B, sample(hist_vals, h, replace = TRUE), simplify = "matrix")
      sims_np <- t(sims_np)
      df_fc <- tibble(
        Date = future_dates,
        Forecast = colMeans(sims_np),
        Lower = apply(sims_np, 2, quantile, 0.025),
        Upper = apply(sims_np, 2, quantile, 0.975),
        locationid = loc
      ) %>% mutate(across(c(Forecast, Lower, Upper), ~pmax(., 0)))
      model_used <- "nonparametric-sampling"
      sims <- sims_np
    }
    
    # prepare historical series for plotting
    hist_df <- sd %>% select(Date, Value = value)
    
    # plot
    p <- ggplot() +
      geom_line(data = hist_df, aes(Date, Value), color = "#0072B2", size = 1.1) +
      geom_line(data = df_fc,   aes(Date, Forecast), color = "#D55E00", size = 1) +
      geom_ribbon(data = df_fc, aes(Date, ymin = Lower, ymax = Upper), fill = "#D55E00", alpha = .2) +
      scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
      labs(title = paste("Location", loc), subtitle = paste(model_used, "| B =", B, "| h =", h), x = "Date", y = "Sales") +
      theme_minimal(base_size = 13)
    
    if (plot_result) print(p)
    
    results[[as.character(loc)]] <- list(model = if (exists("fit_a")) fit_a else fit_lm,
                                         model_used = model_used,
                                         forecast = df_fc, sims = sims, plot = p)
  }
  
  return(results)
}

# ---------------------------
# 7. RUN EVERYTHING (example)
# ---------------------------
 inputs <- read_inputs()
 all_sales <- prepare_sales(inputs)
 syn <- make_synthetic_sales(all_sales)              # optional
 aggs <- make_aggregations(all_sales)
 fc_all <- forecast_bootstrap_improved(aggs$total_per_day_loc)

# ---------------------------
# 8. QUICK VISUALISATIONS
# ---------------------------
# Total per location
# ggplot(aggs$total_per_location, aes(factor(locationid), total)) +
#   geom_col(fill = "steelblue") + labs(title = "Total Sales per Location", x = "Location ID", y = "Sales") +
#   scale_y_continuous(labels = scales::comma) + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Transaction heatmap
# sales_on_loc_plot <- all_sales %>% mutate(hour = as.numeric(substr(Time, 1, 2)))
# heatmap_data <- sales_on_loc_plot %>% count(locationid, Date, hour, name = "count")
# ggplot(heatmap_data, aes(hour, Date, fill = count)) + geom_tile(colour = "white") + labs(title = "Transaction Heatmap", x = "Hour", y = "Date") + theme_minimal() + facet_wrap(~ locationid, ncol = 2)

# ---------------------------
# End of script
# ---------------------------
