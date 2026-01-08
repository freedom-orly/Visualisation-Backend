dir.create(Sys.getenv("R_LIBS_USER"), recursive = TRUE)  # create personal library
.libPaths(Sys.getenv("R_LIBS_USER"))

args <- commandArgs(trailingOnly = TRUE)

packages <- c(
  "readr",
  "tidyverse",
  "lubridate"
)

install_and_load <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    #message(paste("📦 Installing missing package:", pkg))
    install.packages(pkg, dependencies = TRUE, repos='http://cran.us.r-project.org')
    library(pkg, character.only = TRUE)
  } else {
    #message(paste("✅ Package already loaded:", pkg))
  }
}

for (p in packages) {
  install_and_load(p)
}

vis_id <- args[1]
source(file.path(getwd(),"instance", "store",vis_id,"rscripts", "data_prep_functions.R"))


data_dir <- file.path(getwd(),"instance", "store",vis_id,"data")


sales_data <- read_csv2(file.path(data_dir,"assignment2-fulldata-NDAprotected","sales.csv"))

subgroup_linktable <- read.csv2(file.path(data_dir,"subgroup_catagory.csv"))

#FIRST FILTER DATA FOR STORES WE WANT
#Store 157 not in sales data
stores_list <- c(101, 102,103, 108, 110, 111, 116, 119, 120, 129, 131, 135, 138, 140, 145, 149, 156, 164)

filtered_data <- sales_data %>%
  filter(StoreId %in% stores_list)
View(filtered_data)

catagory_sales_data <- filtered_data %>%
  right_join(subgroup_linktable, by = "SubgroupId", relationship = "many-to-many") %>%
  filter(!is.na(StoreId)) %>%
  filter(!Catagories == "Other") %>%
  select(ReceiptDateTime, NetAmountExcl, SubgroupId, StoreId, Catagories, Subgroup)

View(catagory_sales_data)

#write.csv2(catagory_sales_data, "catagory_sales_data.csv", , row.names = FALSE)

#SUM TOTALS FOR EACH CATAGORY

total_sales <- catagory_sales_data %>%
  #MERGE STORES 119 and 120 (Animazia)
  mutate(StoreId = ifelse(StoreId %in% c(119, 120), 120, StoreId)) %>%
  group_by(StoreId, Catagories) %>%
  summarise(Catagory_Total = sum(NetAmountExcl)) %>%
  group_by(StoreId)

View(total_sales)
#write.csv2(total_sales, "stores_total_sales.csv", , row.names = FALSE)

#SIMILARITY SCORE
store_distribution_percentage <- total_sales %>%
  mutate(pct_sales = round(Catagory_Total / sum(Catagory_Total), 2)) %>%
  ungroup() %>%
  select(StoreId, Catagories, pct_sales) %>%
  pivot_wider(
    names_from = Catagories, 
    values_from = pct_sales, 
    values_fill = 0
  )

View(store_distribution_percentage)
#write.csv2(store_distribution_percentage, "store_distribution_percentage.csv", , row.names = FALSE)


#Calculate store attraativness
sales_cleaned <- filtered_data %>%
  mutate(
    ReceiptDateTime = as.POSIXct(ReceiptDateTime),
    Date = floor_date(ReceiptDateTime, "day")
  ) %>%
  filter(NetAmountExcl >= 0.1)

sales_attractiveness <- sales_cleaned %>%
  group_by(StoreId, Date) %>%
  summarise(DailyTotal = sum(NetAmountExcl)) %>%
  group_by(StoreId) %>%
  summarise(Avg_sales = mean(DailyTotal))

View(sales_attractiveness)
#write.csv2(sales_attractiveness, "sales_attractiveness.csv", , row.names = FALSE)

