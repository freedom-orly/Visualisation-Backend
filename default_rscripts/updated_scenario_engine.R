dir.create(Sys.getenv("R_LIBS_USER"), recursive = TRUE)  # create personal library
.libPaths(Sys.getenv("R_LIBS_USER"))

args <- commandArgs(trailingOnly = TRUE)

packages <- c(
  "readr",
  "tidyverse",
  "lubridate",
  "jsonlite",
  "purrr"
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

parameters_json_str <- args[2]
parameters <- fromJSON(parameters_json_str)
#print(parameters)

vis_id <- args[1]
source(file.path(getwd(),"instance", "store",vis_id,"rscripts", "data_prep_functions.R"))


data_dir <- file.path(getwd(),"instance", "store",vis_id,"data")

data_prep(vis_id)

#total_sales <- read_csv2(file.path(data_dir,"stores_total_sales.csv"))
store_distribution <- read_csv2(file.path(data_dir,"store_distribution_percentage.csv"))
sales_attractiveness <- read_csv2(file.path(data_dir,"sales_attractiveness.csv"))
distance_df <- read.csv(file.path(data_dir,"completed_friction_matrix2.csv"), check.names = FALSE)

#take first colum values to label the rows
rownames(distance_df) <- as.character(distance_df$Origin)
#cleans df so just numbers
df_clean <- distance_df[, -1]
#covert to matrix
distance_matrix <- as.matrix(df_clean)

stores_list <- c(101, 102, 108, 110, 111, 116, 120, 129, 131, 135, 138, 140, 145, 149, 156, 164)

stores_to_close_list <- c(135, 131)

simulation_engin <- function(stores_list, stores_to_close, loss_rate){
  #get final list of open stores based on closed stores list
  open_stores <- setdiff(stores_list, stores_to_close)
  
  final_results <- data.frame()
  
  #loop for each closed store
  for (closed_store in stores_to_close){
    temp_results <- data.frame(
      Source_Store = numeric(),
      Target_Store = numeric(),
      Utility_Score = numeric(),
      stringsAsFactors = FALSE
    )
    
    i <- closed_store
    
    #calculate transfer to open stores
    for (j in open_stores){
      if (j == i){ next}
      
      attractivness <- get_attractivness(j)
      similarity <- calc_simularity(j, i)
      friction_val <- 2
      
      #get the distance value for the 2 stores
      distance_value <- tryCatch({
        distance_matrix[as.character(i), as.character(j)]
      }, error = function(e) { 100 }) #if error then make 100
      
      #U_{ij} = A_j \times S_{ij} \times \frac{1}{(D_{ij})^\lambda}
      store_pull <- attractivness * similarity * (1 / (distance_value^friction_val))
      
      temp_results <- rbind(temp_results, data.frame(
        Source_Store = i, 
        Target_Store = j,
        Utility_Score = store_pull
        ))
    }
    
    #Calculate the probability of the current store
  
    total_physical_utility <- sum(temp_results$Utility_Score)
    
    #Calculate K value based on target loss rate
    
    k_new <- total_physical_utility * (loss_rate / (1 - loss_rate))
    
    total_system_utility <- total_physical_utility + k_new
    #probability formula
    temp_results$Probability <- (temp_results$Utility_Score / total_system_utility) * 100
    
    loss_prob <- (k_new / total_system_utility) * 100
    
    temp_results <- rbind(temp_results, data.frame(
      Source_Store = i,
      Target_Store = "Walk_Away",
      Utility_Score = k_new,
      Probability = loss_prob
    ))
    
    final_results <- rbind(final_results, temp_results)
    
    #print(paste("Processed Closure for Store:", i, "| Loss Rate:", round(loss_prob, 1), "%"))
  }
    return(final_results)
  
}

get_attractivness <- function(storeId){
  value <- sales_attractiveness %>%
    filter(StoreId == storeId) %>%
    pull(Avg_sales)
  
  if (length(value) == 0) {
    #print(paste("WARNING: Store", storeId, "not found in Sales Data."))
    return(0)
  }
  
  return(value)
}

#cosine similarity
calc_simularity <- function(openStore, closedStore){
  closed_food_value <- store_distribution %>%
    filter(StoreId == closedStore) %>%
    pull(Food_Beverage)
  closed_merch_value <- store_distribution %>%
    filter(StoreId == closedStore) %>%
    pull(Merchandise)
  open_food_value <- store_distribution %>%
    filter(StoreId == openStore) %>%
    pull(Food_Beverage)
  open_merch_value <- store_distribution %>%
    filter(StoreId == openStore) %>%
    pull(Merchandise)
  
  food_part <- closed_food_value * open_food_value
  merch_value <- closed_merch_value * open_merch_value
  total_value <- food_part + merch_value
  
  closed_lenght <- sqrt(closed_food_value**2 + closed_merch_value**2)
  open_lenght <- sqrt(open_food_value**2 + open_merch_value**2)
  
  #similarity
  sim <- total_value / (closed_lenght * open_lenght)
  
  if (length(open_food_value) == 0 || length(closed_food_value) == 0) {
    return(0) 
  }
  
  return(sim)
}


#------------------------REDISTRIBUTE FORECASTED SALES--------------------------


#RUN
loss_rate <- parameters$loss_rate
stores_to_close_list <- c(as.integer(parameters$stores_to_close))
new_results <- simulation_engin(stores_list, stores_to_close_list, loss_rate)
#View(new_results)

formated <- list()

for (s in unique(new_results$Source_Store)) {

  d <- new_results[new_results$Source_Store == s, ]

  formated <- append(
    formated,
    list(
      list(
        name = paste0("Probability_", s),
        values = map2(d$Target_Store, d$Probability,
                      ~ list(x = .x, y = .y))
      ),
      list(
        name = paste0("Utility_Score_", s),
        values = map2(d$Target_Store, d$Utility_Score,
                      ~ list(x = .x, y = .y))
      )
    )
  )
}


# #print results
cat(toJSON(formated, pretty = TRUE, auto_unbox = TRUE))