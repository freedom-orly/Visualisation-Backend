library(readr)
library(tidyverse)
library(lubridate)

total_sales <- read_csv2("stores_total_sales.csv")
store_distribution <- read_csv2("store_distribution_percentage.csv")
sales_attractiveness <- read_csv2("sales_attractiveness.csv")
distance_df <- read.csv("DATA/completed_friction_matrix2.csv", check.names = FALSE)

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
    
    print(paste("Processed Closure for Store:", i, "| Loss Rate:", round(loss_prob, 1), "%"))
  }
    return(final_results)
  
}

get_attractivness <- function(storeId){
  value <- sales_attractiveness %>%
    filter(StoreId == storeId) %>%
    pull(Avg_sales)
  
  if (length(value) == 0) {
    print(paste("WARNING: Store", storeId, "not found in Sales Data."))
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
loss_rate <- 0.2
new_results <- simulation_engin(stores_list, stores_to_close_list, loss_rate)
View(new_results)