# List of required packages
packages <- c(
  "readxl",
  "readr",
  "dplyr",
  "tidyr",
  "ggplot2",
  "scales",
  "synthpop",
  "lubridate",
  "jsonlite",
  "caret",
  "forecast",
  "purrr"
)

# Function to install (if missing) and load each package
install_and_load <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    message(paste("📦 Installing missing package:", pkg))
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  } else {
    message(paste("✅ Package already loaded:", pkg))
  }
}

# Loop through each package
for (p in packages) {
  install_and_load(p)
}

message("\n🎉 All required libraries are installed and loaded successfully!")

read_file_by_id <- function(id_input, base_dir = "intance\\store", file_type = "all") {
  
  # 1️⃣ Construct folder path
  folder_path <- file.path(base_dir, as.character(id_input), as.character("data"))
  
  
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
    # Create clean names (without file extensions)
    file_names <- tools::file_path_sans_ext(basename(files))
    return(setNames(lapply(files, read_single_file), file_names))
  }
  
}
 #Reads any file in folder "instance/store/1/data/"
data <- read_file_by_id(1)

historical_data <- list(
  sales_location_hourly = data$sales_location_hourly,
  total_hourly_visitors = data$total_hourly_visitors
)
 cat(toJSON(historical_data, pretty = TRUE))