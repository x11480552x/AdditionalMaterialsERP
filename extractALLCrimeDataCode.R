extract_crime_data <- function(LAD_name, LSOA_codes) {
  # lookup the police force for the given LAD
  police_force <- LADPolice2022 %>%
    filter(LAD22NM == LAD_name) %>%
    select(PFA22NM) %>%
    distinct() %>%
    pull()
  
  # check if police force was found
  if (length(police_force) == 0) {
    message(paste("No matching police force found for LAD:", LAD_name))
    return(NULL)  # skip if not
  }
  
  # check if police force is Greater Manchester, skip if true
  if (grepl("Greater Manchester", police_force, ignore.case = TRUE)) {
    message(paste("Skipping Greater Manchester LAD:", LAD_name))
    return(NULL)  
  }
  
  # construct the file path
  file_name <- tolower(gsub(" ", "-", police_force))
  file_path <- paste0("/home/user/", file_name, "-all.csv") #change with your own filepath
  
  message(paste("Processing LAD:", LAD_name, "with police force:", police_force))
  message(paste("Looking for file at:", file_path))
  
  # check if the file exists
  if (!file.exists(file_path)) {
    message(paste("CSV file not found for police force:", police_force, "from LAD:", LAD_name))
    return(NULL)  # skip if the file does not exist
  }
  
  # read the CSV file for the identified police force
  crime_data <- read.csv(file_path)
  
  # filter the data for the specified LSOA codes
  filtered_data <- crime_data %>%
    filter(LSOA.name %in% LSOA_codes)
  
  if (nrow(filtered_data) == 0) {
    message(paste("No data found for LSOAs in LAD:", LAD_name))
    return(NULL)  # skip if no data for specified LSOA codes
  }
  
  # filter the data for the specified crime types
  crime_types <- c("Anti-social behaviour", "Violence and sexual offences", "Public order", "Criminal damage and arson")
  filtered_data <- filtered_data %>%
    filter(Crime.type %in% crime_types)
  
  # select the required columns
  selected_columns <- c("Crime.ID", "Month", "Longitude", "Latitude", "LSOA.name", "Crime.type")
  filtered_data <- filtered_data %>%
    select(all_of(selected_columns))
  
  return(filtered_data)
}

#test
extract_crime_data('Liverpool', 'Liverpool 060D')


# define the number of cores available
num_cores <- 39

# Create a cluster
cl <- makeCluster(num_cores)

# load necessary libraries on all worker nodes
clusterEvalQ(cl, {
  library(dplyr)
})

# export necessary functions and data to the worker nodes
clusterExport(cl, c("extract_crime_data", "LADPolice2022", "filtered_lsoa_shapes", "get_lad_name"))

# Run the parallelised function with debugging
crime_data_list <- parLapply(cl, 1:nrow(filtered_lsoa_shapes), function(i) {
  lsoa_shape <- filtered_lsoa_shapes[i, ]
  LAD_name <- get_lad_name(lsoa_shape$LSOA21NM)
  LSOA_codes <- lsoa_shape$LSOA21CD
  
  message(paste("Processing LAD:", LAD_name, "with LSOA code:", LSOA_codes))
  
  # use tryCatch to handle errors and log them
  result <- tryCatch({
    extract_crime_data(LAD_name, LSOA_codes)
  }, error = function(e) {
    message(paste("Error in processing LAD:", LAD_name, "with LSOA code:", LSOA_codes, "Error message:", e$message))
    return(NULL)  # return NULL on error
  })
  
  return(result)
})

# filter out NULL results 
crime_data_list <- crime_data_list[!sapply(crime_data_list, is.null)]

# stop the cluster
stopCluster(cl)

