#################### crime_buffer_eps75_NEW creation ####################

# Select the desired columns from all_points_unique2.sf
selected_columns <- all_points_unique2.sf %>%
  select(osm_id, name, shop, amenity)
# miss girl i had to make some decisions-which are shops and which are amenities when some are BOTH UHHH
valid_shops <- c("alcohol", "wine", "convenience")
valid_amenities <- c("bar", "pub", "nightclub", "biergarten", "casino", "stripclub", "restaurant")

# Set invalid shops to NA
selected_columns$shop <- ifelse(selected_columns$shop %in% valid_shops, 
                                selected_columns$shop, 
                                NA)

# Set invalid amenities to NA
selected_columns$amenity <- ifelse(selected_columns$amenity %in% valid_amenities, 
                                   selected_columns$amenity, 
                                   NA)

# 1. Alcohol = NA when amenity = pub, bar, restaurant
selected_columns$shop <- ifelse(selected_columns$amenity %in% c("pub", "bar", "restaurant") & 
                                  selected_columns$shop == "alcohol", 
                                NA, 
                                selected_columns$shop)

# 2. Wine = NA when amenity = bar, restaurant
selected_columns$shop <- ifelse(selected_columns$amenity %in% c("bar", "restaurant") & 
                                  selected_columns$shop == "wine", 
                                NA, 
                                selected_columns$shop)

# List of OSM IDs that need their 'shop' value updated to NA
osm_ids_to_update <- c(2428095208, 8495101135, 217683441, 246569256, 373120778)


# Update the 'shop' column to NA for these specific OSM IDs
selected_columns$shop <- ifelse(selected_columns$osm_id %in% osm_ids_to_update, 
                                NA, 
                                selected_columns$shop)

# Check the first few rows to ensure the updates were made
head(selected_columns %>% filter(osm_id %in% osm_ids_to_update))

# double checkin
# Check for any rows where both 'shop' and 'amenity' have values
both_filled <- selected_columns %>%
  filter(!is.na(shop) & !is.na(amenity)) #none here

# Check for any rows where both 'shop' and 'amenity' are NA
both_na <- selected_columns %>%
  filter(is.na(shop) & is.na(amenity))

head(both_na) #none


#ok so now i need to put it back into results2, retrieve coordinates, retrieve the amenity from
#allpointsunique, and then merge it with my crime buffer eps75

# Extract X and Y coordinates from the geometry column in selected_columns
selected_columns$X <- st_coordinates(selected_columns)[,1]
selected_columns$Y <- st_coordinates(selected_columns)[,2]

# Function to count outlets by type in each cluster for a given LSOA
count_outlets_by_type <- function(cluster_data, outlet_data) {
  
  # Ensure cluster_data has 'X' and 'Y' columns
  if (!("X" %in% colnames(cluster_data)) | !("Y" %in% colnames(cluster_data))) {
    stop("X and Y columns are missing in cluster_data")
  }
  
  # Convert sf objects to regular data frames for the join
  cluster_df <- as.data.frame(cluster_data)
  outlet_df <- as.data.frame(outlet_data)
  
  # Perform the join based on X and Y coordinates
  merged_data <- cluster_df %>%
    left_join(outlet_df, by = c("X", "Y"))
  
  # Filter out noise (cluster = 0)
  merged_data <- merged_data %>% filter(cluster != 0)
  
  # Count the number of each type of outlet per cluster
  outlet_counts <- merged_data %>%
    group_by(cluster) %>%
    summarise(
      pub = sum(amenity == "pub", na.rm = TRUE),
      bar = sum(amenity == "bar", na.rm = TRUE),
      nightclub = sum(amenity == "nightclub", na.rm = TRUE),
      biergarten = sum(amenity == "biergarten", na.rm = TRUE),
      casino = sum(amenity == "casino", na.rm = TRUE),
      stripclub = sum(amenity == "stripclub", na.rm = TRUE),
      restaurant = sum(amenity == "restaurant", na.rm = TRUE),
      convenience = sum(shop == "convenience", na.rm = TRUE),
      alcohol = sum(shop == "alcohol", na.rm = TRUE),
      wine = sum(shop == "wine", na.rm = TRUE)
    )
  
  return(outlet_counts)
}

# Define outlet types
outlet_types <- c("pub", "bar", "nightclub", "biergarten", "casino", "stripclub", 
                  "restaurant", "convenience", "alcohol", "wine")

# Set up parallel processing
num_cores <- detectCores() - 1
cl <- makeCluster(num_cores)

# Export necessary variables and functions to the cluster
clusterExport(cl, c("results2", "selected_columns", "outlet_types", "count_outlets_by_type"))
clusterEvalQ(cl, {
  library(sf)
  library(dplyr)
})

# Parallel processing of LSOAs
crime_buffer_eps75 <- parLapply(cl, seq_along(results2), function(i) {
  lsoa_clusters <- results2[[i]]
  
  # Count outlets for each cluster
  outlet_counts <- count_outlets_by_type(lsoa_clusters, selected_columns)
  
  # Merge outlet counts back into the original cluster data
  lsoa_clusters_df <- as.data.frame(lsoa_clusters) %>%
    left_join(outlet_counts, by = "cluster")
  
  # Reconvert to sf if needed
  lsoa_clusters_sf <- st_as_sf(lsoa_clusters_df, coords = c("X", "Y"), crs = st_crs(selected_columns))
  
  return(lsoa_clusters_sf)
})

# Stop the cluster after processing
stopCluster(cl)

# Combine the results into a single data frame or sf object
crime_buffer_eps75 <- do.call(rbind, crime_buffer_eps75)

# View the resulting data frame or sf object
head(crime_buffer_eps75) 


# this worked.

# however, filtering needed to be done
crime_buffer_eps75 <- crime_buffer_eps75 %>%
  filter(cluster != 0)
# Remove the geometry column
crime_buffer_eps75_no_geometry <- crime_buffer_eps75 %>%
  st_set_geometry(NULL)

# Remove duplicates
crime_buffer_eps75_no_duplicates <- crime_buffer_eps75_no_geometry %>%
  distinct()

# List of Greater Manchester regions
greater_manchester_regions <- c(
  "Bolton", "Bury", "Manchester", "Oldham", 
  "Rochdale", "Salford", "Stockport", 
  "Tameside", "Trafford", "Wigan"
)

# Filter out LSOAs that are in Greater Manchester
crime_buffer_eps75_no_duplicates_filtered <- crime_buffer_eps75_no_duplicates %>%
  filter(!sapply(LSOA21NM, function(x) any(grepl(paste(greater_manchester_regions, collapse="|"), x))))

# Check the result
nrow(crime_buffer_eps75_no_duplicates_filtered)
nrow(crime_buffer_eps75_2)
#1 extra? this extra 1 doesnt have any crime data so we get rid
crime_buffer_eps75_no_duplicates_filtered$LSOA21CD <- crime_buffer_eps75_no_duplicates_filtered$LSOA
# Identify the LSOA and LSOA21NM present in crime_buffer_eps75_no_duplicates_filtered but not in crime_buffer_eps75_2
extra_row <- anti_join(
  crime_buffer_eps75_no_duplicates_filtered[, c("LSOA21CD", "LSOA21NM")],
  crime_buffer_eps75_2[, c("LSOA21CD", "LSOA21NM")],
  by = c("LSOA21CD", "LSOA21NM")
)

# Display the extra LSOA and LSOA21NM
print(extra_row)
# Remove the specific row from crime_buffer_eps75_no_duplicates_filtered
crime_buffer_eps75_no_duplicates_filtered <- crime_buffer_eps75_no_duplicates_filtered %>%
  filter(!(LSOA == "E01035701" & LSOA21NM == "Tower Hamlets 034B"))

# Convert the `cluster` column in crime_buffer_eps75_2 to integer
crime_buffer_eps75_2$cluster <- as.integer(crime_buffer_eps75_2$cluster)

# Merge the two data frames based on LSOA and cluster
crime_buffer_eps75_NEW <- left_join(crime_buffer_eps75_2, 
                                    crime_buffer_eps75_no_duplicates_filtered, 
                                    by = c("LSOA21CD" = "LSOA21CD", "cluster" = "cluster"))

# tidy up the dataset by removing cols
crime_buffer_eps75_NEW <- crime_buffer_eps75_NEW %>%
  select(-cluster_assignment, -LSOA, -LSOA21NM.y) %>%
  rename(LSOA21NM = LSOA21NM.x)

#verify that the no.of outlets in cluster add up to all the types
crime_buffer_eps75_NEW <- crime_buffer_eps75_NEW %>%
  mutate(total_outlet_types = pub + bar + nightclub + biergarten + 
           casino + stripclub + restaurant + 
           convenience + alcohol + wine)

# check if `no_of_outlets_in_cluster` matches `total_outlet_types`
verification <- crime_buffer_eps75_NEW %>%
  mutate(verification = no_of_outlets_in_cluster == total_outlet_types)

# Display rows where the verification failed
mismatch <- verification %>%
  filter(verification == FALSE)

# Print the mismatched rows
print(mismatch) #no rows - good

#delete column
crime_buffer_eps75_NEW$total_outlet_types <- NULL