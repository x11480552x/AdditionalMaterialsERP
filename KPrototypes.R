############################ K-PROTOTYPES CODE ############################
# select the desired columns from all_points_unique2.sf
selected_columns <- all_points_unique2.sf %>%
  select(osm_id, name, shop, amenity)
# i had to make some decisions-which are shops and which are amenities when some are BOTH 
valid_shops <- c("alcohol", "wine", "convenience")
valid_amenities <- c("bar", "pub", "nightclub", "biergarten", "casino", "stripclub", "restaurant")

# set invalid shops to NA
selected_columns$shop <- ifelse(selected_columns$shop %in% valid_shops, 
                                selected_columns$shop, 
                                NA)

# set invalid amenities to NA
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

# list of OSM IDs that need their 'shop' value updated to NA
osm_ids_to_update <- c(2428095208, 8495101135, 217683441, 246569256, 373120778)


# update the 'shop' column to NA for these specific OSM IDs
selected_columns$shop <- ifelse(selected_columns$osm_id %in% osm_ids_to_update, 
                                NA, 
                                selected_columns$shop)

# check the first few rows to ensure the updates were made
head(selected_columns %>% filter(osm_id %in% osm_ids_to_update))

# double checking for any rows where both 'shop' and 'amenity' have values
both_filled <- selected_columns %>%
  filter(!is.na(shop) & !is.na(amenity)) #none here

#  double check for any rows where both 'shop' and 'amenity' are NA
both_na <- selected_columns %>%
  filter(is.na(shop) & is.na(amenity))

head(both_na) #none


#ok so now i need to put it back into results, retrieve coordinates, retrieve the amenity from
#allpointsunique, and then merge it with my crime buffer eps75
# this is to obtain the number of the the types of alcohol outlets per cluster 

# assuming `results` is your list
names(results) <- sapply(results, function(df) df$LSOA[1])

# check the result
names(results)

# function to count outlets by type in each cluster for a given LSOA
count_outlets_by_type <- function(cluster_data, outlet_data) {
  
  # ensure cluster_data has 'X' and 'Y' columns
  if (!("X" %in% colnames(cluster_data)) | !("Y" %in% colnames(cluster_data))) {
    stop("X and Y columns are missing in cluster_data")
  }
  
  # convert sf objects to regular data frames for the join
  cluster_df <- as.data.frame(cluster_data)
  outlet_df <- as.data.frame(outlet_data)
  
  # perform the join based on X and Y coordinates
  merged_data <- cluster_df %>%
    left_join(outlet_df, by = c("X", "Y"))
  
  # filter out noise (cluster = 0)
  merged_data <- merged_data %>% filter(cluster != 0)
  
  # count the number of each type of outlet per cluster
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

# define outlet types
outlet_types <- c("pub", "bar", "nightclub", "biergarten", "casino", "stripclub", 
                  "restaurant", "convenience", "alcohol", "wine")

# set up parallel processing
num_cores <- detectCores() - 1
cl <- makeCluster(num_cores)

# export necessary variables and functions to the cluster
clusterExport(cl, c("results", "selected_columns", "outlet_types", "count_outlets_by_type"))
clusterEvalQ(cl, {
  library(sf)
  library(dplyr)
})

# parallel processing of LSOAs
crime_buffer_eps75 <- parLapply(cl, seq_along(results), function(i) {
  lsoa_clusters <- results[[i]]
  
  # count outlets for each cluster
  outlet_counts <- count_outlets_by_type(lsoa_clusters, selected_columns)
  
  # merge outlet counts back into the original cluster data
  lsoa_clusters_df <- as.data.frame(lsoa_clusters) %>%
    left_join(outlet_counts, by = "cluster")
  
  # reconvert to sf if needed
  lsoa_clusters_sf <- st_as_sf(lsoa_clusters_df, coords = c("X", "Y"), crs = st_crs(selected_columns))
  
  return(lsoa_clusters_sf)
})

# stop the cluster after processing
stopCluster(cl)

# combine the results into a single data frame or sf object
crime_buffer_eps75 <- do.call(rbind, crime_buffer_eps75)

# view the resulting data frame or sf object
head(crime_buffer_eps75) 


# this worked.

# however, filtering needed to be done
crime_buffer_eps75 <- crime_buffer_eps75 %>%
  filter(cluster != 0)
# remove the geometry column
crime_buffer_eps75_no_geometry <- crime_buffer_eps75 %>%
  st_set_geometry(NULL)

# remove duplicates
crime_buffer_eps75_no_duplicates <- crime_buffer_eps75_no_geometry %>%
  distinct()

# list of Greater Manchester regions (this is because we don't have any crime data for the clusters so we need to get rid of these)
greater_manchester_regions <- c(
  "Bolton", "Bury", "Manchester", "Oldham", 
  "Rochdale", "Salford", "Stockport", 
  "Tameside", "Trafford", "Wigan"
)

# filter out LSOAs that are in Greater Manchester
crime_buffer_eps75_no_duplicates_filtered <- crime_buffer_eps75_no_duplicates %>%
  filter(!sapply(LSOA21NM, function(x) any(grepl(paste(greater_manchester_regions, collapse="|"), x))))

# check the result
nrow(crime_buffer_eps75_no_duplicates_filtered)
nrow(crime_buffer_eps75_2)
#1 extra? this extra 1 doesnt have any crime data so we get rid
crime_buffer_eps75_no_duplicates_filtered$LSOA21CD <- crime_buffer_eps75_no_duplicates_filtered$LSOA
# identify the LSOA and LSOA21NM present in crime_buffer_eps75_no_duplicates_filtered but not in crime_buffer_eps75_2
extra_row <- anti_join(
  crime_buffer_eps75_no_duplicates_filtered[, c("LSOA21CD", "LSOA21NM")],
  crime_buffer_eps75_2[, c("LSOA21CD", "LSOA21NM")],
  by = c("LSOA21CD", "LSOA21NM")
)

# display the extra LSOA and LSOA21NM
print(extra_row)
# remove the specific row from crime_buffer_eps75_no_duplicates_filtered
crime_buffer_eps75_no_duplicates_filtered <- crime_buffer_eps75_no_duplicates_filtered %>%
  filter(!(LSOA == "E01035701" & LSOA21NM == "Tower Hamlets 034B"))

# convert the `cluster` column in crime_buffer_eps75_2 to integer
crime_buffer_eps75_2$cluster <- as.integer(crime_buffer_eps75_2$cluster)

# merge the two data frames based on LSOA and cluster
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

# display rows where the verification failed
mismatch <- verification %>%
  filter(verification == FALSE)

# print the mismatched rows
print(mismatch) #no rows - good

#delete column
crime_buffer_eps75_NEW$total_outlet_types <- NULL
#new dataset with outlet numbers
colnames(crime_buffer_eps75_NEW)
#correlation plot 
cor_matrix <- cor(crime_buffer_eps75_NEW %>% select_if(is.numeric))
corrplot(cor_matrix, method = "number", tl.cex = 0.8, number.cex=0.4)

# we test validation_kproto to find the best avg silhouette for a set of variables
set.seed(123)
k_values <- 2:20  # range of k

# w/o crime, outlet type
data_for_clustering <- crime_buffer_eps75_NEW %>%
  select(area_km2,
         RUC11, IMD_Score, ah3ahah_rnk, ah3pubs, pub, bar, biergarten, casino, nightclub,
         stripclub, restaurant, alcohol, wine, convenience)
#categorical variables to factors
data_for_clustering$RUC11 <- as.factor(data_for_clustering$RUC11)
names(data_for_clustering)
#scaling numerical cols
numerical_columns <- sapply(data_for_clustering, is.numeric)
data_for_clustering[numerical_columns] <- scale(data_for_clustering[numerical_columns]) #for better results
within_errors <- numeric(length(k_values))
val <- validation_kproto(data = data_for_clustering,  method = "silhouette", k =k_values, type='huang')
val #kopt:2, silhouette: 0.8102921

#crime type, no outlets
data_for_clustering2 <-  crime_buffer_eps75_NEW %>%
  dplyr::select(area_km2, `Violence and sexual offences`, `Public order`, `Anti-social behaviour`,
                `Criminal damage and arson`,
                RUC11, IMD_Score, ah3ahah_rnk, ah3pubs)
numerical_columns <- sapply(data_for_clustering2, is.numeric)
data_for_clustering2[numerical_columns] <- scale(data_for_clustering2[numerical_columns])
data_for_clustering2$RUC11 <- as.factor(data_for_clustering2$RUC11)
val2 <- validation_kproto(data = data_for_clustering2,  method = "silhouette", k =k_values, type='huang')
val2 # kopt:2, silhouette: 0.775795

#includes crime type, and outlet type
data_for_clustering3 <- crime_buffer_eps75_NEW %>%
  dplyr::select(area_km2, `Violence and sexual offences`, `Public order`, `Anti-social behaviour`,
                `Criminal damage and arson`,
                RUC11, IMD_Score, ah3ahah_rnk, ah3pubs, pub, bar, biergarten, casino, nightclub,
                stripclub, restaurant, alcohol, wine, convenience)
numerical_columns <- sapply(data_for_clustering3, is.numeric)
data_for_clustering3[numerical_columns] <- scale(data_for_clustering3[numerical_columns])
data_for_clustering3$RUC11 <- as.factor(data_for_clustering3$RUC11)
val3 <- validation_kproto(data = data_for_clustering3,  method = "silhouette", k =k_values, type='huang')
val3 #kopt:2, silhouette: 0.7922284

#crime (total, not types), outlet types
data_for_clustering4 <- crime_buffer_eps75_NEW %>%
  dplyr::select(area_km2, total_crimes,
                RUC11, IMD_Score, ah3ahah_rnk, ah3pubs, pub, bar, biergarten, casino, nightclub,
                stripclub, restaurant, alcohol, wine, convenience)
numerical_columns <- sapply(data_for_clustering4, is.numeric)
data_for_clustering4[numerical_columns] <- scale(data_for_clustering4[numerical_columns])
data_for_clustering4$RUC11 <- as.factor(data_for_clustering4$RUC11)
val4 <- validation_kproto(data = data_for_clustering4,  method = "silhouette", k =k_values, type='huang')
val4 #kopt:2, silhouette: 0.7959836


# we don't want k=2, so we'll use the elbow plot to find a higher k.
# loop over each k and calculate within-cluster error
for (i in seq_along(k_values)) {
  set.seed(123)
  kproto_result <- kproto(data_for_clustering,i)
  within_errors[i-1] <- sum(kproto_result$withinss)
}
within_errors

elbow_data <- data.frame(k = k_values, within_error = within_errors)
elbow_data
#elbow plot to see the k we can work with
library(ggplot2)
ggplot(elbow_data, aes(x = k, y = within_error)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Elbow Plot for K-Prototypes Clustering",
       x = "Number of Clusters (k)",
       y = "Total Within-Cluster Error") +
  theme_minimal() #k=8 shows the elbow.


#application

set.seed(123)
k <- 8
kproto_result <- kproto(data_for_clustering, k, type='huang')
print(kproto_result)


#add the kproto cluster assignments to new dataset (we use unscaled here so we can create the table)
data_for_clustering_unscaled <- crime_buffer_eps75_NEW %>%
  select(area_km2,
         RUC11, IMD_Score, ah3ahah_rnk, ah3pubs, pub, bar, biergarten, casino, nightclub,
         stripclub, restaurant, alcohol, wine, convenience)
head(data_for_clustering_unscaled)
# extract the cluster assignments from kproto_result
cluster_assignments <- kproto_result$cluster
# add the cluster assignments to the original data
data_for_clustering_unscaled$kproto_cluster <- cluster_assignments


#lets try and do that table 

# calculate the overall mean for each numeric variable
numeric_vars <- colnames(data_for_clustering_unscaled)[sapply(data_for_clustering_unscaled, is.numeric)]
overall_means <- colMeans(data_for_clustering_unscaled[, numeric_vars], na.rm = TRUE)

# initialize an empty list to store the results
percentage_above_below_mean <- list()

# Calculate percentages above and below the mean for each numeric variable within each cluster
for (var in numeric_vars) {
  mean_value <- overall_means[var]
  
  percentage_above_below_mean[[var]] <- data_for_clustering_unscaled %>%
    mutate(above_mean = ifelse(!!sym(var) > mean_value, 1, 0)) %>%
    group_by(kproto_cluster) %>%
    summarize(
      percentage_above_mean = sum(above_mean, na.rm = TRUE) / n() * 100,
      percentage_below_mean = (n() - sum(above_mean, na.rm = TRUE)) / n() * 100
    ) %>%
    mutate(variable = var) %>%
    select(variable, kproto_cluster, percentage_above_mean, percentage_below_mean)
}

# combine the results into a single data frame
final_percentage_table <- bind_rows(percentage_above_below_mean)
#checking
print(final_percentage_table)

# now RUC11
# calculate the percentage of each RUC11 category within each cluster
percentage_ruc11 <- data_for_clustering_unscaled %>%
  group_by(kproto_cluster, RUC11) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  select(kproto_cluster, RUC11, percentage)

#checking
print(percentage_ruc11)

#all unique RUC11 categories
all_ruc11_categories <- unique(percentage_ruc11$RUC11)
# create a data frame with all combinations of clusters and RUC11 categories
full_combinations <- expand.grid(
  kproto_cluster = 1:8,
  RUC11 = all_ruc11_categories
)
# merge with the existing RUC11 data
percentage_ruc11_complete <- full_combinations %>%
  left_join(percentage_ruc11, by = c("kproto_cluster", "RUC11")) %>%
  mutate(percentage = ifelse(is.na(percentage), 0, percentage))


# merge with the RUC11 Data to fill missing values with 0
percentage_ruc11_complete <- full_combinations %>%
  left_join(percentage_ruc11_named, by = c("kproto_cluster", "RUC11")) %>%
  mutate(percentage = ifelse(is.na(percentage), 0, percentage))

# add cluster nicknames
cluster_nicknames <- data.frame(
  kproto_cluster = 1:8,
  nickname = c("Urban Less Deprived", "Urban Healthy", "Rural Pub-heavy", 
               "Urban Hazardous", "Urban Low-outlet", "Restaurant Heavy", 
               "Large Hazardous", "Deprived High Access")
)

# merge filled RUC11 data with cluster nicknames
percentage_ruc11_complete_named <- percentage_ruc11_complete %>%
  left_join(cluster_nicknames, by = "kproto_cluster") %>%
  rename(variable = RUC11)

# prepare the numeric variable percentages for merging
final_percentage_table_named <- final_percentage_table %>%
  left_join(cluster_nicknames, by = "kproto_cluster") %>%
  pivot_longer(cols = c(percentage_above_mean, percentage_below_mean),
               names_to = "type",
               values_to = "percentage") %>%
  select(variable, kproto_cluster, nickname, type, percentage)

#combine with RUC11 Percentages
combined_table_complete <- bind_rows(
  final_percentage_table_named,
  percentage_ruc11_complete_named %>%
    mutate(type = "RUC11 Percentage") %>%
    select(variable, kproto_cluster, nickname, type, percentage)
)

# pivot to make final table
final_table_complete <- combined_table_complete %>%
  pivot_wider(names_from = c(kproto_cluster, nickname), 
              values_from = percentage) %>%
  arrange(variable, type)

# checking
print(final_table_complete)

data_for_clustering$cluster_assignment <- as.factor(data_for_clustering$cluster_assignment)
# pair plots with clusters colored
ggpairs(data_for_clustering, aes(color = cluster_assignment, alpha = 0.6),
        upper = list(continuous = "points"),
        lower = list(continuous = "points"),
        diag = list(continuous = "densityDiag")) +
  theme_minimal() +
  scale_color_brewer(palette = "Dark2") +
  labs(title = "Pair Plot of K-Prototype Clustering")


## now we create the map of dbscan clusters coloured by kproto cluster

# Define the variables to match on
matching_vars <- c("area_km2", "IMD_Score", "ah3ahah_rnk", "ah3pubs", "RUC11", "pub", "bar", "biergarten", "casino", "nightclub", "stripclub", "restaurant", "alcohol", "wine", "convenience")

# Merge the kproto_cluster into crime_buffer_eps75_NEW based on matching variables
crime_buffer_eps75_NEW_matched <- crime_buffer_eps75_NEW %>%
  left_join(data_for_clustering_unscaled %>% select(all_of(matching_vars), kproto_cluster),
            by = matching_vars)

sum(is.na(crime_buffer_eps75_NEW_matched)) # no nas 


#crime_buffer_eps75 has the geometry of the outlets, so ill make a polygon then extract the centroid
# group by LSOA and cluster, and create polygons
outlet_polygons <- crime_buffer_eps75 %>%
  group_by(LSOA, cluster) %>%
  summarise(geometry = st_combine(geometry)) %>% 
  st_buffer(dist=75) #buffer dist=75 since eps=75
# identify matching observations based on LSOA and cluster
outlet_polygons <- outlet_polygons %>%
  filter(LSOA %in% crime_buffer_eps75_NEW_matched$LSOA21CD &
           cluster %in% crime_buffer_eps75_NEW_matched$cluster)
# perform a left join to keep only the relevant polygons in the matched data
kproto_final_matched <- crime_buffer_eps75_NEW_matched %>%
  left_join(outlet_polygons, by = c("LSOA21CD" = "LSOA", "cluster" = "cluster"))

# ensure kproto_final_matched is an sf object
if (!inherits(kproto_final_matched, "sf")) {
  kproto_final_matched <- st_as_sf(kproto_final_matched)
}

# calculate centroids for each polygon
polygon_centroids <- st_centroid(kproto_final_matched)
head(polygon_centroids)

#checking if these are same crs
st_crs(polygon_centroids)
st_crs(lsoa_shapes)

# plot the LSOA shapes as the base map without borders
ggplot() +
  geom_sf(data = lsoa_shapes, fill = "grey90", color = NA) +  # LSOA shapes with no borders
  geom_sf(data = kproto_final_matched, fill = NA, color = "black", alpha = 0.5) +  # DBSCAN cluster polygons
  geom_sf(data = polygon_centroids, aes(color = as.factor(kproto_cluster)), size = 3) +  # Centroids colored by kproto cluster
  scale_color_manual(values = RColorBrewer::brewer.pal(8, "Set1")) +  # Custom colors for clusters
  theme_minimal() +
  labs(color = "kproto_cluster", title = "DBSCAN Clusters and Centroids on LSOA Map")

