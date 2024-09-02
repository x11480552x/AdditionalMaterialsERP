########################## DBSCAN CODE ##########################

#filtered_lsoa_shapes filters all the lsoa_shapes that do not have no. of outlets of 3 or above
#this is because of our minPts being 3, so they won't be possible to implement DBSCAN on

### function to count outlets within each LSOA and include all LSOAs (for filtered_lsoa_shapes)
count_outlets_per_lsoa <- function(points, lsoa_shapes) {
  # Perform spatial join to find which points are within each LSOA
  points_within <- st_join(points, lsoa_shapes, join = st_intersects)
  
  # Filter out points that are not within any LSOA
  points_within <- points_within %>% filter(!is.na(LSOA21CD))
  
  # Count the number of points in each LSOA
  outlet_count_per_lsoa <- points_within %>%
    group_by(LSOA21NM, LSOA21CD) %>%
    summarise(outlet_count = n()) %>%
    ungroup()
  
  # Get a list of all LSOAs from the lsoa_shapes dataset
  all_lsoas <- lsoa_shapes %>%
    select(LSOA21NM, LSOA21CD) %>%
    st_set_geometry(NULL)  # Remove geometry for the join
  
  # Merge outlet counts with the full list of LSOAs
  all_lsoa_counts <- all_lsoas %>%
    left_join(outlet_count_per_lsoa, by = c("LSOA21NM", "LSOA21CD")) %>%
    mutate(outlet_count = ifelse(is.na(outlet_count), 0, outlet_count))
  
  return(all_lsoa_counts)
}

outlet_counts <- count_outlets_per_lsoa(all_points_unique2.sf, lsoa_shapes)
# Join the outlet counts back to lsoa_shapes to create a new column lsoa.n.outlets
lsoa.n.outlets <- lsoa_shapes %>%
  left_join(outlet_counts, by = c("LSOA21NM", "LSOA21CD"))

# if there are any LSOAs with no outlets, set their count to 0
lsoa.n.outlets$outlet_count <- ifelse(is.na(lsoa_shapes$outlet_count), 0, lsoa_shapes$outlet_count)

filtered_lsoa_shapes <- lsoa.n.outlets %>%
  filter(outlet_count >= 3)

# check the result
head(filtered_lsoa_shapes)


### individual DBSCAN for a few lsoas, create KNN plot and create leaflet map to see the distribution of clusters for a LSOA.

###KNN distplot


library(dplyr)
library(dbscan)
library(ggplot2)
library(sf)

# Function to filter points within a given polygon
filter_points_within_polygon <- function(points, polygon) {
  points_within <- st_join(points, polygon, join = st_intersects)
  points_within <- points_within %>% filter(!is.na(LSOA21CD))
  return(points_within)
}

# define a couple of LSOAs that you want to plot KNN dist plot
selected_lsoa <- c('Birmingham 139A', 'Birmingham 138A', 'Birmingham 138B')

# Extract the coordinates of the outlets for the selected LSOAs
coords_list <- list()

for (lsoa in selected_lsoas) {
  lsoa_polygon <- lsoa_shapes %>%
    filter(LSOA21NM == lsoa)
  outlets <- filter_points_within_polygon(all_points_unique2.sf, lsoa_polygon)
  coords <- st_coordinates(outlets)
  if (nrow(coords) > 4) {  # ensure there are more than 5 points, for minpts=3
    coords_list[[lsoa]] <- coords
  }
}

# check the contents of coords_list
print("Contents of coords_list:")
print(coords_list)

# generate KNN distance plots if coords_list is not empty
if (length(coords_list) > 0) {
  
  for (i in 1:length(coords_list)) {
    coords <- coords_list[[i]]
    if (nrow(coords) > 0) {  # ensure there are points to plot
      kNNdistplot(coords, k = 2)  # using k = 2 for DBSCAN (literature says k = minPts-1 for KNN distplot)
      title(main = paste(names(coords_list)[i]))
      # Customizing the y-axis to have more ticks
      y_ticks <- pretty(range(coords[,2]), n = 4)  # generate pretty y-axis ticks
      axis(2, at = y_ticks, las = 1)  # las = 1 makes the y-axis labels horizontal
    } else {
      plot.new()
      title(main = paste(names(coords_list)[i], "\n(No points)"))
    }
  }
} else {
  print("No LSOAs with sufficient points for plotting.")
}


## generate the map for visualisation of the clusters and buffers
create_leaflet_map_individual <- function(coords, outlet_names, eps, minPts, place, map_title = "Clustered Alcohol Outlets") {
  # perform DBSCAN clustering
  db <- dbscan(coords, eps = eps, minPts = minPts)
  corepts.vec <- is.corepoint(coords, eps = eps, minPts = minPts)
  
  # transform coordinates to WGS84 (EPSG:4326) - this is needed because leaflet doesnt work well with any other CRS than WGS84!
  coords_sf <- st_as_sf(as.data.frame(coords), coords = c("X", "Y"), crs = 27700) #initialise the crs first (necessary)
  coords_4326 <- st_transform(coords_sf, 4326) #then transform
  
  # prepare core points and buffers for each cluster
  buffers <- list()
  for (cluster_num in unique(db$cluster)) {
    if (cluster_num == 0) next  # skip noise points
    
    core_points <- coords[corepts.vec & db$cluster == cluster_num, , drop = FALSE]
    if (nrow(core_points) == 1) {
      # single core point case
      point <- st_point(core_points[1, ])
      buffer <- st_buffer(st_sfc(point, crs = 27700), dist = eps)
    } else {
      # multiple core points case
      multipoint <- st_multipoint(as.matrix(core_points))
      buffer <- st_buffer(st_sfc(multipoint, crs = 27700), dist = eps)
    }
    buffer <- st_transform(buffer, 4326)
    buffers[[as.character(cluster_num)]] <- buffer
  }
  
  # prepare data frame for leaflet markers
  db_frame <- data.frame(cluster = db$cluster)
  coords_4326 <- cbind(coords_4326, db_frame)
  
  # define a color palette for clusters
  cluster_colors <- colorFactor(rainbow(length(unique(coords_4326$cluster))), 
                                coords_4326$cluster)
  
  # create the title and subtitle
  title_html <- paste0("<h2>", map_title, " in ", place, "</h2>")
  subtitle_html <- paste0("<h4>MinPts: ", minPts, ", Epsilon: ", eps, "</h4>")
  
  # create and return the leaflet map
  map <- leaflet() %>%
    addTiles()  # add default OpenStreetMap tiles
  
  for (buffer in buffers) {
    map <- map %>% addPolygons(data = buffer, color = "black", weight = 1)
  }
  
  map <- map %>%
    addCircleMarkers(data = coords_4326, 
                     color = ~cluster_colors(cluster), radius = 1, fillOpacity = 1,
                     popup = ~paste0("Cluster: ", cluster, "<br>Name: ", name)) %>%
    addLegend(pal = cluster_colors, values = coords_4326$cluster, opacity = 1,
              title = "Cluster Number", position = "bottomright") %>%
    addControl(html = subtitle_html, position = "topright")
  
  return(map)
}

#example: creating the leaflet map for Birmingham LSOAS (figures 9 & 10)
LSOAs_Birmingham <- c('Birmingham 139A', 'Birmingham 138A', 'Birmingham 138B')
lsoa_B <- lsoa_shapes %>%
  filter(LSOA21NM %in% LSOAs_Birmingham)
B_outlets <- filter_points_within_polygon(all_points_unique2.sf, lsoa_B)
coords_B <- st_coordinates(B_outlets)
outletnames_B <- B_outlets$name
resultB <-  create_leaflet_map_individual(coords_B, outletnames_B, eps=100, minPts = 3, place = "Birmingham City Centre Area")
#look at the leaflet map
resultB
# do the same with others examples. :)


###main function to perform DBSCAN per LSOA 
# function to apply DBSCAN per LSOA and return cluster data
dbscan_per_lsoa <- function(lsoa_shape, points, eps, minPts) {
  lsoa_id <- lsoa_shape$LSOA21CD
  filtered_points <- filter_points_within_polygon(points, lsoa_shape)
  if (nrow(filtered_points) < minPts) {
    return(data.frame(X = numeric(0), Y = numeric(0), cluster = integer(0), LSOA = lsoa_id))
  }
  
  coords <- st_coordinates(filtered_points)
  db <- dbscan(coords, eps = eps, minPts = minPts)
  clusters <- db$cluster
  
  cluster_data <- data.frame(X = coords[,1], Y = coords[,2], cluster = clusters, LSOA = lsoa_id)
  return(cluster_data)
}

# Parallel processing setup
num_cores <- 39  # Number of cores available
cl <- makeCluster(num_cores)
clusterEvalQ(cl, {
  library(sf)
  library(dplyr)
  library(dbscan)
})
clusterExport(cl, c("filter_points_within_polygon", "dbscan_per_lsoa", "all_points_unique2.sf", "filtered_lsoa_shapes"))

# Function to safely perform DBSCAN and handle errors
safe_dbscan_per_lsoa <- function(i) {
  lsoa_shape <- filtered_lsoa_shapes[i, ]
  tryCatch({
    dbscan_per_lsoa(lsoa_shape, all_points_unique2.sf, eps = 75, minPts = 3)  # Adjust eps and minPts as needed
  }, error = function(e) {
    return(data.frame(X = numeric(0), Y = numeric(0), cluster = integer(0), LSOA = lsoa_shape$LSOA21CD))
  })
}

# Perform DBSCAN clustering for each LSOA in parallel
results <- parLapply(cl, 1:nrow(filtered_lsoa_shapes), safe_dbscan_per_lsoa)

# Stop the cluster
stopCluster(cl)


#### this part of the code extracts the crime for each DBSCAN buffer for each (possible) LSOA 
# Define the main function to create the buffer for each cluster, then count the crimes within the buffer
analyze_lsoa <- function(coords, crime_data, eps, minPts) {
  # ensure coordinates are numeric
  coords <- as.data.frame(coords)
  coords$X <- as.numeric(coords$X)
  coords$Y <- as.numeric(coords$Y)
  
  # perform DBSCAN clustering on alcohol outlets
  db <- dbscan(coords[, c('X', 'Y')], eps = eps, minPts = minPts)
  corepts.vec <- is.corepoint(coords[, c('X', 'Y')], eps = eps, minPts = minPts)
  
  # prepare core points and buffers for each cluster
  buffers <- list()
  valid_clusters <- unique(db$cluster[db$cluster != 0])
  if(length(valid_clusters) == 0) return(NULL) # no valid clusters, skip this LSOA
  
  for (cluster_num in valid_clusters) {
    core_points <- coords[corepts.vec & db$cluster == cluster_num, , drop = FALSE]
    if (nrow(core_points) == 1) {
      # single core point case
      point <- st_point(as.numeric(core_points[1, c('X', 'Y')]))
      buffer <- st_buffer(st_sfc(point, crs = 27700), dist = eps)
    } else {
      # multiple core points case
      multipoint <- st_multipoint(as.matrix(core_points[, c('X', 'Y')]))
      buffer <- st_buffer(st_sfc(multipoint, crs = 27700), dist = eps)
    }
    # calculate buffer area in EPSG:27700 and convert to square kilometers
    buffer_area_km2 <- as.numeric(st_area(buffer)) / 1e6  # Area in square kilometers
    buffer <- st_transform(buffer, 4326)  # Transform buffer to CRS 4326
    buffers[[as.character(cluster_num)]] <- list(buffer = buffer, area_km2 = buffer_area_km2)
  }
  
  # transform crime data coordinates to WGS 84 (EPSG:4326)
  crime_data_sf <- st_as_sf(crime_data, coords = c('Longitude', 'Latitude'), crs = 4326)
  
  # count crimes within each buffer and use pre-calculated buffer areas
  crime_counts_list <- lapply(buffers, function(buffer_info) {
    buffer <- buffer_info$buffer
    buffer_area_km2 <- buffer_info$area_km2
    crimes_within <- st_within(crime_data_sf, buffer, sparse = FALSE)
    total_crimes <- sum(crimes_within)
    crimes_by_type <- crime_data %>%
      filter(st_within(st_as_sf(., coords = c('Longitude', 'Latitude'), crs = 4326), buffer, sparse = FALSE)[, 1]) %>%
      group_by(Crime.type) %>%
      summarise(count = n(), .groups = 'drop')
    list(total = total_crimes, by_type = crimes_by_type, area_km2 = buffer_area_km2)
  })
  
  # prepare results in a data frame
  result <- data.frame(cluster = names(buffers), 
                       total_crimes = sapply(crime_counts_list, `[[`, "total"),
                       area_km2 = sapply(crime_counts_list, `[[`, "area_km2"))
  for(crime_type in unique(crime_data$Crime.type)) {
    result[[crime_type]] <- sapply(crime_counts_list, function(x) {
      if(crime_type %in% x$by_type$Crime.type) {
        return(x$by_type$count[x$by_type$Crime.type == crime_type])
      } else {
        return(0)
      }
    })
  }
  
  return(result)
}



# function to process each LSOA and its corresponding crime data
process_lsoa <- function(i, results, crime_data_list, eps, minPts) { #results = dbscan results from the parallelised function
  if (is.null(results2[[i]]) || is.null(crime_data_list[[i]])) {
    return(NULL)
  }
  
  coords <- results[[i]][, c('X', 'Y')]
  crime_data <- crime_data_list[[i]]
  
  # ensure there are valid coordinates and crime data
  if (nrow(coords) == 0 || nrow(crime_data) == 0) {
    return(NULL)
  }
  
  lsoa_result <- analyze_lsoa(coords, crime_data, eps, minPts)
  if (!is.null(lsoa_result)) {
    lsoa_id <- unique(results[[i]]$LSOA)
    return(list(lsoa_id = lsoa_id, result = lsoa_result))
  }
  return(NULL)
}


# defining dbscan params and cores for parallelisation
eps <- 125
minPts <- 3
num_cores <- detectCores() - 1

# parallel processing setup (libraries, functions and datasets)
cl <- makeCluster(num_cores)
clusterEvalQ(cl, {
  library(dbscan)
  library(sf)
  library(dplyr)
})
clusterExport(cl, c("resultseps125", "crime_data_list", "eps", "minPts", "analyze_lsoa", "process_lsoa")) #change resultseps125 to the eps=75 results

# parallel execution for eps = 125 
results_crime_eps125 <- parLapply(cl, seq_along(resultseps125), process_lsoa, resultseps125, crime_data_list, eps, minPts)

stopCluster(cl)

#this would be the same for crime_buffer_eps_75, we call it crime_buffer_eps_75_2 to merge it later for kprototypes
#crime_buffer_eps_75_2 <- parLapply(cl, seq_along(resultseps125), process_lsoa, resultseps125, crime_data_list, , minPts)

#results for eps=125
# remove null/na results and convert to named list
results_crime_eps125 <- Filter(Negate(is.null), results_crime_eps125)
results_crime_eps125 <- setNames(lapply(results_crime_eps125, `[[`, "result"), sapply(results_crime_eps125, `[[`, "lsoa_id"))
#checking
head(results_crime_eps125[[1]])

#input/results for eps = 75 would be the same, youd just use resultseps75 instead of resultseps125

# combine the list into a single sf object
results_combined <- do.call(rbind, results) #results is eps=75

# filter out noise (cluster = 0)
results_filtered <- results_combined %>% filter(cluster != 0)
results_filtered_sf <- st_as_sf(results_filtered, coords = c("X", "Y"), crs = 27700) 

# create buffer polygons around each cluster to define their area
cluster_polygons <- results_filtered_sf %>%
  group_by(LSOA, cluster) %>%
  summarize(geometry = st_union(st_buffer(geometry, dist = 75))) %>%
  st_as_sf()

# calculate centroids of each cluster polygon
cluster_centroids <- cluster_polygons %>%
  st_centroid() %>%
  mutate(cluster_id = row_number())  # Create a unique cluster identifier

# transform to WGS84 for mapping with Leaflet
cluster_centroids_wgs84 <- st_transform(cluster_centroids, crs = 4326)
cluster_polygons_wgs84 <- st_transform(cluster_polygons, crs = 4326)
results_filtered_wgs84 <- st_transform(results_filtered_sf, crs = 4326)

# assign colors to different clusters for points visualization
point_colors <- colorFactor(rainbow(length(unique(results_filtered$cluster))), results_filtered$cluster)

# create the leaflet map with marker clustering and buffers (interactive map for the video!!)
leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap tiles
  addCircleMarkers(
    data = cluster_centroids_wgs84,
    radius = 5,  # Fixed radius for centroids
    fillColor = "blue",  # Default fill color for centroids
    fillOpacity = 0.6,  # Marker fill opacity
    stroke = FALSE,  # No border stroke
    clusterOptions = markerClusterOptions(),  # Enable marker clustering
    label = ~paste("Cluster ID:", cluster_id),  # Tooltip showing the cluster ID
    labelOptions = labelOptions(
      direction = "auto"
    ),
    group = "Centroids"  # Assign to 'Centroids' layer group
  ) %>%
  # Add points within each cluster
  addCircleMarkers(
    data = results_filtered_wgs84,
    color = ~point_colors(cluster),
    radius = 3,
    fillOpacity = 0.8,
    stroke = FALSE,
    label = ~paste("Cluster ID:", cluster),  # Tooltip showing the cluster number
    labelOptions = labelOptions(
      direction = "auto"
    ),
    group = "Points"  # Assign to 'Points' layer group
  ) %>%
  # add a group for buffers to show when zoomed in
  addPolygons(
    data = cluster_polygons_wgs84,
    color = "black",  # black border for the buffer polygons
    weight = 1,
    fillOpacity = 0.2,
    group = "Buffers"  # assign to 'Buffers' layer group
  ) %>%
  # add layer control
  addLayersControl(
    overlayGroups = c("Centroids", "Points", "Buffers"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  addProviderTiles(providers$Stamen.Toner)


#static maps
# spatial join to count clusters per LAD
clusters_per_LAD <- st_join(cluster_centroids, LAD_shapes %>% select(LAD22CD, geometry)) %>%
  group_by(LAD22CD) %>%
  summarize(cluster_count = n())

# convert clusters_per_LAD to a data frame for non-spatial join
clusters_per_LAD_df <- st_drop_geometry(clusters_per_LAD)

# merge cluster counts with LAD shapes using a non-spatial left_join
LAD_shapes_with_cluster_counts <- LAD_shapes %>%
  left_join(clusters_per_LAD_df, by = "LAD22CD")

# replace NAs in cluster_count with zero
LAD_shapes_with_cluster_counts$cluster_count[is.na(LAD_shapes_with_cluster_counts$cluster_count)] <- 0

# define breaks for cluster counts
breaks <- c(-Inf,1,2, 4, 8, 12, 20, 40, 60, Inf)
labels <- c("0","1","2-4" ,"5-8", "9-12", "13-20", "21-40", "41-60", "61+")

# cut the cluster counts into categories
LAD_shapes_with_cluster_counts$cluster_category <- cut(
  LAD_shapes_with_cluster_counts$cluster_count, 
  breaks = breaks, 
  labels = labels, 
  right = FALSE
)

# plot with ggplot2 using discrete categories and viridis palette
england_map_cluster_fill <- ggplot() +
  geom_sf(data = LAD_shapes_with_cluster_counts, aes(fill = cluster_category), color = "white", size = 0.2) +  # Cluster count with categories
  scale_fill_viridis_d(option = "viridis", name = "Cluster Count", na.value = "lightgrey", direction = -1) +  # Use viridis palette with discrete categories
  theme_minimal() +
  labs(
    title = "Number of DBSCAN Clusters by LAD in England",
    subtitle = "eps = 75, minPts = 3",
    x = NULL,  # Remove x-axis label
    y = NULL   # Remove y-axis label
  ) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

# Show the plot
print(england_map_cluster_fill)

#save for report
# Save the plot as a PNG file
ggsave("england_map_clusters_eps75.png", plot = england_map_cluster_fill, width = 12, height = 10, dpi = 300)

# save for github
ggsave("england_map_clusters_eps75.pdf", plot = england_map_cluster_fill, width = 10, height = 8)


#now plot RUC,IMD, AHAH by LAD for comparison

# using gsub to remove any trailing numeric codes after a space
AHAH_aggregated <- AHAH_aggregated %>%
  mutate(LAD_name = gsub("\\s\\d+\\w+$", "", LSOA21NM))

# check the result
head(AHAH_aggregated)

# calculate the average ah3ahah_rnk for each LAD_name
average_ah3ahah_by_LAD <- AHAH_aggregated %>%
  group_by(LAD_name) %>%
  summarize(average_ah3ahah = mean(ah3ahah_rnk, na.rm = TRUE))

# first, add LAD_name to the imd2019_aggregated dataset if not already done
imd2019_aggregated <- imd2019_aggregated %>%
  mutate(LAD_name = gsub("\\s\\d+\\w+$", "", LSOA21NM))

# calculate the average IMD score for each LAD_name
average_imd_score_by_LAD <- imd2019_aggregated %>%
  group_by(LAD_name) %>%
  summarize(average_imd_score = mean(`Index.of.Multiple.Deprivation.(IMD).Score`, na.rm = TRUE))

# check the result
head(average_imd_score_by_LAD)

#RUC
head(LADRUC)

#change name so the join is correct
# rename specific LADs in LAD_shapes_with_county
LAD_shapes <- LAD_shapes %>%
  mutate(LAD22NM = case_when(
    LAD22NM == "Kingston upon Hull, City of" ~ "Kingston upon Hull",
    LAD22NM == "Herefordshire, County of" ~ "Herefordshire",
    LAD22NM == "Bristol, City of" ~ "Bristol",
    TRUE ~ LAD22NM  # keep other names unchanged
  ))

# join AHAH scores
LAD_shapes <- LAD_shapes %>%
  left_join(average_ah3ahah_by_LAD, by = c("LAD22NM" = "LAD_name"))

# join IMD scores
LAD_shapes <- LAD_shapes %>%
  left_join(average_imd_score_by_LAD, by = c("LAD22NM" = "LAD_name"))

# join RUC levels
LAD_shapes <- LAD_shapes %>%
  left_join(LADRUC_updated %>% select(LAD22NM = lad22nm, RUC11), by = "LAD22NM")


england_map_ruc <- ggplot() +
  geom_sf(data = LAD_shapes, aes(fill = factor(RUC11)), color = "white", size = 0.2) +  # RUC using categorical viridis fill
  scale_fill_viridis_d(option = "viridis") +  # Use viridis palette for discrete values
  theme_minimal() +
  labs(
    title = "RUC in England (by LAD)",
    fill = "RUC",
    x = NULL,
    y = NULL
  ) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

england_map_ruc
# Save the RUC map
ggsave("england_map_ruc.png", plot = england_map_ruc, width = 12, height = 10, dpi = 300)
ggsave("england_map_ruc.pdf", plot = england_map_ruc, width = 10, height = 8)

# AHAH 
# plot with ggplot2 using magma palette for AHAH
england_map_ahah <- ggplot() +
  geom_sf(data = LAD_shapes, aes(fill = average_ah3ahah), color = "white", size = 0.2) +  
  scale_fill_viridis_c(option = "viridis", name = "Average AHAH Score") +  
  theme_minimal() +
  labs(
    title = "Average AHAH Score in England (by LAD)",
    x = NULL,  # remove x-axis label
    y = NULL   # remove y-axis label
  ) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

england_map_ahah
# save the AHAH map
ggsave("england_map_ahah.png", plot = england_map_ahah, width = 12, height = 10, dpi = 300)
ggsave("england_map_ahah.pdf", plot = england_map_ahah, width = 10, height = 8)


#IMD
# plot with ggplot2 using viridis palette for IMD
england_map_imd <- ggplot() +
  geom_sf(data = LAD_shapes, aes(fill = average_imd_score), color = "white", size = 0.2) +  # IMD using viridis fill
  scale_fill_viridis_c(option = "viridis", name = "Average IMD Score") +  # Use viridis palette for fill
  theme_minimal() +
  labs(
    title = "Average IMD Score in England (by LAD)",
    x = NULL,  # Remove x-axis label
    y = NULL   # Remove y-axis label
  ) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

england_map_imd

# save the IMD map
ggsave("england_map_imd.png", plot = england_map_imd, width = 12, height = 10, dpi = 300)
ggsave("england_map_imd.pdf", plot = england_map_imd, width = 10, height = 8)

#eps =125
# combine the list into a single sf object
results_combined2 <- do.call(rbind, resultseps125)

# filter out noise (cluster = 0)
results_filtered2 <- results_combined2 %>% filter(cluster != 0)
results_filtered_sf2 <- st_as_sf(results_filtered2, coords = c("X", "Y"), crs = 27700) 

# create buffer polygons around each cluster to define their area
cluster_polygons2 <- results_filtered_sf2 %>%
  group_by(LSOA, cluster) %>%
  summarize(geometry = st_union(st_buffer(geometry, dist = 125))) %>%
  st_as_sf()

# calculate centroids of each cluster polygon
cluster_centroids2 <- cluster_polygons2 %>%
  st_centroid() %>%
  mutate(cluster_id = row_number())  # Create a unique cluster identifier

#static maps
# spatial join to count clusters per LAD (in this case we have to use st_nearest_feature as the centroids seem to overlap into different LADs)
clusters_per_LAD2 <- st_join(cluster_centroids2, LAD_shapes %>% select(LAD22CD, geometry), join=st_nearest_feature) %>%
  group_by(LAD22CD) %>%
  summarize(cluster_count = n())

# convert clusters_per_LAD to a data frame for non-spatial join
clusters_per_LAD_df2 <- st_drop_geometry(clusters_per_LAD2)

# merge cluster counts with LAD shapes using a non-spatial left_join
LAD_shapes_with_cluster_counts2 <- LAD_shapes %>%
  left_join(clusters_per_LAD_df2, by = "LAD22CD")

# replace NAs in cluster_count with zero
LAD_shapes_with_cluster_counts2$cluster_count[is.na(LAD_shapes_with_cluster_counts2$cluster_count)] <- 0
summary(LAD_shapes_with_cluster_counts2$cluster_count)
# define breaks for cluster counts
breaks <- c(-Inf,1,2, 4, 8, 12, 20, 40, 60, Inf)
labels <- c("0","1","2-4" ,"5-8", "9-12", "13-20", "21-40", "41-60", "61+")

# cut the cluster counts into categories
LAD_shapes_with_cluster_counts2$cluster_category <- cut(
  LAD_shapes_with_cluster_counts2$cluster_count, 
  breaks = breaks, 
  labels = labels, 
  right = FALSE
)

# plot with ggplot2 using discrete categories and viridis palette
england_map_cluster_fill2 <- ggplot() +
  geom_sf(data = LAD_shapes_with_cluster_counts2, aes(fill = cluster_category), color = "white", size = 0.2) +  # Cluster count with categories
  scale_fill_viridis_d(option = "viridis", name = "Cluster Count", na.value = "lightgrey", direction = -1) +  # Use viridis palette with discrete categories
  theme_minimal() +
  labs(
    title = "Number of DBSCAN Clusters by LAD in England",
    subtitle = "eps = 125, minPts = 3",
    x = NULL,  # remove x-axis label
    y = NULL   # remove y-axis label
  ) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

# show the plot
print(england_map_cluster_fill2)

#save for report (png)
ggsave("england_map_clusters_eps125.png", plot = england_map_cluster_fill2, width = 12, height = 10, dpi = 300)

# save as pdf
ggsave("england_map_clusters_eps75.pdf", plot = england_map_cluster_fill, width = 10, height = 8)

