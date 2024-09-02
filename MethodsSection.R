############################# CODE USED IN METHODS SECTION ############################# 

### bounding box representation (figure 1)

#bbox_england is already defined in Preamble.R so we use this

#load shapefile for the UK and nearby regions
uk_shape <- ne_countries(scale = "medium", returnclass = "sf", continent = "Europe")

# Convert the matrix from getbb() into an sf object for plotting
# st_as_sfc expects a named vector or a bounding box object, not a matrix
bounding_box_sf <- st_as_sfc(st_bbox(c(xmin = bbox_england[1, 1], ymin = bbox_england[2, 1], 
                                       xmax = bbox_england[1, 2], ymax = bbox_england[2, 2]), 
                                     crs = st_crs(uk_shape))) #st_as_sfc() expects a bounding box or a similar object that can be converted directly into a sf object

#plot map using ggplot2
ggplot() +
  geom_sf(data = uk_shape, fill = "lightblue") +  # Plot UK shapefile
  geom_sf(data = bounding_box_sf, color = "red", fill = NA, size = 1) +  # Plot the bounding box
  coord_sf(xlim = c(bbox_england[1, 1], bbox_england[1, 2]), ylim = c(bbox_england[2, 1], bbox_england[2, 2]), expand = FALSE) +  # set limits to focus on the bounding box
  labs(title = "Bounding Box in Relation to UK, Ireland, and Nearby Regions") +
  theme_minimal()

### table of percentages of RUC

# calculate counts and percentages
ruc11_percentages <- RUC_aggregated %>%
  group_by(RUC11) %>%
  summarize(
    Count = n(),  # Count the number of occurrences
    Percentage = (n() / nrow(RUC_aggregated)) * 100  # Calculate the percentage
  )

print(ruc11_percentages)

### DBSCAN quick representation to help understand

set.seed(42)
data <- data.frame(
  X = c(0.5, 2, 2.5, 3, 4.75, 5, 5.5,6),
  Y = c(2, 2, 2.25, 2, 1, 1.5, 1.25,2)
)

# perform DBSCAN clustering
eps <- 1
minPts <- 3
db <- dbscan(data, eps = eps, minPts = minPts)
db
core_points <- is.corepoint(data, eps = eps, minPts = minPts)
data$type <- ifelse(db$cluster == 0, "Noise", ifelse(core_points, "Core", "Density Reachable"))
data$cluster <- factor(db$cluster)


# define different epsilon neighbourhood circle for dens reachable so it has a different colour
eps_dens <- data %>%
  filter(type == "Density Reachable") %>%
  rowwise() %>%
  do(data.frame(x0 = .$X, y0 = .$Y , x = .$X, y = .$Y))


#define epsilon neighborhood circles for core pts
epsilon_circles <- data %>%
  filter(type == "Core") %>%
  rowwise() %>%
  do(data.frame(x0 = .$X, y0 = .$Y , x = .$X, y = .$Y))
#same for noise
epsilon_circles_noise <- data %>%
  filter(type == "Noise") %>%
  rowwise() %>%
  do(data.frame(x0 = .$X, y0 = .$Y , x = .$X, y = .$Y))

# color palette
colors <- c("0" = "black", "1" = "darkgreen", "2" = "navyblue")

#plotting 
ggplot(data) +
  geom_point(aes(x = X, y = Y, color = cluster), size = 10) +
  geom_circle(aes(x0 = x0, y0 = y0, r = eps), data = epsilon_circles, linetype = "dashed", color = "gray") +
  geom_circle(aes(x0 = x0, y0 = y0, r = eps), data = epsilon_circles_noise, linetype = "solid", color = "red") +
  geom_circle(aes(x0 = x0, y0 = y0, r = eps), data = eps_dens, linetype = "solid", color = "purple") +
  theme_void() +
  scale_color_manual(values = colors) +
  coord_fixed(ratio = 1)  +
  theme(legend.position = "none")

### convex hull v buffer (shropshire area as an example)
#change minPts and eps as you wish to get a good visualisation of whats happening 
# Assuming lsoa_shapes is your dataset containing all LSOA geometries and their names
# Filter to find the specific LSOA in Shropshire
lsoa_sh <- lsoa_shapes %>%
  filter(LSOA21NM == 'Shropshire 019G')

#need to define this function, filter_points_within_polygon
# it filters the outlets based on the name of the LSOA you give it, and gets rid of any NAs in LSOA21CD
filter_points_within_polygon <- function(points, polygon) {
  points_within <- st_join(points, polygon, join = st_intersects)
  points_within <- points_within %>% filter(!is.na(LSOA21CD))
  return(points_within)
}

# apply the function
sh_outlets <- filter_points_within_polygon(all_points_unique2.sf, lsoa_sh)

# Get coordinates and outlet names
coords_sh <- st_coordinates(sh_outlets)

# perform DBSCAN (can change these values!!)
eps_value <- 75
minPts_value <- 3
db <- dbscan(coords_sh, eps = eps_value, minPts = minPts_value)
corepts.vec <- is.corepoint(coords_sh, eps = eps_value, minPts = minPts_value)

# create buffers and convex hulls for each cluster
buffers <- list()
convex_hulls <- list()

for (cluster_num in unique(db$cluster)) {
  if (cluster_num == 0) next  # skip noise points
  
  core_points <- coords_sh[corepts.vec & db$cluster == cluster_num, , drop = FALSE]
  
  if (nrow(core_points) == 1) {
    # Single core point case
    point <- st_point(core_points[1, ])
    buffer <- st_buffer(st_sfc(point, crs = 27700), dist = eps_value)
    convex_hull <- buffer  # convex hull is the same as buffer for single point
  } else {
    # multiple core points case
    multipoint <- st_multipoint(as.matrix(core_points))
    buffer <- st_buffer(st_sfc(multipoint, crs = 27700), dist = eps_value)
    convex_hull <- st_convex_hull(st_sfc(multipoint, crs = 27700))
  }
  
  # transform to WGS 84 for visualisation (this is always necessary for plotting in leaflet)
  buffer <- st_transform(buffer, 4326)
  convex_hull <- st_transform(convex_hull, 4326)
  
  # store results
  buffers[[as.character(cluster_num)]] <- buffer
  convex_hulls[[as.character(cluster_num)]] <- convex_hull
}

# prepare data frame for leaflet map
db_frame <- data.frame(cluster = db$cluster)
coords_4326 <- st_as_sf(as.data.frame(coords_sh), coords = c("X", "Y"), crs = 27700)
coords_4326 <- st_transform(coords_4326, 4326)
coords_4326 <- cbind(coords_4326, db_frame)

# Define a color palette for clusters
cluster_colors <- colorFactor(rainbow(length(unique(coords_4326$cluster))), coords_4326$cluster)

# Create and return the leaflet map for buffer example without popups or legend
map_buffer <- leaflet() %>%
  addTiles()  # Add default OpenStreetMap tiles

for (buffer in buffers) {
  map_buffer <- map_buffer %>% addPolygons(data = buffer, color = "black", weight = 1)
}

map_buffer <- map_buffer %>%
  addCircleMarkers(data = coords_4326, 
                   color = ~cluster_colors(cluster), radius = 1, fillOpacity = 1)

# Create and return the leaflet map for convex hull example without popups or legend
map_convex_hull <- leaflet() %>%
  addTiles()  # Add default OpenStreetMap tiles

for (hull in convex_hulls) {
  map_convex_hull <- map_convex_hull %>% addPolygons(data = hull, color = "black", weight = 1)
}

map_convex_hull <- map_convex_hull %>%
  addCircleMarkers(data = coords_4326, 
                   color = ~cluster_colors(cluster), radius = 1, fillOpacity = 1)

# Display the maps
map_buffer
map_convex_hull


