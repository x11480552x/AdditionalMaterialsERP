########################### CLEANING ALCOHOL OUTLETS ###########################

# outlets were classified in geometry as points, polygons or multipolygons. We need all of them as points for easier manipulation
# restaurants were cleaned more heavily as we couldnt say for certain if they sold alcohol

# function to clean and transform data with multipolygons
clean_and_transform <- function(data, target_crs, amenity_values = NULL, shop_values = NULL) {
  if (!is.null(amenity_values)) {
    data$osm_points <- data$osm_points %>% 
      filter(!is.na(amenity) & amenity %in% amenity_values) %>% # filters out NAs and other amenities we don't want
      st_transform(crs = target_crs)
    
    data$osm_polygons <- data$osm_polygons %>% 
      filter(!is.na(amenity) & amenity %in% amenity_values) %>%
      st_transform(crs = target_crs)
    
    data$osm_multipolygons <- data$osm_multipolygons %>% 
      filter(!is.na(amenity) & amenity %in% amenity_values) %>%
      st_transform(crs = target_crs)
  }
  
  if (!is.null(shop_values)) {
    data$osm_points <- data$osm_points %>% 
      filter(!is.na(shop) & shop %in% shop_values) %>% #same applies to the shop values: convenience, alcohol & wine
      st_transform(crs = target_crs)
    
    data$osm_polygons <- data$osm_polygons %>% 
      filter(!is.na(shop) & shop %in% shop_values) %>%
      st_transform(crs = target_crs)
    
    data$osm_multipolygons <- data$osm_multipolygons %>% 
      filter(!is.na(shop) & shop %in% shop_values) %>%
      st_transform(crs = target_crs)
  }
  
  return(data)
}

# function to clean and transform data without multipolygons (some outlet data do not have multipolygons)
clean_and_transform2 <- function(data, target_crs, amenity_values = NULL, shop_values = NULL) {
  if (!is.null(amenity_values)) {
    data$osm_points <- data$osm_points %>% 
      filter(!is.na(amenity) & amenity %in% amenity_values) %>%
      st_transform(crs = target_crs)
    
    data$osm_polygons <- data$osm_polygons %>% 
      filter(!is.na(amenity) & amenity %in% amenity_values) %>%
      st_transform(crs = target_crs)
  }
  
  if (!is.null(shop_values)) {
    data$osm_points <- data$osm_points %>% 
      filter(!is.na(shop) & shop %in% shop_values) %>%
      st_transform(crs = target_crs)
    
    data$osm_polygons <- data$osm_polygons %>% 
      filter(!is.na(shop) & shop %in% shop_values) %>%
      st_transform(crs = target_crs)
  }
  
  return(data)
}

# we want CRS = british national grid 
crs_bng <- st_crs(27700)

#define amenity and shop values
amenities <- c("bar", "pub", "nightclub", "biergarten", "casino", "stripclub")
shops <- c("alcohol", "wine", "convenience", "restaurant")

#apply the functions to clean and transform data
Englandbars.sf <- clean_and_transform(Englandbars.sf, crs_bng, amenity_values = c("bar"))
Englandclubs.sf <- clean_and_transform(Englandclubs.sf, crs_bng, amenity_values = c("nightclub"))
Englandpubs.sf <- clean_and_transform(Englandpubs.sf, crs_bng, amenity_values = c("pub"))
Englandbiergarten.sf <- clean_and_transform(Englandbiergarten.sf, crs_bng, amenity_values = c("biergarten"))
Englandcasinos.sf <- clean_and_transform2(Englandcasinos.sf, crs_bng, amenity_values = c("casino"))
Englandstrips.sf <- clean_and_transform2(Englandstrips.sf, crs_bng, amenity_values = c("stripclub"))
EnglandAlcshop.sf <- clean_and_transform(EnglandAlcshop.sf, crs_bng, shop_values = c("alcohol"))
EnglandWineshop.sf <- clean_and_transform(EnglandWineshop.sf, crs_bng, shop_values = c("wine"))
EnglandConven.sf <- clean_and_transform(EnglandConven.sf, crs_bng, shop_values = c("convenience"))


#function to clean and transform restaurant data
clean_restaurant_data <- function(data, target_crs, columns_to_check) {
  filter_fn <- function(df, columns) {
    existing_columns <- columns[columns %in% colnames(df)]
    if (length(existing_columns) == 0) {
      stop("No valid columns found for filtering")
    }
    df %>%
      filter(if_all(all_of(existing_columns), ~ . %in% c("no", "maybe") | is.na(.)) == FALSE) %>% 
      st_transform(crs = target_crs)
  }
  
  if ("osm_points" %in% names(data)) {
    data$osm_points <- filter_fn(data$osm_points, columns_to_check)
  }
  
  if ("osm_polygons" %in% names(data)) {
    data$osm_polygons <- filter_fn(data$osm_polygons, columns_to_check)
  }
  
  if ("osm_multipolygons" %in% names(data)) {
    data$osm_multipolygons <- filter_fn(data$osm_multipolygons, columns_to_check)
  }
  
  return(data)
}

# define the columns to check
columns_to_check <- c("bar", "cocktails", "alcohol", "brewery", "drink:beer", "drink:wine", "microbrewery", "real_ale", "real_cider")
# if they had any yes's in these defined cols, they can be added to our overall outlets

# apply the function to clean and transform restaurant data
EnglandRestaurants.sf <- clean_restaurant_data(EnglandRestaurants.sf, crs_bng, columns_to_check)
EnglandRestaurants.sf <- clean_and_transform(EnglandRestaurants.sf, crs_bng, amenity_values = c("restaurant"))


# extract points, and the polygons, multipolygons as centroids (this is the way to get those outlets as points!)
extract_points_centroids <- function(osmdata_sf) {
  points <- osmdata_sf$osm_points
  polygons_centroids <- st_centroid(osmdata_sf$osm_polygons)
  multipolygons_centroids <- st_centroid(osmdata_sf$osm_multipolygons)
  
  return(bind_rows(points, polygons_centroids, multipolygons_centroids))
}

# extract points and centroids (for non-multipolygon data)
extract_points_centroids2 <- function(osmdata_sf) {
  points <- osmdata_sf$osm_points
  polygons_centroids <- st_centroid(osmdata_sf$osm_polygons)
  
  return(bind_rows(points, polygons_centroids))
}

bars_points <- extract_points_centroids(Englandbars.sf)
pubs_points <- extract_points_centroids(Englandpubs.sf)
clubs_points <- extract_points_centroids(Englandclubs.sf)
bier_points <- extract_points_centroids(Englandbiergarten.sf)
casino_points <- extract_points_centroids2(Englandcasinos.sf)
strip_points <- extract_points_centroids2(Englandstrips.sf)
restaur_points <- extract_points_centroids(EnglandRestaurants.sf)
convenience_points <- extract_points_centroids2(EnglandConven.sf)
alcshop_points <- extract_points_centroids(EnglandAlcshop.sf)
wineshop_points <- extract_points_centroids(EnglandWineshop.sf)

# bind all datasets into one
all_points <- bind_rows(
  bars_points,
  pubs_points,
  clubs_points,
  bier_points,
  casino_points,
  strip_points,
  restaur_points,
  convenience_points,
  alcshop_points,
  wineshop_points
)

# now we look for duplicates in geometry
duplicates <- all_points[duplicated(all_points$geometry), ] # 45 duplicates 
print(duplicates)
#delete these from the dataset
all_points_unique.sf <- all_points[!duplicated(all_points$geometry), ] #79899 observations as of april 2024 - subject to change when retrieving data directly from osm

#there are some other conditions specified below, as the retrieval process from osm also captured some different shops or amenities that we don't need
# initialise an empty list to store results
filtered_data_list <- list()
spec_cols <- all_points_unique.sf[, c('osm_id','name','amenity', 'shop', 'sells:alcohol', 'drink:liquor', 'drink:vodka', 'liquor')]

# Define conditions
conditions <- list(
  fast_food_convenience = (spec_cols$amenity == 'fast_food') & (spec_cols$shop == 'convenience'),
  social_facility = (spec_cols$amenity == 'social_facility'),
  disused = (spec_cols$amenity == 'disused'),
  cafecon = (spec_cols$amenity == 'cafe' & spec_cols$shop == 'convenience'),
  community_centre = (spec_cols$shop == 'community_centre'),
  money_transfer_convenience = (spec_cols$amenity == 'money_transfer' & spec_cols$shop == 'convenience'),
  pharmacy_name = grepl('Pharmacy', spec_cols$name, ignore.case = TRUE),
  post_office_box = (spec_cols$shop == 'post_office;post_box'),
  bakery = (spec_cols$shop == 'bakery'),
  tattoo_shop = (spec_cols$shop == 'tattoo' | spec_cols$name == 'Tattoo Shop'),
  jps_pat_testing = (spec_cols$name == 'Jps Pat Testing'),
  hairdresser = (spec_cols$shop == 'hairdresser' | spec_cols$name == 'Hairdresser')
)

#loop through each condition
for (condition_name in names(conditions)) {
  condition <- conditions[[condition_name]]
  
  # Filter data based on the condition
  filtered_data <- spec_cols[condition, ]
  
  # Extract osm_id values and remove NAs
  osm_ids <- na.omit(filtered_data$osm_id)
  
  # Store only the osm_ids in the list
  filtered_data_list[[condition_name]] <- osm_ids
}

#combine all filtered datasets into one
combined_osm_ids <- unlist(filtered_data_list)

#identify rows to delete
rows_to_delete <- all_points_unique.sf$osm_id %in% combined_osm_ids

# subset to keep rows not in combined_osm_ids
all_points_unique2.sf <- subset(all_points_unique.sf, !rows_to_delete)

# may want to reset row names after deletion
rownames(all_points_unique2.sf) <- NULL

##### this is extra cleaning, so that outlets that have both amenity and shop values dont get counted twice !!!!! #####
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

# these are the osm_ids that both had shop and amenity and im changing their shop to NA
osm_ids_to_update <- c(2428095208, 8495101135, 217683441, 246569256, 373120778)


# update the 'shop' column to NA for these specific osm_ids
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

#now finally add the columns back into all_points_unique2.sf

# update the all_points_unique2.sf data frame with the cleaned shop and amenity columns using st_join
all_points_unique2.sf <- all_points_unique2.sf %>%
  st_join(selected_columns, join = st_intersects, suffix = c("", ".new")) %>%
  mutate(
    shop = coalesce(shop.new, shop),
    amenity = coalesce(amenity.new, amenity)
  )

# remove temporary columns directly using the $ notation, suitable for sf objects
all_points_unique2.sf$shop.new <- NULL
all_points_unique2.sf$amenity.new <- NULL


########################### MAPPING SOCIOECONOMIC DATA FROM 2011 LSOAS TO 2021 LSOAS ###########################

# data needed for transformation: RUC, AHAH, IMD (for LSOAs)

# map 2011 LSOAs to 2021 LSOAs in these datasets
imd2019scores <- merge(imd2019scores, LSOA2011to2021, by.x = "LSOA.code.(2011)", by.y = "LSOA11CD", all.x = TRUE)
AHAH <- merge(AHAH, LSOA2011to2021, by.x = "lsoa11", by.y = "LSOA11CD", all.x = TRUE)
LSOARUC_selected <- LSOARUC %>%
  left_join(LSOA2011to2021, by = c("LSOA11CD" = "LSOA11CD"))

imd2019_selected <- imd2019scores %>%
  select(`LSOA.code.(2011)`,`LSOA.name.(2011)`, LSOA21CD, LSOA21NM, `Index.of.Multiple.Deprivation.(IMD).Score`)

AHAH_selected <- AHAH %>%
  select(LSOA21CD, ah3ahah, ah3pubs)

# imd2019 uses 2011 lsoas - sometimes two 2011 lsoas correspond to one 2021 lsoa. i find an average for those.
# aggregate IMD 2019 data by averaging the scores for duplicate 2021 LSOAs, keeping all columns
imd2019_aggregated <- imd2019_selected %>%
  group_by(LSOA21CD) %>%
  summarize(across(everything(), ~ if (is.numeric(.)) mean(., na.rm = TRUE) else first(.)))

# same applies to AHAH index. 
# aggregate AHAH data by averaging the relevant scores for duplicate 2021 LSOAs, keeping other columns
AHAH_aggregated <- AHAH_selected %>%
  group_by(LSOA21CD) %>%
  summarize(
    ah3ahah_rnk = mean(ah3ahah, na.rm = TRUE),
    ah3pubs = mean(ah3pubs, na.rm = TRUE),
    across(-c(ah3ahah, ah3pubs), ~ first(.))
  )

# aggregate RUC data, setting to NA if there are different RUC11 values for the same LSOA2021
RUC_aggregated <- LSOARUC_selected %>%
  group_by(LSOA21CD) %>%
  summarize(
    RUC11 = if (n_distinct(RUC11) > 1) NA else first(RUC11),
    .groups = "drop"
  )

### Police Lookup Table
#some discrepancies with the names of LADs on this dataset and how they are on LAD_shapes.
# modify the LAD22NM columns so they align with LAD_shapes names
LADPolice2022 <- LADPolice2022 %>%
  mutate(LAD22NM = ifelse(LAD22NM == 'Bristol, City of', 'Bristol', LAD22NM))
LADPolice2022 <- LADPolice2022 %>%
  mutate(LAD22NM = ifelse(LAD22NM == 'Herefordshire, County of', 'Herefordshire', LAD22NM))
LADPolice2022 <- LADPolice2022 %>%
  mutate(LAD22NM = ifelse(LAD22NM == 'Kingston upon Hull, City of', 'Kingston upon Hull', LAD22NM))
LADPolice2022 <- LADPolice2022 %>%
  mutate(PFA22NM = ifelse(PFA22NM == 'London, City of', 'City of London', PFA22NM))
LADPolice2022 <- LADPolice2022 %>%
  mutate(PFA22NM = ifelse(PFA22NM == 'Metropolitan Police', 'Metropolitan', PFA22NM))
