############################ CARTOGRAPHIC ANALYSIS CODE ############################

### number of pubs and convenience store, and outlet density code 
outlets_all.sf <- all_points_unique2.sf %>%
  select(osm_id, amenity, shop, geometry)


#these shops are both convenience and restaurants - i researched which ones are true.
#altering them so they dont get counted twice
outlets_all.sf <- outlets_all.sf %>%
  mutate(amenity = case_when(
    osm_id == 8495101135 ~ NA_character_,   # convenience
    osm_id == 217683441 ~ 'restaurant',
    osm_id == 246569256 ~ 'restaurant',
    osm_id == 373120778 ~ 'restaurant',
    TRUE ~ amenity
  )) %>%
  mutate(shop = case_when(
    osm_id == 8495101135 ~ 'convenience',
    osm_id %in% c(217683441, 246569256, 373120778) ~ NA_character_,  # restaurant
    TRUE ~ shop
  ))

outlets_in_LAD <- st_join(outlets_all.sf, LAD_shapes, join = st_intersects)
# count amenities
amenity_counts <- outlets_in_LAD %>%
  filter(!is.na(amenity) & !amenity %in% c('fuel','post_office','atm','advice','convenience', 'convenience_store','post_office;post_box','shops',
                                           '', 'cafe', 'community_centre', 'fast_food', 'off_license', 'parking') ) %>% 
  #amenity:fuel = shop:convenience so don't want to double count. also other amenities taken out.
  group_by(LAD22CD, LAD22NM, type = amenity) %>%
  summarise(count = n(), .groups = 'drop')


# count shops
shop_counts <- outlets_in_LAD %>%
  filter(!is.na(shop)& shop != '' & !shop %in% c('vacant','tobacco','newsagent','supermarket',
                                                 'golf','laundry','mechanic','deli','music','alcohol;music;brewing_supplies',
                                                 'yes', 'alcohol;tea', 'angling','antiques','bedding','beverages','books','bookmaker',
                                                 'clothes','bar','beer') &
           !(shop == 'alcohol' & !is.na(amenity) & amenity %in% c('bar', 'pub', 'nightclub', 'casino', 'biergarten', 'restaurant')) &
           !(shop == 'wine' & !is.na(amenity) & amenity %in% c('bar', 'pub', 'nightclub', 'casino', 'biergarten', 'restaurant'))) %>% 
  #blank character shops have an amenity so don't want to double count. also with these values
  group_by(LAD22CD, LAD22NM, type = shop) %>%
  summarise(count = n(), .groups = 'drop')

#get rid of na's - these outlets are not in england !!!
shop_counts <- na.omit(shop_counts)
amenity_counts <- na.omit(amenity_counts)



LAD_names <- LAD_shapes$LAD22NM
LAD_ladpop <- ladpop$Name

# check LAD names that are in LAD_names but not in LAD_ladpop
missing_LAD_names <- setdiff(LAD_names, LAD_ladpop)
missing_LAD_names #none

#mapping

outlet_counts <- bind_rows(amenity_counts, shop_counts)
# summarise total outlets per LAD
total_outlets_per_LAD <- outlet_counts %>%
  group_by(LAD22CD, LAD22NM) %>%
  summarise(total_outlets = sum(count), .groups = 'drop')

# rename columns in SAM_LAD for joining
SAM_LAD <- SAM_LAD %>%
  rename(Code = LAD22CD)

# rename columns in ladpop for consistency
ladpop <- ladpop %>%
  rename(Mid_2022 = `Mid-2022`)

SAM_LAD <- SAM_LAD %>%
  left_join(ladpop, by = "Code") %>%
  rename(LAD22CD = Code) %>%
  select(!Name)

# Merge total outlets with area and population data
SAM_LAD <- SAM_LAD %>%
  left_join(total_outlets_per_LAD, by = c("LAD22CD", "LAD22NM")) %>%
  mutate(outlet_density_area = total_outlets / LandAreaSqkm,
         outlet_density_population = total_outlets / Mid_2022)

# ensure SAM_LAD has the same geometry as LAD_shapes
LAD_data <- LAD_shapes %>%
  left_join(SAM_LAD, by = c("LAD22CD", "LAD22NM"))

Areabreaks = c(0, 0.25, 0.5, 1, 5, 10, Inf)
Arealabels = c('0-0.25','0.25-0.5', '0.5-1', '1-5','5-10','10+')
Popbreaks = c(0, 0.0005, 0.001, 0.0015, 0.002, 0.0025, Inf)
Poplevels = c('0-0.0005','0.0005-0.001','0.001-0.0015','0.0015-0.002','0.002-0.0025', '0.0025+')

# plot outlet density by area with new breaks
ggplot(LAD_data) +
  geom_sf(aes(fill = cut(outlet_density_area, breaks = Areabreaks)), color = NA, size=0.001) +
  scale_fill_viridis_d(labels = Arealabels) +
  theme_minimal() +
  ggtitle("Outlet Density by Area (per km²)") +
  labs(fill = "Density (Outlets per km²)")

# plot outlet density by population with new breaks
ggplot(LAD_data) +
  geom_sf(aes(fill = cut(outlet_density_population, breaks = Popbreaks)), color = NA, size=0.001) +
  scale_fill_viridis_d(labels = Poplevels) +
  theme_minimal() +
  ggtitle("Outlet Density by Population (per capita)") +
  labs(fill = "Density (Outlets per capita)")


# creating the violin plot (not seen in report)
ggplot(LAD_data, aes(x = RUC11, y = outlet_density_area, fill = RUC11)) +
  geom_violin(trim = FALSE) +
  geom_jitter(width = 0.2, size = 0.5, alpha = 0.5) + # adds individual data points for better understanding
  scale_y_continuous(name = "Outlet Density per Square Km") +
  scale_x_discrete(name = "Rural–Urban Classification") +
  theme_minimal() +
  ggtitle("Distribution of Outlet Density by RUC Level") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # rotates x-axis labels for better readability
  scale_fill_viridis_d(name = "RUC11") 

#specific counts
# summarise pubs per LAD
pub_counts <- outlet_counts %>%
  filter(type == "pub") %>%
  group_by(LAD22CD, LAD22NM) %>%
  summarise(pub_count = sum(count), .groups = 'drop')

# summarise convenience stores per LAD
convenience_counts <- outlet_counts %>%
  filter(type == "convenience") %>%
  group_by(LAD22CD, LAD22NM) %>%
  summarise(convenience_count = sum(count), .groups = 'drop')


# perform the spatial join
LAD_data_pubs <- st_join(LAD_shapes, pub_counts, left =T)

LAD_data_conv <- st_join(LAD_shapes, convenience_counts, left=T)

# plot number of pubs
ggplot(LAD_data_pubs) +
  geom_sf(aes(fill = pub_count), color = NA, size = 0.001) +
  scale_fill_viridis_c() +
  theme_minimal() +
  ggtitle("Number of Pubs per Local Authority District") +
  labs(fill = "Number of Pubs")

# plot number of convenience
ggplot(LAD_data_conv) +
  geom_sf(aes(fill = convenience_count), color = NA, size = 0.001) +
  scale_fill_viridis_c() +
  theme_minimal() +
  ggtitle("Number of Convenience Stores per Local Authority District") +
  labs(fill = "Number of Convenience Stores")
