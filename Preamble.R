##################### preamble.1 (installing packages and libraries) #####################

#general packages
library(dplyr, tidyverse)

#plotting and palettes packages 
# install.packages(c('ggplot2','ggforce','leaflet','viridis','RColorBrewer','rnaturalearth','rnaturalearthdata', 'GGally', 'leaflet.extras', 'igraph'))
library(ggplot2, ggforce, leaflet, viridis, RColorBrewer, rnaturalearth, rnaturalearthdata, GGally, leaflet.extras, igraph)

#packages for reading in differnt file types
# install.packages(c('readODS','openxlsx'))
library(readODS, openxlsx)

#dbscan, spatial analysis and parallel processing packages
# install.packages(c('dbscan','parallel', 'doParallel' 'sf', 'sp'))
library(dbscan, parallel, doParallel, sf, sp)

#k-prototypes packages
# install.packages(c('clustMixType', 'corrplot'))
library(clustMixType, corrplot)

#glms and diagnostic checking packages
# install.packages(c('car','performance','dHARMA')) 
library(car, performance, dHARMA)

##################### preamble.2 (reading in files that we utilise in the report) #####################

### LSOA2021 and LAD2022 shapefiles
#from https://geoportal.statistics.gov.uk/datasets/0f80c523f3cd4d0fab5111572f84a2fb_0/explore (LSOA2021)
#from https://geoportal.statistics.gov.uk/datasets/196d1a072aaa4882a50be333679d4f63 (LAD2022)
#LSOAshp_path <- '/Volumes/Terabythia/ExtendedProject/LSOA2021/LSOA_2021_EW_BFC_V8.shp'
lsoa_shapes <- st_read(LSOAshp_path) 
# these LSOA's include more of the UK but we only want England
lsoa_shapes <- lsoa_shapes %>%
  filter(grepl("^E", LSOA21CD))
#LAD_path <- '/Volumes/Terabythia/ExtendedProject/Local_Authority_Districts_May_2022_UK_BFE_V3_2022_3331011932393166417/LAD_MAY_2022_UK_BFE_V3.shp'
LAD_shapes <- st_read(LAD_path)
# these LAD's include Wales, Scotland and NI too, but we only want England
LAD_shapes <- LAD_shapes %>%
  filter(!grepl("^(W|S|N)", LAD22CD))

### LSOA2011 to LSOA2021 Lookup Table
#from https://geoportal.statistics.gov.uk/datasets/cbfe64cc03d74af982c1afec639bafd1_0/explore
LSOA2011to2021 <- read.csv('LSOA2011to2021.csv') #this is the one i used loads. 

### LSOA2021 to LAD2022 Lookup Table
#from https://www.data.gov.uk/dataset/19a01ab0-2111-4c29-8a89-a6dd14ba845c/lsoa-2021-to-electoral-ward-2022-to-lad-2022-best-fit-lookup-in-ew-v3
LSOA2021toLAD2022 <- read.csv('LSOA2021toLAD.csv')

### STANDARD AREA MEASUREMENT DATASET
# from https://geoportal.statistics.gov.uk/datasets/ons::standard-area-measurements-for-administrative-areas-december-2021-in-the-uk-v2/about

SAM_LAD <- read.csv('StdAreaMeasurements/SAM_LAD.csv') %>%
  filter(!grepl("^W|S|N", LAD22CD)) %>%
  mutate(LandAreaSqkm = AREALHECT / 100) %>%
  select(LAD22NM, LAD22CD, LandAreaSqkm) #100 hectares to every sq km

### POPULATION DENSITY DATASET (MID 2023)
# from https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/estimatesofthepopulationforenglandandwales

ladpop <- read.xlsx('poplad2021.xlsx', sheet='MYE4', startRow=8) %>%
  filter(!grepl("^K|W|S", Code)) %>%
  filter(!Geography %in% c('Country','County','Region','Metropolitan County')) %>%
  select(Code, Name, `Mid-2022`)

### RUC (Rural Urban Classification) for LADs and LSOAs
# from https://www.gov.uk/government/statistics/2011-rural-urban-classification-of-local-authority-and-other-higher-level-geographies-for-statistical-purposes#:~:text=The%202011%20Rural%20Urban%20Classification,Local%20Authority%20Rural%20Urban%20Classification.
# and https://geoportal.statistics.gov.uk/datasets/803b5eba7f6f4c998b7d2c5be6729693_0/explore respectively
# lookup table
LADRUC <- read_ods('RUC_LAD11.ods', sheet = 'RUC11_LAD11_EN')
LADRUC <- LADRUC %>%
  filter(startsWith(LAD11CD, 'E')) #these filter out any rows that aren't a part of England

#read LAD RUC file
LSOARUC <- read_ods('RUC_LSOA11.ods', sheet = 'RUC11_LSOA11_EN')
LSOARUC <- LSOARUC %>%
  filter(startsWith(LAD11CD, 'E'))

### LAD Police File lookup
#from https://www.data.gov.uk/dataset/f03d9cdf-6654-4437-8d1e-ecf11a210085/lad-to-community-safety-partnership-to-pfa-december-2022-lookup-in-ew
LADPolice2022 <- read.csv(LADPolice2022.csv)

### IMD 2019
# file 5 of https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019
imd2019scores <- read.xlsx('imd2019scores.xlsx', sheet='IoD2019 Scores') %>%
  filter(grepl('^E', `LSOA.code.(2011)`)) #these filter out any rows that aren't a part of England

### AHAH index
#from https://data.cdrc.ac.uk/dataset/access-healthy-assets-hazards-ahah
AHAH <- read.csv("AHAH.csv") %>%
  filter(grepl('^E', lsoa11))

### RETRIEVING THE RAW OSM DATA 

# retrieve the bounding box of england directly from osm
bbox_england <- getbb('England UK')

# build the queries from all specified alcohol outlets, then retrieve it from osm

# pubs
Englandpubs.sf <- opq(bbox = bbox_england, timeout = 300) %>% #sometimes it takes a long time so added extra timeout time
  add_osm_feature(key = "amenity", value = "pub") %>% #this builds the query
  osmdata_sf()  #this command retrives the data

# nightclubs
Englandclubs.sf <- opq(bbox = bbox_england, timeout = 300) %>%
  add_osm_feature(key = "amenity", value = "nightclub") %>% 
  osmdata_sf()

# biergartens
Englandbiergarten.sf <- opq(bbox = bbox_england, timeout = 300) %>%
  add_osm_feature(key = "amenity", value = "biergarten") %>% 
  osmdata_sf()

# bars
Englandbars.sf <- opq(bbox = bbox_england, timeout = 300) %>%
  add_osm_feature(key = "amenity", value = "bar") %>% 
  osmdata_sf()

#casinos
Englandcasinos.sf <- opq(bbox = bbox_england, timeout = 300) %>%
  add_osm_feature(key = "amenity", value = "casino") %>% 
  osmdata_sf()

#stripclubs
Englandstrips.sf <- opq(bbox = bbox_england, timeout = 300) %>%
  add_osm_feature(key = "amenity", value = "stripclub") %>% 
  osmdata_sf()

# restaurants
EnglandRestaurants.sf <- opq(bbox = bbox_england, timeout=300) %>%
  add_osm_feature(key = "amenity", value = "restaurant") %>%
  add_osm_feature(key = "bar", value = "yes", value_exact = TRUE) %>%
  add_osm_feature(key = "microbrewery", value = "yes", value_exact = TRUE) %>% 
  osmdata_sf()

#convenience
EnglandConven.sf <- opq(bbox = bbox_england, timeout=300) %>%
  add_osm_feature(key = "shop", value = "convenience") %>% 
  osmdata_sf()

# wine
EnglandWineshop.sf <- opq(bbox = bbox_england, timeout=300) %>%
  add_osm_feature(key = "shop", value = "wine") %>% 
  osmdata_sf()

# alcohol
EnglandAlcshop.sf <- opq(bbox = bbox_england, timeout=300) %>%
  add_osm_feature(key = "shop", value = "alcohol") %>% 
  osmdata_sf()






