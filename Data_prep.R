
## clear environment
rm(list = ls())

## load packages
library(sf) # vector spatial data
library(raster) # continuous spatial data
library(lwgeom) # continuous spatial data
library(lubridate) 
library(ggrepel) 
library(rgdal) 
library(rgeos) 

library(dplyr) # data manipulation
library(ggplot2) # plotting
library(xtable) # tex tables for export
library(stargazer) 


### Load Data ----
## Load AID and QIPs
#qips <- read.csv('Data/Projects/qips.csv')
qips <- read.csv('Data/Projects/qips2.csv')
qips$longitude <- as.numeric(as.character(qips$longitude))
iati <- read.csv('Data/Projects/aiddata.csv')

aid <- rbind(qips, iati)
#aid <- na.omit(aid)

names(iati)
xtable(table(iati$sector_group))

table(iati$sector_code)

## ACLED
# acled <- read.csv('Data/ACLED/acled-2013-2020.csv') (only poc)
acled <- read.csv('Data/ACLED/acled.all.csv')


## load population data
#pop_dens <- raster('Data/pop.dens.tif')
# try different
pop_dens <- raster('Data/gpw_v4_population_density_rev11_2015_30_sec.tif')

## load nightlights
nl <- raster('Data/nightlights/F182013.v4c_web.cf_cvg.tif')

## load shapefiles
cshapes <- shapefile("Data/mli_adm_1m_dnct_2019_shp/mli_admbnda_adm3_1m_dnct_20190802.shp")  
names(cshapes)<- tolower(names(cshapes))
sl <- st_read('Data/mli_adm_1m_dnct_2019_shp/', 'mli_admbnda_adm3_1m_dnct_20190802')
plot(cshapes)
cities <- read.csv('Data/Projects/cities.csv')
cities$lon <- as.numeric(as.character(cities$lon))
cities$lon[is.na(cities$lon)]<- -8


## load road data
roads <- st_read('Data/Roads/hotosm_mali_roads_lines_shp',
                 'hotosm_mali_roads_lines')
## load water data
water <- st_read('Data/Water/',
                 'hotosm_mali_waterways_lines')


## Spatial Prep ----
## subset population data to polygon
pop_dens <- crop(pop_dens, cshapes)
nl <- crop(nl, cshapes)
pop_dens <- crop(pop_dens, cshapes)

## create sf object for capital
capital <- sl %>% filter(admin3Name == 'Commune VI')

unbase <- sl %>% filter(admin3Name %in% c('Mopti', 'Tombouctou', 'Gao', 'Ménaka', 'Kidal', 'Commune VI'))


## project roads to WGS84
roads <- st_transform(roads, st_crs(cshapes))
roads <- subset(roads, highway%in%c("trunk", "motorway", "primary", "secondary", "tertiary"))

## project water to WGS84
water <- st_transform(water, st_crs(cshapes))
table(water$waterway)
water <- subset(water, waterway%in%c("river", "riverbank", "lake"))

### Prep ACLED Data ----
acled$event_date <- as.POSIXct(as.character(acled$event_date), format="%Y-%m-%d")

# DV1: Violence against Civilians
civs <- subset(acled, event_type =="Violence against civilians")
#civs.reb <- subset(civs, inter1 %in% c("1"))

## get violence against civilians
civs <- civs %>%  mutate(event_type = 'Violence against civilians',
                         lat = latitude, lon = longitude,
                         timestamp = event_date) %>% 
  dplyr::select(timestamp, lat, lon, event_type)


# DV2: Battles

bat <-subset(acled, event_type =="Battles")

bat <- bat %>%  mutate(event_type = 'Battles',
                       lat = latitude, lon = longitude,
                       timestamp = event_date) %>% 
  dplyr::select(timestamp, lat, lon, event_type)

# DV3: Rebel Activitiy (for appendix)
reb <- subset(acled, inter1 %in% c("4", "3", "2"))

reb <- reb %>%  mutate(event_type = 'NSAG activity',
                        lat = latitude, lon = longitude,
                        timestamp = event_date) %>% 
  dplyr::select(timestamp, lat, lon, event_type)


### Prep Aid Data ----
aid <- aid %>% dplyr::rename(timestamp = start_date)
aid$timestamp <- as.POSIXct(as.character(aid$timestamp), format="%Y-%m-%d")
aid <- aid[, c(2:3, 8:9)]
aid <- aid %>% dplyr::rename(lat = latitude, lon = longitude)
aid$event_type <- as.character(aid$event_type)

### Bind Data ----

mw_data <- rbind(aid, civs)

mw_data <- rbind(mw_data, reb)
mw_data <- rbind(mw_data, bat)

mw_data <- na.omit(mw_data)
table(mw_data$event_type)

### Subset Year  ----

# Subset to 2017 -2020
mw_data <- mw_data %>%
  mutate(
    year = lubridate::year(timestamp))
mw_data <- subset(mw_data, year > 2016)
mw_data <- subset(mw_data, year < 2021)

range(mw_data_sub$year)


### Subset Regions ----
# Add Region Names
e <- extract(cshapes, mw_data[, c('lon','lat')]) 
mw_data$admin1 <- e$admin1name 


mw_data_sub <- subset (mw_data, admin1%in%c("Gao", "Kidal", "Mopti", "Ségou", "Tombouctou"))
table(mw_data$admin1)


### Add Covariates ----


#coordinates(mw_data_sf)
mw_data_sf <- st_as_sf(mw_data_sub, coords = c('lon', 'lat'),
                       crs = 4326, agr = 'identity')


mw_data_sub$road_dist <- log(apply(st_distance(mw_data_sf, roads), 1, min)+1)
mw_data_sub$water_dist <- log(apply(st_distance(mw_data_sf, water), 1, min)+1)
mw_data_sub$pop_dens <- raster::extract(pop_dens,mw_data_sf)
mw_data_sub$unbase <- log(apply(st_distance(mw_data_sf, unbase), 1, min)+1)
mw_data_sub$nl <- raster::extract(nl,mw_data_sf)




