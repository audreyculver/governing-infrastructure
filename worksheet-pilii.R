## Loading data

library(sf)
library(ggplot2)
library(dplyr)
library(leaflet)
library(tidyr)
library(raster)


## set working directory

setwd("~/gi-handouts/Data")

## Read data

state_boundaries <- st_read('BRA_adm1.shp')

state_parcels <- st_read("WAmaz_BR_Parcels_dissolved.shp")  #union of parcels by state

parcels <- st_read('WAmaz_BR_Parcels_sirgaspolyc.shp')

roads <- st_read('ST_DNIT_Rodovias_SNV2015_03.shp')

deforestation <- read.csv('parcels_GFClossyear.csv')  #vectorial raster

## Projection

st_crs(parcels)$proj4string
st_crs(state_parcels)$proj4string

prj <-'+proj=longlat +datum=WGS84 +no_defs'  #state_parcels projection

parcels <- st_transform(parcels, crs = prj)
roads <- st_transform(roads, crs = prj)
state_boundaries <- st_transform(state_boundaries, crs = prj)

# Separate Year (column)

parcels <- parcels %>% 
  separate(col = 'data_submi', into = c("year", "month", 'day'),
           sep = "-")

class(parcels$year)

parcels$year <- as.numeric(parcels$year)

year <- dplyr::select(parcels, year) # create layer of parcels by date

## Join

parcels_GFC <- merge(parcels, deforestation, by = 'parcela_co', all = TRUE) 


## Clipping

# parcels per state
acre_p <- filter(state_parcels, NAME_1 == 'Acre')
rondonia_p<- filter(state_parcels, NAME_1 == 'Rondônia')

# independent state
acre <- filter(state_boundaries, NAME_1 == 'Acre')
rondonia <- filter(state_boundaries, NAME_1 == 'Rondônia')
amaz <- filter(state_boundaries, NAME_1 == 'Amazonas')

# roads per state
roads_acre <- st_intersection(roads, acre)
roads_rondonia <- st_intersection(roads, rondonia)
roads_amaz <- st_intersection(roads, amaz)

## Plot

plot(year, main = 'Parcels by Date of Title')
plot(acre$geometry)
plot(rondonia$geometry)
plot(amaz$geometry)


pdf("parcels_state.pdf", width = 10, height = 10)

plot(year, main = 'Parcels by Date of Title')

jpeg("parcels_year.jpg", width = 10, height = 10)

ggplot() + 
  geom_sf(data = parcels, color = 'red') +
  geom_sf(data = roads_acre, color = "black") +
  geom_sf(data = roads_rondonia, color = 'black') +
  geom_sf(data = roads_amaz, color = 'black') +
  ggtitle("Parcels and Road Network") + 
  coord_sf()

pdf("parcels_roads.pdf", width = 10, height = 10)


## Raster Data

lossyear <- raster('Hansen_lossyear_19_brparcels_AAR.tif')

extent <- matrix(st_bbox(parcels), nrow=2)
lossyear <- crop(lossyear, extent)

extent2 <- matrix(st_bbox(acre), nrow = 2)
lossyear2 <- crop(lossyear, extent2)

plot(lossyear, main = 'Deforestation and Road Network')
plot(roads_acre$geometry, fill = 'black',
     add = TRUE)
plot(roads_rondonia$geometry, fill = 'black',
     add = TRUE)
plot(roads_amaz$geometry, fill = 'black',
     add = TRUE)

pdf("roads_loss.pdf", width = 10, height = 10)

plot(lossyear, main = 'Deforestation and Property')
plot(parcels$geometry, fill = 'black', add = TRUE)

plot(lossyear2, main = 'Deforestation and Property in Acre')
plot(acre_p$geometry, fill = 'black', add = TRUE)
plot(acre$geometry, fill = 'black', add = TRUE)
plot(roads_acre$geometry, fill = 'white', add = TRUE)

ggplot(parcels_deforestation) +  #just for vector data, doesn't apply now
  geom_sf(aes(fill=year))


