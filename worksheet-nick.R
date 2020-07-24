## gov-infra script from Sesync Cyberinstitute 2020

## Inputs: parcels (shp), deforestation by year per parcel (csv)
## Task 1 - READ in data
## Task 2 - DETECT registration year for parcels
## Task 3 - CALCULATE before/after/difference in forest loss
## Task 4 - CONVERT to rate based on parcel area
## Task 5 - VIZ distribution of in a map
## Task 6 - VIZ distribution in a chart

## READ DATA
library(sf)
library(lubridate)
library(data.table)
library(dplyr)


shp <- '/home/nicholas/WAmaz_BR_Parcels_sirgaspolyc.shp'   ##change based on wkdir

parcels <- st_read(
  shp,
  stringsAsFactors = FALSE)

deforyr <- fread('/home/nicholas/parcels_GFClossyear_BefAft.csv',header=TRUE)


## Deriving year as a number for visualization
parcels2 <- mutate(parcels,
                           YEAR = year(data_aprov))

##plot(parcels2['YEAR'], main="Year of registration")  ## -received


## Joining deforestation to parcels (2013-2017 ONLY so inner join)
parcels3 <- parcels2 %>% inner_join(deforyr)

parcels3 <- mutate(parcels3,
                   TOTALM2 = 900*TOTAL, 
                   TOTALRATE = (900*TOTAL)/(Area_km+0.01)/1000000) ## the 0.01 has no real basis I just added it in as a quick fix to handle a few parcels that are super tiny, likely errors
                 
##Plotting maps
    
##plot(parcels3['TOTALM2'], main="Total Deforestation within Parcels 2000-2018 (m2)", border=NA)
##plot(parcels3['TOTALRATE'], main="Total Deforestation within Parcels 2000-2018 as a portion of Parcel Area", border=NA)
plot(parcels3['ANNRATEDIFF'], main="Difference in Annualized Deforestation Before/After Registration", border=NA)
