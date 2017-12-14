#========================================================================================*
# ---- SET-UP ----
#========================================================================================*

library(tidyverse)
library(raster)
library(rgdal)
library(leaflet)
library(maptools)
library(stringr)

select <- dplyr::select
filter <- dplyr::filter


#========================================================================================*
# ---- SHAPEFILE WORK ----
#========================================================================================*

#----------------------------------------------------------------------------------------*
# ---- Simplify ecoregions file to biomes ----
#----------------------------------------------------------------------------------------*

# Ecoregions file:

ecoRegions <- readOGR(dsn = 'terr-ecoregions-TNC', 'tnc_terr_ecoregions')

# Add latitude and longitude of polygons centroids to data file:

ecoRegions@data <- ecoRegions@data %>%
  mutate(
    cLong = coordinates(ecoRegions)[,1],
    cLat = coordinates(ecoRegions)[,2]
  )

# Simplify to Americas:

centroids <- coordinates(ecoRegions)

ecoAmericas <- ecoRegions %>%
  subset(
    cLong < -20,
    cLong > -168.5,
    cLat > -60,
    cLat < 85)

ecoAmericas <- ecoRegions %>%
  subset(WWF_REALM %in% c('NA', 'NT', 'PA')) %>%
  subset(between(cLong,  -168.5, -20))

cp <- ecoAmericas %>%
  subset(between(cLong, -93, -74)) %>%
  subset(between(cLat, 26, 38))

# Make sure coastal plain savannahs are correctly classified!

grass <- 'Temperate Grasslands, Savannas and Shrublands'

ecoAmericas@data <- ecoAmericas@data %>%
  mutate(WWF_MHTNAM = case_when(
    ECO_NAME == 'East Gulf Coastal Plain' ~ grass,
    ECO_NAME == 'South Atlantic Coastal Plain' ~ grass,
    ECO_NAME == 'Florida Peninsula' ~ grass,
    ECO_NAME == 'Mid-Atlantic Coastal Plain' ~ grass,
    TRUE ~ as.character(WWF_MHTNAM)
  )
  )

# Get unique biomes:

biomes <- ecoAmericas@data$WWF_MHTNAM

# Simplify polygons to biomes:

ecoBiome <- unionSpatialPolygons(ecoAmericas, biomes)

# Make a data frame and bring in to simplified shape:

biomeSpDf <- SpatialPolygonsDataFrame(
  ecoBiome, # was biomeSimp
  data.frame(biome = unique(biomes),
             row.names = unique(biomes))
)


# Simplify the edges of the shapes:

# biomeSimp <- rgeos::gSimplify(ecoBiome, tol = 0.1, topologyPreserve = TRUE)

# Shapefile:

writeOGR(biomeSpDf, dsn = '.', 'biomes', driver = 'ESRI Shapefile')

#----------------------------------------------------------------------------------------*
# ---- Simplify biomes file to generalized biomes ----
#----------------------------------------------------------------------------------------*

# Read biomes file (created above)

biomesFull <- readOGR(dsn = '.', 'biomes') # was reading 'biomeSpDf'

# Add generalized biome column:

biomesFull@data <- biomesFull@data %>%
  mutate(newBiome = c(
    'Boreal forest', 
    'Desert and dry shubland',
    'Wetland',
    'Inland water',
    'Mangrove',
    'Mediterranean forest, woodland, and shrub',
    'Grassland and shrubland',
    'Rock and ice',
    'Temperate forest',
    'Temperate forest',
    'Grassland, savannah, and shrubland',
    'Tropical and subtropical forest',
    'Tropical and subtropical forest',
    'Grassland and shrubland',
    'Tropical and subtropical forest',
    'Tundra'
  ))

# Simplify polygons to biomes:

newBiomePoly <- unionSpatialPolygons(biomesFull, biomesFull@data$newBiome)

# Make a data frame and bring in to simplified shape:

newBiome <- SpatialPolygonsDataFrame(
  newBiomePoly,
  data.frame(biome = unique(biomesFull@data$newBiome),
             row.names = unique(biomesFull@data$newBiome))
)

# Remove inland water:

newBiome <- subset(newBiome,  biome != 'Inland water')


# Write shapefile:

writeOGR(newBiome, dsn = '.', 'biomeSimple', driver = 'ESRI Shapefile')


# View:

factPal <- colorFactor('Spectral', newBiome$biome)

leaflet(newBiome) %>%
  addTiles() %>%
  setView(-20, 23, 2) %>%
  addPolygons(color = "#444444", weight = .5, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 1,
              fillColor = ~factPal(biome),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)) %>%
  addLegend("bottomleft", pal = factPal, values = ~biome,
            title = "Biome",
            opacity = 1
  ) 

#========================================================================================*
# ---- RASTER WORK ----
#========================================================================================*

# Raster land cover work (Argentina):

lc <- raster('Globcover/GLOBCOVER_200412_200606_V2.2_Global_CLA.tif')

# North and South America extents:

eSouthAmerica <- extent(c(-86,-32,-60,15))

eNorthAmerica <- extent(c(-165,-51,6,85))

# Crop lc by extent

lcSouthAmerica <- crop(lc, eSouthAmerica)

lcNorthAmerica <- crop(lc, eNorthAmerica)

# Keep raw cropped files:

lcSAraw <- lcSouthAmerica

lcNAraw <- lcNorthAmerica

# Write to files:

writeRaster(lcSAraw,'lc-SouthAmericaRaw', overwrite = TRUE)

writeRaster(lcNAraw,'lc-NorthAmericaRaw', overwrite = TRUE)

# Read rasters:

lcSAraw <- raster('lc-SouthAmericaRaw')

lcNAraw <- raster('lc-NorthAmericaRaw')

#----------------------------------------------------------------------------------------*
# ---- Raster processing, Aggregation ----
#----------------------------------------------------------------------------------------*

# Aggregation of raster, South America:

lcSouthAmericaAgg1 <- raster::aggregate(lcSAraw, fact = 4, fun=modal)

lcSouthAmericaAgg2 <- raster::aggregate(lcSouthAmericaAgg1, fact = 4, fun=modal)

lcSouthAmerica <- lcSouthAmericaAgg2

# Aggregation of raster, North America:

lcNorthAmericaAgg1 <- raster::aggregate(lcNAraw, fact = 4, fun=modal)

lcNorthAmericaAgg2 <- raster::aggregate(lcNorthAmericaAgg1, fact = 4, fun=modal)

lcNorthAmerica <- lcNorthAmericaAgg2

# Write to files:

writeRaster(lcSouthAmerica,'lcSouthAmericaAgg', overwrite = TRUE)

writeRaster(lcNorthAmerica,'lcNorthAmericaAgg', overwrite = TRUE)

# Read rasters:

lcSouthAmerica <- raster('lcSouthAmericaAgg')

lcNorthAmerica <- raster('lcNorthAmericaAgg')

#----------------------------------------------------------------------------------------*
# ---- Raster reclassification, South America ----
#----------------------------------------------------------------------------------------*

# Reclassification of rasters:

values(lcSouthAmerica)[values(lcSouthAmerica) %in% c(210,230)] <- NA # water and no data

lcSAtrim <- trim(lcSouthAmerica)

values(lcSouthAmerica)[values(lcSouthAmerica) %in% c(120, 140, 60, 90, 110)] <- 1 # grassland

values(lcSouthAmerica)[values(lcSouthAmerica) %in% c(130)] <- 2 # shrub

values(lcSouthAmerica)[values(lcSouthAmerica) %in% c(11, 14, 20, 30)] <- 3 # agriculture

values(lcSouthAmerica)[values(lcSouthAmerica) %in% c(40,50, 70, 100)] <- 4 # forest

values(lcSouthAmerica)[values(lcSouthAmerica) %in% c(160,170,180)] <- 5 # wetland

values(lcSouthAmerica)[values(lcSouthAmerica) %in% c(190)] <- 6 # city

values(lcSouthAmerica)[values(lcSouthAmerica) %in% c(150, 200)] <- 7 # barren

values(lcSouthAmerica)[values(lcSouthAmerica) %in% c(220)] <- 7 # ice

# Write raster to file:

writeRaster(lcSouthAmerica, 'lcSouthAmericaRc', overwrite = TRUE)

lcSouthAmerica <- raster('lcSouthAmericaRc')

# Project to leaflet projection:

lcSouthAmericaPrj <- projectRaster(lcSouthAmerica, crs = "+init=epsg:3857")

values(lcSouthAmericaPrj) <- round(values(lcSouthAmericaPrj), 0)

#----------------------------------------------------------------------------------------*
# ---- Raster reclassification, North America ----
#----------------------------------------------------------------------------------------*

# Reclassification of rasters:

values(lcNorthAmerica)[values(lcNorthAmerica) %in% c(210, 230)] <- NA # water or no data

lcNorthAmerica <- trim(lcNorthAmerica)

values(lcNorthAmerica)[values(lcNorthAmerica) %in% c(120, 140, 60, 90, 110)] <- 1 # grassland

values(lcNorthAmerica)[values(lcNorthAmerica) %in% c(130)] <- 2 # shrub

values(lcNorthAmerica)[values(lcNorthAmerica) %in% c(11, 14, 20, 30)] <- 3 # agriculture

values(lcNorthAmerica)[values(lcNorthAmerica) %in% c(40,50, 70, 100)] <- 4 # forest

values(lcNorthAmerica)[values(lcNorthAmerica) %in% c(160,170,180)] <- 5 # wetland

values(lcNorthAmerica)[values(lcNorthAmerica) %in% c(190)] <- 6 # city

values(lcNorthAmerica)[values(lcNorthAmerica) %in% c(150, 200)] <- 7 # barren

values(lcNorthAmerica)[values(lcNorthAmerica) %in% c(220)] <- 8 # ice

# Write raster to file:

writeRaster(lcNorthAmerica, 'lcNorthAmericaRc', overwrite = TRUE)

lcNorthAmerica <- raster('lcNorthAmericaRc')

# Project to leaflet projection:
# 
# lcNorthAmericaPrj <- projectRaster(lcNorthAmerica, crs = "+init=epsg:3857")
# 
# values(lcNorthAmericaPrj) <- round(values(lcNorthAmericaPrj), 0)

#----------------------------------------------------------------------------------------*
# ---- Leaflet, South America ----
#----------------------------------------------------------------------------------------*

colors1 <- c('#00FF00','#808000', '#FFFF00', '#008000',
             '#0000FF','#CE33FF','#808080','#FFFFFF')

habLabels <- c(
  'grassland', 'shrub', 'agriculture', 'forest', 'wetland', 'city', 'barren', 'ice'
)

pal <- colorNumeric('Spectral', values(lcSouthAmericaPrj),
                    na.color = "transparent")

# pal <- colorNumeric(colors1, values(lcNorthAmericaPrj),
#                     na.color = "transparent")

# pal <- colorFactor(colors, values(lcSouthAmericaPrj), na.color = 'transparent')

leaflet() %>% 
  addTiles() %>%
  addRasterImage(lcSouthAmerica, colors = colors1, opacity = 1) %>%
  addLegend(values = values(lcNorthAmerica), 
            colors = pal,
            labels = habLabels,
            opacity = 1,
            title = "Habitat")

#----------------------------------------------------------------------------------------*
# ---- Leaflet, North America ----
#----------------------------------------------------------------------------------------*


colors1 <- c('#00FF00','#808000', '#FFFF00', '#008000',
             '#0000FF','#CE33FF','#808080','#FFFFFF')

habLabels <- c(
  'grassland', 'shrub', 'agriculture', 'forest', 'wetland', 'city', 'barren', 'ice'
)

pal <- colorNumeric(colors1, values(lcNorthAmerica),
                    na.color = "transparent")

# pal <- colorFactor(colors, values(lcSouthAmericaPrj), na.color = 'transparent')

leaflet() %>% 
  addTiles() %>%
  addRasterImage(lcNorthAmerica, colors = pal, opacity = .8) 

leaflet() %>% 
  addTiles() %>%
  addRasterImage(lcNorthAmerica, colors = pal, opacity = .8) %>%
  addLegend(values = values(lcNorthAmerica), 
            colors = colors1,
            labels = habLabels,
            opacity = 1,
            title = "Habitat")



leaflet() %>% 
  addTiles() %>%
  addRasterImage(lcNorthAmerica, colors = pal, opacity = .8) 

%>%
  addLegend(values = values(lcNorthAmerica), 
            pal = pal,
            labels = habLabels,
            opacity = 1,
            title = "Habitat")

pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(lcNAf),
                    na.color = "transparent")

pal <- colorNumeric('Spectral', values(lcNAf),
                    na.color = "transparent")

leaflet() %>% addTiles() %>%
  addRasterImage(lcNAf, colors = pal, opacity = 0.8) %>%
  addLegend(pal = pal, values = values(lcNAf),
            title = "Surface temp")







