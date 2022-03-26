#/////////////////////////
# reformat AZn spatial data
# Alice Stears
# 13 March 2022
#/////////////////////////

library(tidyverse)
library(sf)

# name of file that contains all spatial data
fileName <- "../../Data/ChartQuadDatasets/AZn_downloaded data/Quadrat_Spatial_Data/"

# load density data for all sites
points <- st_read(dsn = "../../Data/ChartQuadDatasets/AZn_downloaded data/Quadrat_Spatial_Data/Shapefiles", layer = "Density_All")
# trim unnecessary columns/rename
points <- points %>%
  select(species, seedling, Site, Quadrat, z_Year, Type, geometry)

# load cover data for all sites
polys <- st_read(dsn = "../../Data/ChartQuadDatasets/AZn_downloaded data/Quadrat_Spatial_Data/Shapefiles", layer = "Cover_All")
# trim unncessary columns/rename
polys <- polys %>%
  select(species, seedling, Site, Quadrat, z_Year, Type, geometry)

## put into one big d.f
AZn_all <- rbind(points, polys)
# rename some columns
AZn_all <- AZn_all %>%
  rename(Species = species, Year = z_Year, Quad = Quadrat, subSite = Site)
AZn_all$Site <- "AZn"
AZn_all <- AZn_all %>% mutate(Year = as.numeric(Year))

# fix invalid geometries
AZn_all <- st_make_valid(AZn_all)

# it's ready! save to file
saveRDS(AZn_all, file = "../../Data/ChartQuadDatasets/AZn_downloaded data/AZn_readyToGo.RDS")
