#/////////////////////////
# Using plantTracker to generate Demographic Data for all chart-quadrat sites
# Alice Stears
# 28 February 2022
#/////////////////////////


# Load Packages -----------------------------------------------------------
library(tidyverse)
library(sf)
library(devtools)
# devtools::install_github("aestears/plantTracker")
library(plantTracker)

# Load Data ---------------------------------------------------------------
# read in d.f that contains all shapefiles (Megashape), saved as an .rdata object
megaShape <- st_read(dsn = "/Users/astears/MegaShape.gpkg")
# make sure it is in the correct sf format
megaShape <- st_as_sf(megaShape)
names(megaShape)[11] <- "geometry"
st_geometry(megaShape) <- "geometry"
# fix some 'invalid' geometries
megaShape <- st_make_valid(megaShape)
# remove rows that don't have species name info
megaShape <- megaShape[!is.na(megaShape$Species),]
# correct year format
megaShape$Year <- as.integer(megaShape$Year)
# for years where only two last digits are stored... add 1900
megaShape[megaShape$Year < 1900,]$Year <- megaShape[megaShape$Year < 1900,]$Year + 1900
# remove data for things that aren't plants
megaShape[megaShape$Species !%in% c("ant hill", "Ant hill", "bare ground", 
                                    "Corner plate", "Cow pie", "Crown", 
                                    "depression", "dung", "fragment", "Mixed grass", "Moss", "Mushroom")]

# make quadrat inventory list
invTemp <- unique(st_drop_geometry(megaShape[,c( "Quad", "Year")]))
names(invTemp)[2] <- "Year_name"
invTemp$Year_value <- invTemp$Year
invTemp <- invTemp[order(invTemp$Year_name),]
invTemp <- pivot_wider(invTemp, names_from = Quad, values_from = "Year_value")
invTemp <- invTemp[,2:ncol(invTemp)]
inv <- lapply(X = invTemp, FUN = function(x) x[!is.na(x)])

# trackSpp ----------------------------------------------------------------
# check the data w/ checkDat
checkDat(dat = megaShape, inv = inv)

# clonal argument data.frame
clonalDF <- data.frame("Species" = unique(megaShape$Species), "clonal" )
trackOut_dorm1_buff5

