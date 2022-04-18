#/////////////////////////
# Using plantTracker to generate Demographic Data for all chart-quadrat sites
# Alice Stears
# 28 February 2022
#/////////////////////////

# Load Packages -----------------------------------------------------------
library(tidyverse)
library(sf)
library(devtools)
devtools::install_github("aestears/plantTracker", force = TRUE)
library(plantTracker)

# Load Data ---------------------------------------------------------------
# read in d.f that contains all shapefiles (Megashape), saved as an .rdata object
#megaShape <- st_read(dsn = "/Users/Alice/Dropbox/Grad School/Research/Trait Project/Data/QuadratShapefiles/MegaShape.gpkg")
load("../../MegaShapeEnvironment.RData")
# temporarily make it tiny:
megaShape <- bigShape4
# megaShape <- megaShape[1:1000,]
# make sure it is in the correct sf format
megaShape <- st_as_sf(megaShape)
#names(megaShape)[11] <- "geometry"
#st_geometry(megaShape) <- "geometry"
# fix some 'invalid' geometries
megaShape <- st_make_valid(megaShape)
# remove rows that don't have species name info
megaShape <- megaShape[!is.na(megaShape$Species),]
# correct year format
megaShape$Year <- as.integer(megaShape$Year)
# for years where only two last digits are stored... add 1900
megaShape[megaShape$Year < 1900,]$Year <- megaShape[megaShape$Year < 1900,]$Year + 1900
# remove data for things that aren't plants
megaShape <- megaShape[!(megaShape$Species %in% c("ant hill", "Ant hill", "bare ground",
                                    "Corner plate", "Cow pie", "Crown", "Bare ground", "Unknown Moss", "Unknown Weed", "Unknown Lichen", "Fragment", "Cymopterus spp.","Stipa spp.","Malva spp.","Cirsium spp.", "Viola spp.","Carduus spp.","Unknown Perennial", "Andropogon spp.", "Dichanthelium spp.", "depression", "dung", "fragment", "Mixed grass", "Moss", "Mushroom", "Short grass", "Unknown", "unknown", "Unknown Cactus", "unknown cactus", "Unknown forb", "Unknown Forb", "unknown forb", "Unknown Grass", "unknown grass", "Unknown grass", "unknown lichen", "unknown moss", "unknown perennial", "Unknown Seedling", "unknown seedling", "Unknown Shrub", "Unknown Shrub Point", "unknown weed")),]

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
clonalDF <- read.csv("../clonalDF.csv")

#### AZs data ####
tempDat <- megaShape[megaShape$Site == "AZs" ,]
tempDat <- tempDat[!duplicated(tempDat),]
AZs_demo_dorm1_buff5_buffG05 <- trackSpp(dat = tempDat, inv = inv, dorm = 1, buff = .05, buffGenet = .005, clonal = clonalDF, aggByGenet = FALSE)
## aggregate by genet, but have to do it in a loop because the dataset is too large??
for (i in 1:length(unique(tempDat$Quad))) {
  temp <- AZs_demo_dorm1_buff5_buffG05[tempDat$Quad == unique(tempDat$Quad)[i],]
  tempAgg <- aggregateByGenet(dat = temp)
  if (i == 1) {
    AZs_demo_dorm1_buff5_buffG05_aggregated <- tempAgg
  } else {
    AZs_demo_dorm1_buff5_buffG05_aggregated <- rbind(tempAgg, AZs_demo_dorm1_buff5_buffG05_aggregated)
  }
}

saveRDS(AZs_demo_dorm1_buff5_buffG05_aggregated, file = "../AZs_buff5_dorm1_demoDat.RDS")

#### CO data ####
tempDat <- megaShape[megaShape$Site == "CO" ,]
# remove a duplicate
tempDat <- tempDat[!duplicated(tempDat),]
CO_demo_dorm1_buff5_buffG05 <- trackSpp(dat = tempDat, inv = inv, dorm = 1, buff = .05, buffGenet = .005, clonal = clonalDF, aggByGenet = FALSE)

# remove the observations that have an area of '0'
badTrackIDs <- CO_demo_dorm1_buff5_buffG05[st_area(CO_demo_dorm1_buff5_buffG05)==0 ,]$trackID
test <- CO_demo_dorm1_buff5_buffG05[!(CO_demo_dorm1_buff5_buffG05$trackID %in% badTrackIDs),]

# memory is exhausted when doing it all at once, so loop through
for (i in 1:length(unique(test$Quad))) {
  temp <- test[test$Quad == unique(test$Quad)[i],]
  tempAgg <- aggregateByGenet(dat = temp)
  if (i == 1) {
    CO_demo_dorm1_buff5_buffG05_aggregated <- tempAgg
  } else {
    CO_demo_dorm1_buff5_buffG05_aggregated <- rbind(tempAgg,  CO_demo_dorm1_buff5_buffG05_aggregated)
  }
}

saveRDS(CO_demo_dorm1_buff5_buffG05_aggregated, file = "../CO_buff5_dorm1_demoDat.RDS")

#### ID data ####
tempDat <- megaShape[megaShape$Site == "ID" ,]
# remove a duplicate
tempDat <- tempDat[!duplicated(tempDat),]
ID_demo_dorm1_buff5_buffG05 <- trackSpp(dat = tempDat, inv = inv, dorm = 1, buff = .05, buffGenet = .005, clonal = clonalDF, aggByGenet = FALSE)

## aggregate by genet
for (i in 1:length(unique(tempDat$Quad))) {
  temp <- ID_demo_dorm1_buff5_buffG05[ID_demo_dorm1_buff5_buffG05$Quad == unique(tempDat$Quad)[i],]
  tempAgg <- aggregateByGenet(dat = temp)
  if (i == 1) {
    ID_demo_dorm1_buff5_buffG05_aggregated <- tempAgg
  } else {
    ID_demo_dorm1_buff5_buffG05_aggregated <- rbind(tempAgg, ID_demo_dorm1_buff5_buffG05_aggregated)
  }
}

saveRDS(ID_demo_dorm1_buff5_buffG05_aggregated, file = "../ID_buff5_dorm1_demoDat.RDS")

#### MT data ####
tempDat <- megaShape[megaShape$Site == "MT" ,]
# remove a duplicate
tempDat <- tempDat[!duplicated(tempDat),]
MT_demo_dorm1_buff5_buffG05 <- trackSpp(dat = tempDat, inv = inv, dorm = 1, buff = .05, buffGenet = .005, clonal = clonalDF, aggByGenet = FALSE)

## aggregate by genet
for (i in 1:length(unique(tempDat$Quad))) {
  temp <- MT_demo_dorm1_buff5_buffG05[MT_demo_dorm1_buff5_buffG05$Quad == unique(tempDat$Quad)[i],]
  tempAgg <- aggregateByGenet(dat = temp)
  if (i == 1) {
    MT_demo_dorm1_buff5_buffG05_aggregated <- tempAgg
  } else {
    MT_demo_dorm1_buff5_buffG05_aggregated <- rbind(tempAgg, MT_demo_dorm1_buff5_buffG05_aggregated)
  }
}

saveRDS(MT_demo_dorm1_buff5_buffG05_aggregated, file = "../MT_buff5_dorm1_demoDat.RDS")

#### KS data ####
tempDat <- megaShape[megaShape$Site == "KS" ,]
# remove a duplicate
tempDat <- tempDat[!duplicated(tempDat),]
KS_demo_dorm1_buff5_buffG05 <- trackSpp(dat = tempDat, inv = inv, dorm = 1, buff = .05, buffGenet = .005, clonal = clonalDF, aggByGenet = FALSE)

## aggregate by genet
for (i in 1:length(unique(tempDat$Quad))) {
  temp <- KS_demo_dorm1_buff5_buffG05[KS_demo_dorm1_buff5_buffG05$Quad == unique(tempDat$Quad)[i],]
  tempAgg <- aggregateByGenet(dat = temp)
  if (i == 1) {
    KS_demo_dorm1_buff5_buffG05_aggregated <- tempAgg
  } else {
    KS_demo_dorm1_buff5_buffG05_aggregated <- rbind(tempAgg,KS_demo_dorm1_buff5_buffG05_aggregated)
  }
}

saveRDS(KS_demo_dorm1_buff5_buffG05_aggregated, file = "../KS_buff5_dorm1_demoDat.RDS")

#### NM data ####
tempDat <- megaShape[megaShape$Site == "NM" ,] %>%
  mutate(Year = as.integer(Year), Quad = str_to_upper(Quad))
## change the appropriate years for the NM dataset
projectYears <- read.csv("../../cross_site_analysis/Jornada_quadrat_sampling_dates.csv") %>%
  select(-X) %>%
  mutate(quadrat = str_to_upper(quadrat))

tempDat_new <- tempDat %>%
  left_join(projectYears, by = c("Year" = "year", "Quad" = "quadrat", "Month" = "month")) %>%
  mutate(Year = project_year) %>%
  select(-c(day, project_year))

# rescale the geometry to be on a scale of 1m
tempDat_new$geometry <- tempDat_new$geometry/max(st_bbox(tempDat_new))
tempDat_new$Area <- st_area(tempDat_new$geometry)

# remove a duplicate
tempDat <- tempDat[!duplicated(tempDat),]
names(inv) <- str_to_upper(names(inv))
NM_demo_dorm1_buff5_buffG05 <- trackSpp(dat = tempDat, inv = inv, dorm = 1, buff = .05, buffGenet = .005, clonal = clonalDF, aggByGenet = FALSE)

## aggregate by genet
for (i in 1:length(unique(tempDat$Quad))) {
  temp <- NM_demo_dorm1_buff5_buffG05[NM_demo_dorm1_buff5_buffG05$Quad == unique(tempDat$Quad)[i],]
  tempAgg <- aggregateByGenet(dat = temp)
  if (i == 1) {
    NM_demo_dorm1_buff5_buffG05_aggregated <- tempAgg
  } else {
    NM_demo_dorm1_buff5_buffG05_aggregated <- rbind(tempAgg,NM_demo_dorm1_buff5_buffG05_aggregated)
  }
}

saveRDS(NM_demo_dorm1_buff5_buffG05_aggregated, file = "../NM_buff5_dorm1_demoDat.RDS")

#### AZn data ####
AZn_all <- readRDS("../../AZn_rawData/AZn_readyToGo.RDS")

# make quadrat inventory list
invTemp <- unique(st_drop_geometry(AZn_all[,c( "Quad", "Year")]))
names(invTemp)[2] <- "Year_name"
invTemp$Year_value <- invTemp$Year
invTemp <- invTemp[order(invTemp$Year_name),]
invTemp <- pivot_wider(invTemp, names_from = Quad, values_from = "Year_value")
invTemp <- invTemp[,2:ncol(invTemp)]
inv <- lapply(X = invTemp, FUN = function(x) x[!is.na(x)])

# there are some rows that have a geometry of 0... should remove these
AZn_all <- AZn_all[units::drop_units(st_area(AZn_all)) != 0,]

AZn_demo_dorm1_buff5_buffG05 <- trackSpp(dat = AZn_all, inv = inv,  dorm = 1, buff = .05, buffGenet = .005,
          clonal = data.frame("Species" = unique(AZn_all$Species),
                              "clonal" = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,1,1,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0))
                                           , aggByGenet = FALSE)

## aggregate by genet
for (i in 1:length(unique(AZn_all$Quad))) {
  temp <- AZn_demo_dorm1_buff5_buffG05[AZn_demo_dorm1_buff5_buffG05$Quad == unique(AZn_all$Quad)[i],]
  tempAgg <- aggregateByGenet(dat = temp)
  if (i == 1) {
    AZn_demo_dorm1_buff5_buffG05_aggregated <- tempAgg
  } else {
    AZn_demo_dorm1_buff5_buffG05_aggregated <- rbind(tempAgg,AZn_demo_dorm1_buff5_buffG05_aggregated)
  }
}

saveRDS(AZn_demo_dorm1_buff5_buffG05_aggregated, file = "../AZn_buff5_dorm1_demoDat.RDS")


#### get demographic data only for spp. we have trait data for ####
## read in demographic data and remove spatial data
AZn_demoDat <- readRDS("../trackSpp_outputData/AZn_buff5_dorm1_demoDat.RDS") %>%
  st_drop_geometry() %>%
  mutate("sp_code_6" = str_to_upper(paste0(str_sub(Species,
                                start = 1,
                               end = 3),
                              str_sub(Species,
                                      start = (str_locate(Species, " ")[,1] + 1),
                                      end = (str_locate(Species, " ")[,1] + 3))
         )))

str_locate(AZn_demoDat$Species, " ")

AZs_demoDat <- readRDS("../trackSpp_outputData/AZs_buff5_dorm1_demoDat.RDS") %>%
  st_drop_geometry() %>%
  mutate("sp_code_6" = str_to_upper(paste0(str_sub(Species,
                                                   start = 1,
                                                   end = 3),
                                           str_sub(Species,
                                                   start = (str_locate(Species, " ")[,1] + 1),
                                                   end = (str_locate(Species, " ")[,1] + 3))
  )))
CO_demoDat <- readRDS("../trackSpp_outputData/CO_buff5_dorm1_demoDat.RDS") %>%
  st_drop_geometry() %>%
  mutate("sp_code_6" = str_to_upper(paste0(str_sub(Species,
                                                   start = 1,
                                                   end = 3),
                                           str_sub(Species,
                                                   start = (str_locate(Species, " ")[,1] + 1),
                                                   end = (str_locate(Species, " ")[,1] + 3))
  )))
ID_demoDat <- readRDS("../trackSpp_outputData/ID_buff5_dorm1_demoDat.RDS") %>%
  st_drop_geometry() %>%
  mutate("sp_code_6" = str_to_upper(paste0(str_sub(Species,
                                                   start = 1,
                                                   end = 3),
                                           str_sub(Species,
                                                   start = (str_locate(Species, " ")[,1] + 1),
                                                   end = (str_locate(Species, " ")[,1] + 3))
  )))
MT_demoDat <- readRDS("../trackSpp_outputData/MT_buff5_dorm1_demoDat.RDS") %>%
  st_drop_geometry() %>%
  mutate("sp_code_6" = str_to_upper(paste0(str_sub(Species,
                                                   start = 1,
                                                   end = 3),
                                           str_sub(Species,
                                                   start = (str_locate(Species, " ")[,1] + 1),
                                                   end = (str_locate(Species, " ")[,1] + 3))
  )))
KS_demoDat <- readRDS("../trackSpp_outputData/KS_buff5_dorm1_demoDat.RDS") %>%
  st_drop_geometry() %>%
  mutate("sp_code_6" = str_to_upper(paste0(str_sub(Species,
                                                   start = 1,
                                                   end = 3),
                                           str_sub(Species,
                                                   start = (str_locate(Species, " ")[,1] + 1),
                                                   end = (str_locate(Species, " ")[,1] + 3))
  )))
NM_demoDat <- readRDS("../trackSpp_outputData/NM_buff5_dorm1_demoDat.RDS") %>%
  st_drop_geometry() %>%
  mutate("sp_code_6" = str_to_upper(paste0(str_sub(Species,
                                                   start = 1,
                                                   end = 3),
                                           str_sub(Species,
                                                   start = (str_locate(Species, " ")[,1] + 1),
                                                   end = (str_locate(Species, " ")[,1] + 3))
  )))

# get trait data all-site means d.f
traits_AllMeans <- read.csv( "../../Data/ProcessedTraitData/Traits_allSitesMeans.csv")
traitSppNames <- unique(traits_AllMeans$Species)

## get the species names of all the species in demographic data
demoSpp <- unique(c(AZn_demoDat$Species, AZs_demoDat$Species, CO_demoDat$Species, MT_demoDat$Species, KS_demoDat$Species, NM_demoDat$Species, ID_demoDat$Species))
#write.csv(demoSpp, "../analysis_data/demoData_speciesNames.csv")

## read in the .csv that gives the demographic data spp. names, the trait data spp. names, and whether to use each species
sppNamesKey <- read.csv("../analysis_data/demoData_speciesNames.csv")

## put all demographic data together into one d.f
demoDat <- rbind(AZn_demoDat, AZs_demoDat, CO_demoDat, NM_demoDat, KS_demoDat, MT_demoDat, ID_demoDat)

## remove data for species that won't be included in future analysis
demoDat <- demoDat[demoDat$Species %in% c(sppNamesKey[sppNamesKey$Use== 1,"DemographicData_names"]),]

## get a list of the site/species combos for which we need trait data
testTraitMat <- unique(demoDat[,c("Site", "Species")])
testTraitMat$Height_cm <- NA
testTraitMat$SLA_cm2_g <- NA
testTraitMat$LDMC_g_g <- NA
testTraitMat$RDMC <- NA
testTraitMat$RTD_g_cm3 <- NA
testTraitMat$rootAvgDiam_mm <- NA
testTraitMat$SRL_m_g <- NA
testTraitMat$TLP <- NA

traitMat <- testTraitMat %>%
  pivot_longer(cols = c("Height_cm", "SLA_cm2_g", "LDMC_g_g",
                        "RDMC", "RTD_g_cm3", "rootAvgDiam_mm", "SRL_m_g","TLP"), names_to = "trait_name", values_to = "trait_value")

## prepare to add trait data to the matrix
traitMat <- traitMat %>%
  left_join(y = data.frame("Site" = unique(traitMat$Site), "Location" = c("Flagstaff", "Santa Rita", "CPER", "Jornada", "Hays", "Ft. Keogh", "Sheep Station"))) %>%
  select(Site, Location, Species, trait_name)

## add the 'trait data' species name for those species w/ different names in each dataset
traitMat <- traitMat %>%
  left_join(sppNamesKey[,c("DemographicData_names", "TraitNames")], by = c("Species" = "DemographicData_names")) %>%
  rename(Species_traitDF = TraitNames, Species_demoDF = Species) %>%
  mutate(sp_code_6 = str_to_upper(paste0(str_sub(Species_traitDF, start = 1, end = 3),
         str_sub(Species_traitDF,
                 start =  (str_locate(Species_traitDF, " ")[,1]+1),
                 end = (str_locate(Species_traitDF, " ")[,1]+3))
         )))


#### %%% AES still need to fix the trait values from Daniel... not sure what the units are

## join with trait data
# load updated trait matrix from file
traitCSV <- read.csv("../analysis_data/traitMatrix_allSiteAnalysis_updated.csv")
traitMat_final <- traitMat %>%
  left_join(traitCSV[,c("Location", "Species", "trait_name", "trait_value", "TraitFromOtherSource_Location")], by = c("Location" = "Location", "sp_code_6" = "Species", "trait_name"= "trait_name")) %>%
  mutate(trait_value = as.numeric(trait_value))

## save to file
#write.csv(traitMat_final, file = "../analysis_data/traitMatrix_final.csv", row.names = FALSE)
