#/////////////////////////
# Prepare Trait Data for Analysis
# Alice Stears
# 21 March 2022
#/////////////////////////

# Load Packages -----------------------------------------------------------
library(tidyverse)

# Load Data ---------------------------------------------------------------
## raw root scan data
rootScan_1 <- read.delim("../../Data/Raw Trait Data/rawData_forAnalysis/rootScanData/WinRhizoResults.TXT", sep = "\t") %>%
  dplyr::select(RHIZO.2017a, AnalysisDateTime, ImageFileName, Length.cm., SurfArea.cm2., AvgDiam.mm., RootVolume.cm3.) %>%
  rename(Analysis.Date.Time = AnalysisDateTime)

rootScan_2 <- read.delim("../../Data/Raw Trait Data/rawData_forAnalysis/rootScanData/WinRhizoResults_2.TXT", sep = "\t") %>%
  dplyr::select(RHIZO.2017a, Analysis.Date.Time, ImageFileName, Length.cm., SurfArea.cm2., AvgDiam.mm., RootVolume.cm3.) %>%
  slice(5:172)

rootScan_3 <- read.delim("../../Data/Raw Trait Data/rawData_forAnalysis/rootScanData/WinRhizoResults_3.TXT", sep = "\t") %>%
  dplyr::select(RHIZO.2017a, Analysis.Date.Time, ImageFileName, Length.cm., SurfArea.cm2., AvgDiam.mm., RootVolume.cm3.) %>%
  slice(5:16)

rootScan_4 <- read.delim("../../Data/Raw Trait Data/rawData_forAnalysis/rootScanData/WinRhizoResults_4.TXT", sep = "\t") %>%
  dplyr::select(RHIZO.2017a, Analysis.Date.Time, ImageFileName, Length.cm., SurfArea.cm2., AvgDiam.mm., RootVolume.cm3.) %>%
  slice(5:250)

rootScan <- rbind(rootScan_1, rootScan_2, rootScan_3, rootScan_4)

## raw trait data matrix
rawTraits <- read.csv(
  "../../Data/Raw Trait Data/rawData_forAnalysis/AllTraits_7_26_21.csv")

## raw leaf area data
rawLeafArea <- read.csv(
  "../../Data/Raw Trait Data/rawData_forAnalysis/allSite_leafArea.csv")

## raw LOP data
rawLOP <- read.csv(
  "../../Data/Raw Trait Data/rawData_forAnalysis/LOP_complete.csv"
)

# Root data ---------------------------------------------------------------
# get the sample names for each scan
rootScan$sampleID_temp <- sapply(X = str_split(string = rootScan$ImageFileName,pattern = "_"), FUN = function(x) paste0(x[1], "_", x[2]))

rootScan$sampleID<- str_to_upper(
  str_sub(
    string = rootScan$sampleID_temp,
    str_locate(string = rootScan$sampleID_temp, pattern = ".*_[[:digit:]]*")))

rootScan$thresh <- str_to_upper(rootScan$RHIZO.2017a)
rootScan$thresh[229:474] <- "AUTO"
rootScan$thresh[217:228] <- str_extract(string = rootScan$thresh[217:228],
                                             pattern = "[[:alnum:]]*$")

# for those with 'best threshhold' data, subset the rootScan df to include only the measurements w/ an 'AUTO' threshhold
rootScan <- rootScan %>%
  filter(thresh == "AUTO") %>%
  rename("rootSurfaceArea_cm2" = SurfArea.cm2., "rootLength_cm" = Length.cm. , "rootAvgDiam_mm" = AvgDiam.mm., "rootVolume_cm3" = RootVolume.cm3.) %>%
  select(-c(sampleID_temp))

## are there duplicates in sampleID in the root data?
dupIDs <- rootScan[duplicated(rootScan$sampleID),"sampleID"]
rootScan[rootScan$sampleID %in% dupIDs,]
# remove the duplicates that we don't want
rootScan <- rootScan %>%
  filter(!(Analysis.Date.Time %in% c("3/9/2022 15:32:22", "3/10/2022 11:00:16", "3/10/2022 11:07:36", "3/11/2022 13:14:34", "3/11/2022 13:18:45", "3/11/2022 14:01:31", "3/10/2019 12:22:58")) & rootLength_cm != 14.3463) %>%
  mutate(rootVolume_cm3 = as.numeric(rootVolume_cm3), rootAvgDiam_mm = as.numeric(rootAvgDiam_mm), rootLength_cm = as.numeric(rootLength_cm), rootSurfaceArea_cm2 = as.numeric(rootSurfaceArea_cm2))

## add rootScan data to the traits df
# remove previous root columns that will be replaced
traits <- rawTraits %>%
  select(-c("Length_cm_10", "Length_cm_20", "Length_cm_AUTO",
            "AvgDiam_mm_10", "AvgDiam_mm_20", "AvgDiam_mm_AUTO", "RootVolume_cm3_10",     "RootVolume_cm3_20", "RootVolume_cm3_AUTO", "root_length_m")) %>%
  left_join(rootScan[,c("rootSurfaceArea_cm2", "rootLength_cm", "rootAvgDiam_mm", "rootVolume_cm3", "sampleID")])

# calculate root traits (RDMC, RTD, SRL)
# RDMC (g/g)
traits$RDMC <- traits$Root_Dry_Mass_g / traits$Root_Wet_Mass_g
# SRL (m/g)
traits$SRL_m_g <- (traits$rootLength_cm/100) / traits$Root_Dry_Mass_g
#RTD (g/cm3)
traits$RTD_g_cm3 <- traits$Root_Dry_Mass_g / traits$rootVolume_cm3

#remove values for SRL that are larger than 350 (observed global maximum)
traits[traits$SRL_m_g > 500 & is.na(traits$SRL_m_g) == FALSE, "SRL_m_g"] <- NA

# TLP data ----------------------------------------------------------------
LOP <- rawLOP %>%
  select(OsmoticPotential_MPa, sampleID)
# deal w/ duplicates
dupIDs <- LOP[duplicated(LOP$sampleID), "sampleID"]
LOP[LOP$sampleID %in% dupIDs,]
# get the mean of each of the duplicated values
LOP <- LOP %>%
  group_by(sampleID) %>%
  summarize(OsmoticPotential_MPa = mean(OsmoticPotential_MPa))
# add to traits df
traits <- traits %>%
  select(-OsmoticPotential_MPa) %>%
  left_join(LOP)

## calculate TLP (independently for graminoids and forbs)
#for forbs
#TLP = 0.80 * LOP − 0.845
# for graminoids
#TLP = 0.994 * LOP - 0.611

for (i in 1:nrow(traits)) {
  if (traits$GF[i] == "G" & is.na(traits$GF[i]) == FALSE) {
    traits$TLP[i] <- 0.994 * traits$OsmoticPotential_MPa[i] - 0.611
  } else if (traits$GF[i] == "F" & is.na(traits$GF[i]) == FALSE) {
    traits$TLP[i] <- 0.80 * traits$OsmoticPotential_MPa[i] - 0.845
  }
}

# Leaf Area Data ----------------------------------------------------------
## get the Extra Flagstaff data
flagDat <- read.csv("../../Data/Raw Trait Data/FlagstaffTraitData.csv") %>%
  mutate(sampleID = paste0(Species, "_", Alice_ReplicateID))

leafArea <- rawLeafArea %>%
  select(-c(X, X.1, X.2, X.3)) %>%
  rename(LeafArea_cm2 = "Leaf.Area..cm2..ImageJ") %>%
  mutate(sampleID = paste0(ï..Species, "_", Replicate))
# update the 'rolled leaves' column to have no "NA"s
leafArea[leafArea$LvsRolled != 1 | is.na(leafArea$LvsRolled), "LvsRolled"] <- 0

## add to 'traits' d.f
traits <- traits %>%
  select(-c("LvsRolled", "Leaf_Area_cm2")) %>%
  left_join(leafArea[,c("LeafArea_cm2", "LvsRolled", "sampleID")], by = "sampleID")

## transform leaf area for rolled leaves (x3)
traits$Leaf_Area_TRANSFORMED <- traits$LeafArea_cm2
traits[traits$LvsRolled == 1 & is.na(traits$LvsRolled) == FALSE, "Leaf_Area_TRANSFORMED"] <-
  traits[traits$LvsRolled == 1 & is.na(traits$LvsRolled) == FALSE, "LeafArea_cm2"] * 3

### add Flagstaff data to the 'traits' d.f
# remove duplicate flag data
flagDat <- flagDat[!duplicated(flagDat$sampleID),]
names <- inner_join(data.frame("sampleID" = traits$sampleID, "type_A" = "traits"),
                    data.frame("sampleID" = flagDat$sampleID, "type_b" = "flag")) %>%
  select(sampleID)

traits[traits$sampleID %in% names$sampleID,
       c("Height_cm", "Leaf_Wet_Mass_g", "Leaf_Dry_Mass_g", "Number_Leaves", "TLP")] <- flagDat[flagDat$sampleID %in%  names$sampleID,c( "Height_cm", "Leaf_Wet_Mass_g", "Leaf_Dry_Mass_g", "Number_Leaves", "TLP")]

## calculate SLA (cm2/g)
traits$SLA_cm2_g <- traits$Leaf_Area_TRANSFORMED / traits$Leaf_Dry_Mass_g
# check for SLA values that are too big
traits[traits$SLA_cm2_g > 1000 & is.na(traits$SLA_cm2_g)==FALSE, "SLA_cm2_g"] <- NA

traits$LDMC_g_g <- traits$Leaf_Dry_Mass_g / traits$Leaf_Wet_Mass_g
# check for SLA values that are too big
traits[traits$LDMC_g_g > 7 & is.na(traits$LDMC_g_g)==FALSE, "LDMC_g_g"] <- NA

# Calculate mean trait values  --------------------------------------------
## for each site
traits_SiteMeans <- traits %>%
  group_by(Location, Species) %>%
  summarize( n_height = sum(is.na(Height_cm)==FALSE),
             Height_cm = mean(Height_cm, na.rm = TRUE),
             n_SLA = sum(is.na(SLA_cm2_g) == FALSE),
             SLA_cm2_g = mean(SLA_cm2_g, na.rm = TRUE),
             n_LDMC = sum(is.na(LDMC_g_g) == FALSE),
             LDMC_g_g = mean(LDMC_g_g, na.rm = TRUE),
             n_RDMC = sum(is.na(RDMC) == FALSE),
             RDMC = mean(RDMC, na.rm = TRUE),
             n_RTD= sum(is.na(RTD_g_cm3) == FALSE),
             RTD_g_cm3 = mean(RTD_g_cm3, na.rm = TRUE),
             n_RDiam = sum(is.na(rootAvgDiam_mm) == FALSE),
             rootAvgDiam_mm = mean(rootAvgDiam_mm, na.rm = TRUE),
             n_SRL = sum(is.na(SRL_m_g) == FALSE),
             SRL_m_g = mean(SRL_m_g, na.rm = TRUE),
             n_TLP = sum(is.na(TLP) == FALSE),
             TLP = mean(TLP, na.rm = TRUE))

## across all sites
traits_AllMeans <- traits %>%
  group_by( Species) %>%
  summarize( n_height = sum(is.na(Height_cm)==FALSE),
             Height_cm = mean(Height_cm, na.rm = TRUE),
             n_SLA = sum(is.na(SLA_cm2_g) == FALSE),
             SLA_cm2_g = mean(SLA_cm2_g, na.rm = TRUE),
             n_LDMC = sum(is.na(LDMC_g_g) == FALSE),
             LDMC_g_g = mean(LDMC_g_g, na.rm = TRUE),
             n_RDMC = sum(is.na(RDMC) == FALSE),
             RDMC = mean(RDMC, na.rm = TRUE),
             n_RTD= sum(is.na(RTD_g_cm3) == FALSE),
             RTD_g_cm3 = mean(RTD_g_cm3, na.rm = TRUE),
             n_RDiam = sum(is.na(rootAvgDiam_mm) == FALSE),
             rootAvgDiam_mm = mean(rootAvgDiam_mm, na.rm = TRUE),
             n_SRL = sum(is.na(SRL_m_g) == FALSE),
             SRL_m_g = mean(SRL_m_g, na.rm = TRUE),
             n_TLP = sum(is.na(TLP) == FALSE),
             TLP = mean(TLP, na.rm = TRUE))

# write to file -----------------------------------------------------------
# each replicate d.f
write.csv(traits, "../../Data/Raw Trait Data/rawData_forAnalysis/Traits_allSites_byReplicate.csv", row.names = FALSE)
# site-level trait means d.f
write.csv(traits_SiteMeans, "../../Data/ProcessedTraitData/Traits_SiteMeans.csv", row.names = FALSE)
# all sites trait means d.f
write.csv(traits_AllMeans, "../../Data/ProcessedTraitData/Traits_allSitesMeans.csv", row.names = FALSE)

# Make big trait matrix for further analysis ------------------------------

tempSiteTraits <- traits_SiteMeans
tempSiteTraits[tempSiteTraits$n_height < 3,"Height_cm"] <- NA
tempSiteTraits[tempSiteTraits$n_LDMC <3, "LDMC_g_g"] <- NA
tempSiteTraits[tempSiteTraits$n_SLA <3, "SLA_cm2_g"] <- NA
tempSiteTraits[tempSiteTraits$n_RDMC <3, "RDMC"] <- NA
tempSiteTraits[tempSiteTraits$n_RTD <3, "RTD_g_cm3"] <- NA
tempSiteTraits[tempSiteTraits$n_RDiam <3, "rootAvgDiam_mm"] <- NA
tempSiteTraits[tempSiteTraits$n_SRL <3, "SRL_m_g"] <- NA
tempSiteTraits[tempSiteTraits$n_TLP <3, "TLP"] <- NA

tempAllTraits <- traits_AllMeans
tempAllTraits[tempAllTraits$n_height < 5,"Height_cm"] <- NA
tempAllTraits[tempAllTraits$n_LDMC <5, "LDMC_g_g"] <- NA
tempAllTraits[tempAllTraits$n_SLA <5, "SLA_cm2_g"] <- NA
tempAllTraits[tempAllTraits$n_RDMC <3, "RDMC"] <- NA
tempAllTraits[tempAllTraits$n_RTD <3, "RTD_g_cm3"] <- NA
tempAllTraits[tempAllTraits$n_RDiam <3, "rootAvgDiam_mm"] <- NA
tempAllTraits[tempAllTraits$n_SRL <3, "SRL_m_g"] <- NA
tempAllTraits[tempAllTraits$n_TLP <3, "TLP"] <- NA

tempAllTraits$Location <- "AllSiteMean"

# reorder the columns
tempAllTraits <- tempAllTraits[,names(tempSiteTraits)]

# put the two d.fs together
allTraitValues <- rbind(tempAllTraits, tempSiteTraits)

# make a 'long' format
allTrait_temp1 <- allTraitValues %>%
  select(-c("n_height", "n_SLA", "n_LDMC", "n_RDMC", "n_RTD", "n_RDiam", "n_SRL", "n_TLP")) %>%
  pivot_longer(cols = c("Height_cm", "SLA_cm2_g", "LDMC_g_g",
                        "RDMC", "RTD_g_cm3", "rootAvgDiam_mm", "SRL_m_g","TLP"),
               names_to = "trait_name",
               values_to = "trait_value",
               values_drop_na = FALSE)

allTrait_temp2 <- allTraitValues %>%
    select(-c("Height_cm", "SLA_cm2_g", "LDMC_g_g",
              "RDMC", "RTD_g_cm3", "rootAvgDiam_mm", "SRL_m_g","TLP")) %>%
  pivot_longer(cols = c("n_height", "n_SLA", "n_LDMC", "n_RDMC", "n_RTD",
                        "n_RDiam", "n_SRL", "n_TLP"),
               names_to ="trait_name_TEMP",
               values_to = "trait_n",
               values_drop_na = FALSE)
# fix names for 'n'
translate <- data.frame("trait_name" = c("Height_cm", "SLA_cm2_g", "LDMC_g_g",
                                         "RDMC", "RTD_g_cm3", "rootAvgDiam_mm", "SRL_m_g","TLP"),
                        "trait_name_TEMP" = c("n_height", "n_SLA", "n_LDMC", "n_RDMC", "n_RTD", "n_RDiam", "n_SRL", "n_TLP"))

allTrait_temp2 <-  allTrait_temp2 %>%
  left_join(translate) %>%
  select(-trait_name_TEMP)

allTraitValues <- left_join(allTrait_temp1, allTrait_temp2, by = c("Location", "Species", "trait_name"))

# write to file
write.csv(allTraitValues, file = "../analysis_data/traitMatrix_allSiteAnalysis.csv", row.names = FALSE)


# Data from Other Sources -------------------------------------------------
#### get mean root data from GLEES
glees <- read.csv("../../../GLEES_TraitProject/GLEES_rootData/GLEESRootData_final.csv")
## calculate SLA
glees$SLA_cm2_g <- glees$area_cm2 / glees$leafMass_dry_g

## remove obs that have bad nots
glees <- glees[!(glees$Notes %in%  c("missing dried root sample? Not too sure what happened here, since we have the root scan", "not enough roots for analysis")), ]

# aggregate by species
gleesSpp <- glees %>%
  group_by(Species) %>%
  summarize(rootAvgDiam_mm = mean(rootAvgDiam_mm, na.rm = TRUE),
            LDMC_g_g = mean(LDMC_g_g, na.rm = TRUE),
            RDMC_g_g = mean(RDMC_g_g, na.rm = TRUE),
            SRL_cm_g = mean(SRL_cm_g, na.rm = TRUE),
            RTD_g_cm3 = mean(RTD_g_cm3, na.rm = TRUE),
            SLA_cm2_g = mean(SLA_cm2_g, na.rm = TRUE)
            )
# vector of species names present in 'traits' d.f
traitsSpp <- unique(traits$Species)
gleesSpp[gleesSpp$Species %in% traitsSpp,]

### trait data from Dana for CO site
CO_traits <- read.csv("../../Data/CO Analysis Data Files/CO_mean_traits.csv") %>%
  rename(SLA_cm2_g = SLA_adj_cm2_g, RDMC = RDMC_g_g, rootAvgDiam_mm = AvgDiam_mm, Location = trait_site)
CO_traits$Height_cm <- NA

CO_traits_long  <- CO_traits %>%
  select(-c("sp_code", "Functional_Group", "Dicot_Monocot", "Grass_Forb_Shrub", "Annual_Perennial", "C3_C4", "LeafOsmoticPotential_Mpa", "n", "SRL_best_m_g", "site", "other_name.x", "sp_code_6", "sp_code_4", "Flwr_Start", "Flwr_End", "Flwr_Total", "Flwr_Peak", "Tribe", "other_name.y")) %>%
  pivot_longer(cols = c("Height_cm", "SLA_cm2_g", "LDMC_g_g",
                        "RDMC", "RTD_g_cm3", "rootAvgDiam_mm", "SRL_m_g","TLP"),
               names_to = "trait_name",
               values_to = "trait_value",
               values_drop_na = FALSE)

write.csv(CO_traits_long, "../analysis_data/CO_traits_long.csv", row.names= FALSE)
