#/////////////////////////
# Run Models
# Alice Stears
# 26 March 2022
#/////////////////////////

# load pacakges
library(tidyverse)
library(lme4)
library(sf)
library(ggeffects)

#### load data ####
AZn_dat <- readRDS("../getNeighbors_outputData/AZn_buff10_allSpp_count.RDS")
AZs_dat <- readRDS("../getNeighbors_outputData/AZs_buff10_allSpp_count.RDS")
CO_dat <- readRDS("../getNeighbors_outputData/CO_buff10_allSpp_count.RDS")
ID_dat <- readRDS("../getNeighbors_outputData/ID_buff10_allSpp_count.RDS")
MT_dat <- readRDS("../getNeighbors_outputData/MT_buff10_allSpp_count.RDS")
NM_dat <- readRDS("../getNeighbors_outputData/NM_buff10_allSpp_count.RDS")
KS_dat <- readRDS("../getNeighbors_outputData/KS_buff10_allSpp_count.RDS")

# remove spatial data, and put all together
demoDat <- #st_drop_geometry(
  rbind(AZs_dat, CO_dat, ID_dat, MT_dat, NM_dat, KS_dat#, AZn_dat
  )#)

#### add trait data ####
# use a inner_join, since the demographic data still contains values for species that we don't want/need, but the trait matrix only has rows for the species we want/need
# read in trait data
traits <- read.csv("../analysis_data/traitMatrix_final.csv")

traits_wide <- traits %>%
  select(Site, Species_demoDF, trait_name, trait_value) %>%
  pivot_wider(id_cols = c(Site, Species_demoDF), names_from = c(trait_name), values_from = trait_value)

## add to demo data
demoDat_traits <- demoDat %>%
  inner_join(traits_wide, by = c("Site" = "Site", "Species" = "Species_demoDF"))

#### add SPEI data ####
SPEI <- read.csv("../analysis_data/SPEI_allSites.csv")

## add to demo data
demoDat_all <- demoDat_traits %>%
  left_join(SPEI, by = c("Site" = "Site", "Year" = "Year"))

#### get recruit data and prep for analysis ####
# for each species/quad/year combo
recruits <- plantTracker::getRecruits(dat = demoDatNew)
# add trait data
recruits <- recruits %>%
  left_join(traits_wide, by = c("Site", "Species" = "Species_demoDF"))
# add SPEI data
recruits <- recruits %>%
  left_join(SPEI, by = c("Site", "Year"))
# get basal area occupied in the plot the previous year
basalAreas <- getBasalAreas(dat = demoDat_all)

#### remove size for observations originally mapped as a point (size_t and size_tplus1) ####
# load megaShape file to get the 'type' column back
load("../../Data/QuadratShapefiles/MegaShapeEnvironment.RData")
# calculate the centroid coords of the bigShape4 data
bigShape_cent <- st_centroid(bigShape4)
bigShape4$X <- (sapply(bigShape_cent$geometry, function(x) x[1]))
bigShape4$Y <- (sapply(bigShape_cent$geometry, function(x) x[2]))
# calculate the centroid coords of the demoDat data
demoDat_cent <- st_centroid(demoDat_all)
demoDat_all$X <-(sapply(demoDat_cent$geometry, function(x) x[1]))
demoDat_all$Y <- (sapply(demoDat_cent$geometry, function(x) x[2]))

demoDatNew <- st_drop_geometry(demoDat_all)
bigShapeNew <- st_drop_geometry(bigShape4)

# add the 'type' column to the demoDatNew d.f (from the bigShapeNew d.f)--join by centroid
demoDatNew <- demoDatNew %>% left_join(bigShapeNew[,c("Site", "Species", "Quad", "Year", "X", "Y", "Type")],
                                       by  = c("Species", "Site", "Quad", "Year", "X", "Y"))
# for those values without a type from the left_join, add type by hand
demoDatNew[demoDatNew$basalArea_genet > 2.5-05 & is.na(demoDatNew$Type) == TRUE, "Type"] <- "poly"
demoDatNew[demoDatNew$basalArea_genet <= 2.5-05 & is.na(demoDatNew$Type) == TRUE, ] <- "point"

# remove duplicates
demoDatNew <- demoDatNew[!duplicated(demoDatNew),]

#write.csv(demoDatNew, file = "../analysis_data/dataReadyForModels.csv", row.names = FALSE)
demoDatNew <- read.csv("../analysis_data/dataReadyForModels.csv")

#### scale the predictor variables ####
demoDatNew$size_tplus1_log_s <- scale(log(demoDatNew$size_tplus1))
demoDatNew$size_t_log_S <- scale(log(demoDatNew$basalArea_genet))
demoDatNew$Year_fac <- as.factor(demoDatNew$Year)
demoDatNew$SPEI_waterYr_s <- scale(demoDatNew$SPEI_waterYr)
demoDatNew$SPEI_unique_s <- scale(demoDatNew$SPEI_unique)
demoDatNew$TLP_s <- scale(demoDatNew$TLP)
demoDatNew$LDMC_s <- scale(demoDatNew$LDMC_g_g)
demoDatNew$SLA_s <- scale(demoDatNew$SLA_cm2_g)
demoDatNew$RDMC_s <- scale(demoDatNew$RDMC)
demoDatNew$SRL_s <- scale(demoDatNew$SRL_m_g)
demoDatNew$RTD_s <- scale(demoDatNew$RTD_g_cm3)
demoDatNew$rDiam_s <- scale(demoDatNew$rootAvgDiam_mm)
demoDatNew$neighbors_count_s <- scale(as.numeric(demoDatNew$neighbors_count))
demoDatNew$nearEdge <- as.factor(demoDatNew$nearEdge)
demoDatNew$Species <- as.factor(demoDatNew$Species)
demoDatNew$Quad <- as.factor(demoDatNew$Quad)
demoDatNew$Site <- as.factor(demoDatNew$Site)

#### split data into point and polygon d.fs ####
polyDat <- demoDatNew[demoDatNew$Type == "poly",]
pointDat <- demoDatNew[demoDatNew$Type == "point",]

#### polygon survival models ####
polySurv_TLP <- glmer( survives_tplus1 ~ neighbors_count_s + size_t_log_S + nearEdge +
                         SPEI_waterYr_s * TLP_s +
                         (SPEI_waterYr_s * TLP_s | Site) +  (size_t_log_S | Species) + (1 | Quad) + (1 | Year_fac),
                      data = polyDat,
                      family = binomial,
                      control = glmerControl("bobyqa"))
saveRDS(polySurv_TLP, "../modelObjects/polygonSurvivalMod_TLP.RDS")

polySurv_LDMC <- glmer(survives_tplus1 ~ neighbors_count_s + size_t_log_S  + nearEdge +
                         SPEI_waterYr_s *  LDMC_s +
                         (SPEI_waterYr_s * LDMC_s | Site)+ (size_t_log_S | Species) + (1 | Quad) + (1 | Year),
                       data = polyDat,
                       family = binomial,
                       control = glmerControl("bobyqa"))
saveRDS(polySurv_LDMC, "../modelObjects/polygonSurvivalMod_LDMC.RDS")

polySurv_SLA <- glmer(survives_tplus1 ~ neighbors_count_s + size_t_log_S + nearEdge +
                        SPEI_waterYr_s * SLA_s +
                        (SPEI_waterYr_s * TLP_s | Site) + (size_t_log_S | Species) + (1 | Quad) + (1 | Year),
                      data = polyDat,
                      family = binomial,
                      control = glmerControl("bobyqa"))
saveRDS(polySurv_SLA, "../modelObjects/polygonSurvivalMod_SLA.RDS")

polySurv_RDMC <- glmer(survives_tplus1 ~ neighbors_count_s + size_t_log_S + nearEdge +
                         SPEI_waterYr_s * RDMC_s +
                         (SPEI_waterYr_s * TLP_s | Site) + (size_t_log_S|Species) + (1|Quad) + (1|Year),
                       data = polyDat,
                       family = binomial,
                       control = glmerControl("bobyqa"))
saveRDS(polySurv_RDMC, "../modelObjects/polygonSurvivalMod_RDMC.RDS")

polySurv_SRL <- glmer(survives_tplus1 ~ neighbors_count_s + size_t_log_S + nearEdge +
                        SPEI_waterYr_s * SRL_s +
                        (SPEI_waterYr_s * TLP_s | Site) +  (size_t_log_S | Species) (1|Quad) + (1|Year),
                      data = polyDat,
                      family = binomial,
                      control = glmerControl("bobyqa"))
saveRDS(polySurv_SRL, "../modelObjects/polygonSurvivalMod_SRL.RDS")

polySurv_rDiam <- glmer(survives_tplus1 ~ neighbors_count_s + size_t_log_S + nearEdge +
                          SPEI_waterYr_s * rDiam_s +
                          (SPEI_waterYr_s * TLP_s | Site) +  (size_t_log_S | Species) (1|Quad) + (1|Year),
                        data = polyDat,
                        family = binomial,
                        control = glmerControl("bobyqa"))
saveRDS(polySurv_rDiam, "../modelObjects/polygonSurvivalMod_rDiam.RDS")

polySurv_RTD <- glmer(survives_tplus1 ~ neighbors_count_s + size_t_log_S + SPEI_waterYr_s * RTD_s + nearEdge +
                        (SPEI_waterYr_s * TLP_s | Site) + (size_t_log_S | Species) + (1|Quad) + (1|Year),
                      data = polyDat,
                      family = binomial,
                      control = glmerControl("bobyqa"))
saveRDS(polySurv_RTD, "../modelObjects/polygonSurvivalMod_RTD.RDS")

#### polygon growth models ####
polyGrow_TLP <- lmer(size_tplus1_log_s ~ neighbors_count_s + size_t_log_S + SPEI_waterYr_s * TLP_s  + nearEdge + (SPEI_waterYr_s * TLP_s | Site) +
                       (size_t_log_S|Species) + (1|Quad) + (1|Year_fac),
                     data = polyDat,
                     control = lmerControl("bobyqa"))
saveRDS(polyGrow_TLP, "../modelObjects/polygonGrowthMod_TLP.RDS")

polyGrow_LDMC <- lmer(size_tplus1_log_s ~ neighbors_count_s + size_t_log_S + SPEI_waterYr_s * LDMC_s + nearEdge + (SPEI_waterYr_s * LDMC_s | Site) +
                        (size_t_log_S|Species) + (1|Quad) + (1|Year),
                      data = polyDat,
                      control = lmerControl("bobyqa"))
saveRDS(polyGrow_LDMC, "../modelObjects/polygonGrowthMod_LDMC.RDS")

polyGrow_SLA <- lmer(size_tplus1_log_s ~ neighbors_count_s + size_t_log_S + SPEI_waterYr_s * SLA_s + nearEdge + (SPEI_waterYr_s * SLA_s | Site) +
                       (size_t_log_S|Species) + (1|Quad) + (1|Year),
                     data = polyDat,
                     control = lmerControl("bobyqa"))
saveRDS(polyGrow_SLA, "../modelObjects/polygonGrowthMod_SLA.RDS")

polyGrow_RDMC <- lmer(size_tplus1_log_s ~ neighbors_count_s + size_t_log_S + SPEI_waterYr_s * RDMC_s  + nearEdge + (SPEI_waterYr_s * RDMC_s | Site) +
                        (size_t_log_S|Species) + (1|Quad) + (1|Year),
                      data = polyDat,
                      control = lmerControl("bobyqa"))
saveRDS(polyGrow_RDMC, "../modelObjects/polygonGrowthMod_RDMC.RDS")

polyGrow_SRL <- lmer(size_tplus1_log_s ~ neighbors_count_s + size_t_log_S + SPEI_waterYr_s * SRL_s  + nearEdge + (SPEI_waterYr_s * SRL_s | Site) +
                       (size_t_log_S|Species) + (1|Quad) + (1|Year),
                     data = polyDat,
                     control = lmerControl("bobyqa"))
saveRDS(polyGrow_SRL, "../modelObjects/polygonGrowthMod_SRL.RDS")

polyGrow_rDiam <- lmer(size_tplus1_log_s ~ neighbors_count_s + size_t_log_S + SPEI_waterYr_s * rDiam_s + nearEdge + (SPEI_waterYr_s * rDiam_s | Site) +
                         (size_t_log_S|Species) + (1|Quad) + (1|Year),
                       data = polyDat,
                       control = lmerControl("bobyqa"))
saveRDS(polyGrow_rDiam, "../modelObjects/polygonGrowthMod_rDiam.RDS")

polyGrow_RTD <- lmer(size_tplus1_log_s ~ neighbors_count_s + size_t_log_S + SPEI_waterYr_s * RTD_s  + nearEdge + (SPEI_waterYr_s * RTD_s | Site) +
                       (size_t_log_S|Species) + (1|Quad) + (1|Year),
                     data = polyDat,
                     control = lmerControl("bobyqa"))
saveRDS(polyGrow_RTD, "../modelObjects/polygonGrowthMod_RTD.RDS")

#### point survival models ####
pointSurv_TLP <- glmer(survives_tplus1 ~ neighbors_count_s+ SPEI_waterYr_s * TLP_s  + nearEdge + (SPEI_waterYr_s * TLP_s | Site) +
                         (1|Species) + (1|Quad) + (1|Year_fac),
                       data = pointDat,
                       family = binomial,
                       control = glmerControl("bobyqa"))
saveRDS(pointSurv_TLP, "../modelObjects/pointSurvivalMod_TLP.RDS")

pointSurv_LDMC <- glmer(survives_tplus1 ~ neighbors_count_s  + SPEI_waterYr_s * LDMC_s + nearEdge + (SPEI_waterYr_s * LDMC_s | Site) +
                          (1|Species) + (1|Quad) + (1|Year),
                        data = pointDat,
                        family = binomial,
                        control = glmerControl("bobyqa"))
saveRDS(pointSurv_LDMC, "../modelObjects/pointSurvivalMod_LDMC.RDS")

pointSurv_SLA <- glmer(survives_tplus1 ~ neighbors_count_s  + SPEI_waterYr_s * SLA_s + nearEdge + (SPEI_waterYr_s * SLA_s | Site) +
                         (1|Species) + (1|Quad) + (1|Year),
                       data = pointDat,
                       family = binomial,
                       control = glmerControl("bobyqa"))
saveRDS(pointSurv_SLA, "../modelObjects/pointSurvivalMod_SLA.RDS")

pointSurv_RDMC <- glmer(survives_tplus1 ~ neighbors_count_s  + SPEI_waterYr_s * RDMC_s  + nearEdge + (SPEI_waterYr_s * RDMC_s | Site) +
                          (1|Species) + (1|Quad) + (1|Year),
                        data = pointDat,
                        family = binomial,
                        control = glmerControl("bobyqa"))
saveRDS(pointSurv_RDMC, "../modelObjects/pointSurvivalMod_RDMC.RDS")

pointSurv_SRL <- glmer(survives_tplus1 ~ neighbors_count_s  + SPEI_waterYr_s * SRL_s  + nearEdge + (SPEI_waterYr_s * SRL_s | Site) +
                         (1|Species) + (1|Quad) + (1|Year),
                       data = pointDat,
                       family = binomial,
                       control = glmerControl("bobyqa"))
saveRDS(pointSurv_SRL, "../modelObjects/pointSurvivalMod_SRL.RDS")

pointSurv_rDiam <- glmer(survives_tplus1 ~ neighbors_count_s  + SPEI_waterYr_s * rDiam_s + nearEdge + (SPEI_waterYr_s * rDiam_s | Site) +
                           (1|Species) + (1|Quad) + (1|Year),
                         data = pointDat,
                         family = binomial,
                         control = glmerControl("bobyqa"))
saveRDS(pointSurv_rDiam, "../modelObjects/pointSurvivalMod_rDiam.RDS")

pointSurv_RTD <- glmer(survives_tplus1 ~ neighbors_count_s + SPEI_waterYr_s * RTD_s  + nearEdge + (SPEI_waterYr_s * RTD_s | Site) +
                         (1|Species) + (1|Quad) + (1|Year),
                       data = pointDat,
                       family = binomial,
                       control = glmerControl("bobyqa"))
saveRDS(pointSurv_RTD, "../modelObjects/pointSurvivalMod_RTD.RDS")
