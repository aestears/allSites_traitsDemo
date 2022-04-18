#/////////////////////////
# recruitment models
# Alice Stears
# 26 March 2022
#/////////////////////////
library(tidyverse)
library(plantTracker)
library(lme4)
library(ggeffects)

demoDatNew <- read.csv("../analysis_data/dataReadyForModels.csv")
traits <- read.csv("../analysis_data/traitMatrix_final.csv")
traits_wide <- traits %>%
  select(Site, Species_demoDF, trait_name, trait_value) %>%
  pivot_wider(id_cols = c(Site, Species_demoDF), names_from = c(trait_name), values_from = trait_value)
SPEI <- read.csv("../analysis_data/SPEI_allSites.csv")

#### get recruit data and prep for analysis ####
# for each species/quad/year combo
recruits <-  getRecruits(dat = demoDatNew)
# add trait data
recruits <- recruits %>%
  left_join(traits_wide, by = c("Site", "Species" = "Species_demoDF"))
# add SPEI data
recruits <- recruits %>%
  left_join(SPEI, by = c("Site", "Year"))

# get basal area occupied in the plot the previous year
basalAreas_species <- demoDatNew %>%
  group_by(Site, Quad, Species, Year) %>%
  summarize(totalBasalArea_spp_tminus1 = sum(basalArea_genet)) %>%
  mutate(Year_tplus1 = Year +1 ) %>%
  select(-Year)

basalAreas_quad <- demoDatNew %>%
  group_by(Site, Quad,  Year) %>%
  summarize(totalBasalArea_allSpp_tminus1 = sum(basalArea_genet)) %>%
  mutate(Year_tplus1 = Year +1 )%>%
  select(-Year)

# add basal area to recruit d.f
recruits <- recruits %>% left_join(basalAreas_quad, by = c("Site", "Quad", "Year" = "Year_tplus1")) %>%
  left_join(basalAreas_species, by = c("Site", "Quad", "Species", "Year" = "Year_tplus1"))

## scale all variables
recruits$SPEI_waterYr_s <- scale(recruits$SPEI_waterYr)
recruits$Year <- as.factor(recruits$Year)
recruits$TLP_s <- scale(recruits$TLP)
recruits$LDMC_s <- scale(recruits$LDMC_g_g)
recruits$RDMC_s <- scale(recruits$RDMC)
recruits$totalBasalArea_allSpp_s <- scale(recruits$totalBasalArea_allSpp_tminus1)
recruits$totalBasalArea_spp_s <- scale(recruits$totalBasalArea_spp_tminus1)

# make models -------------------------------------------------------------
# I think use a Poisson dist.?
recMod_TLP <- glmer(recruits ~  totalBasalArea_allSpp_s + totalBasalArea_spp_s + SPEI_waterYr_s * TLP_s + (SPEI_waterYr_s * TLP_s|Site) +  (1|Quad) + (1|Year) + (1 | Species),
      data = recruits,
      family = "poisson",
      control = glmerControl("bobyqa"))
# check for overdispersion
library("blmeco")
dispersion_glmer(recMod_TLP)
# sort of high, but not too high??

recMod_LDMC <- glmer(recruits ~  totalBasalArea_allSpp_s + totalBasalArea_spp_s + SPEI_waterYr_s * LDMC_s + (SPEI_waterYr_s * LDMC_s|Site) +  (1|Quad) + (1|Year) + (1 | Species),
                    data = recruits,
                    family = "poisson",
                    control = glmerControl("bobyqa"))
recMod_RDMC <- glmer(recruits ~  totalBasalArea_allSpp_s + totalBasalArea_spp_s + SPEI_waterYr_s * RDMC_s + (SPEI_waterYr_s * RDMC_s|Site) +  (1|Quad) + (1|Year) + (1 | Species),
                    data = recruits,
                    family = "poisson",
                    control = glmerControl("bobyqa"))


# figures -----------------------------------------------------------------
#get 2.5 and 97.5 percentiles of the distribution
meanSPEI_G <- mean(unique(recruits[,c("SPEI_waterYr_s", "Site")])$SPEI_waterYr_s, na.rm = TRUE)
sdSPEI_G <- sd(unique(recruits[,c("SPEI_waterYr_s", "Site")])$SPEI_waterYr_s, na.rm = TRUE)
#get 97.5 quantile of the distribution
SPEI_97_5_G <- qnorm(.975, meanSPEI_G, sdSPEI_G)
SPEI_2_5_G <- qnorm(.025, meanSPEI_G, sdSPEI_G)

temp <- unique(recruits[,c("Site", "SPEI_waterYr_s")])
spei_vals <- c(SPEI_2_5_G, SPEI_97_5_G)
site_vals <- as.character(unique(recruits$Site))

#for TLP_s
TLP_vals <- seq(min(recruits$TLP_s, na.rm = TRUE), max(recruits$TLP_s, na.rm = TRUE), length.out = 20)
TLP_G_dat <- ggpredict(recMod_TLP, terms = c("TLP_s[TLP_vals]", "SPEI_waterYr_s[spei_vals]", "Site[site_vals]"), type = "random", back.transform = TRUE)


#make a data.frame to contain all of the values for each trait
GramDat <- data.frame(trait = c("scaled(Leaf TLP)"),
                      Site = TLP_G_dat$facet, # site identity
                      x = TLP_G_dat$x,  # TLP values
                      Recruitment = TLP_G_dat$predicted, # predicted recruitment values
                      CI_low = TLP_G_dat$conf.low, # lower boundary of confidence interval
                      CI_high = TLP_G_dat$conf.high, # upper boundary of confidence interval
                      SPEI = TLP_G_dat$group)
#make data for rug plot
RugDat_G <-  data.frame(rug = recruits$TLP_s, trait = "scaled(Leaf TLP)", Site = recruits$Site)

#make a multipanel figure that shows only the graminoid survival probs for 3 traits
recFig_TLP <- ggplot(data = GramDat) +
  #geom_ribbon(aes(x = x, ymin = CI_low, ymax = CI_high, fill = SPEI), alpha = 0.3) +
  geom_line(aes(x=x, y = Recruitment, col = SPEI))  +
  geom_rug(aes(x = rug, lty = Site), data = RugDat_G) +
  labs(title = "plot-level recruitment") +
  xlab(NULL) +
  ylab("Number of recruits (scaled)") +
  #scale_y_continuous(limits = c(0,1)) +
  scale_color_manual(labels = c("dry year", "wet year"), values = c("#fc8d62","#66c2a5"), guide = FALSE) +
  scale_fill_manual(values = c("#fc8d62","#66c2a5"), guide = FALSE) +
  facet_wrap(~Site, scales = c("free"), strip.position =  "bottom", ncol =1) +
  theme_classic()+
  theme(legend.position = "bottom", legend.title = element_blank(), legend.background = element_rect(fill="grey95",size=0.5, linetype="solid"), strip.background = element_rect(colour=NA, fill=NA), strip.placement = "outside", strip.text.x = element_text(margin = margin(0, 0, 1.5, 0)), plot.title = element_text(hjust = 0.5, size = 13, face = "bold"))
  #geom_text(data= dat_text, mapping = aes(x = x, y = y, label = label), size = 3, fontface = "bold")

