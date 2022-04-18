#/////////////////////////
# Prepare figures for the manuscript
# Alice Stears
# 26 March 2022
#/////////////////////////


# load packages -----------------------------------------------------------
library(tidyverse)
library(stargazer)

# SPEI figure -------------------------------------------------------------
## get data from SPEI script
SPEI_all <- read.csv("../analysis_data/SPEI_allSites.csv")
# subset to have just the span of data collection
moreDat <- data.frame(Site = c("AZn", "AZs", "CO", "KS", "ID","MT", "NM"),
           Location = c("AZ: Sonoran desert", "AZ: Ponderosa", "CO: shortgrass", "KS: S. Mixed-grass", "ID: Sagebrush", "MT: S. Mixed-grass", "NM: Chihuahuan desert"),
           YearMin = c(2002, 1915, 1997, 1932, 1923, 1932, 1915),
           YearMax = c(2020, 1935, 2010, 1972, 1957, 1945, 1950))
SPEI_fig <- SPEI_all %>%
  left_join(moreDat)

## make a figure
ggplot(SPEI_fig) +
  geom_hline(aes(yintercept = 0), lty = 2, col = "grey") +
  geom_rect(aes(ymin = -2, ymax = 2, xmin = YearMin, xmax = YearMax), fill = "lightgrey", alpha = .8) +
  geom_line(aes(x = Year, y = SPEI_waterYr, col = Site)) +
  facet_wrap(~Location) +
  theme_classic() +
  scale_color_brewer(guide = "none", palette = "Dark2") +
  scale_x_continuous(limits = c(1900,2020), breaks = c(seq(from = 1900, to = 2020, by = 20))) +
  ylim(c(-2.7,2.7))


# table of trait sampling locations ---------------------------------------
traits <- read.csv("../analysis_data/traitMatrix_final.csv")
traitsWide <- traits %>%
  pivot_wider(names_from = c(trait_name),
              values_from = c(trait_value, TraitFromOtherSource_Location),
                ) %>%
  select(-Species_traitDF, -sp_code_6, -Site, -trait_value_Height_cm, -TraitFromOtherSource_Location_Height_cm)

names(traitsWide) <- c("Location", "Species", "LDMC_g_g", "RDMC_g_g" ,"SLA_cm2_g", "TLP_MPa", "RTD_g_cm3", "SRL_m_g", "rootDiam_mm", "source_LDMC", "source_RDMC" ,"source_SLA", "source_TLP", "source_RTD", "source_SRL", "source_rootDiam")

# make into a table
write.csv(traitsWide, "../figures/traitSourceTable.csv", row.names = FALSE)


# Polygon survival model figures ------------------------------------------
### figure of model results
# TLP model
#make figure for graminoid survival

#get 2.5 and 97.5 percentiles of the distribution
meanSPEI_G <- mean(polyDat$SPEI_waterYr_s, na.rm = TRUE)
sdSPEI_G <- sd(polyDat$SPEI_waterYr_s, na.rm = TRUE)
#get 97.5 quantile of the distribution
SPEI_97_5_G <- qnorm(.975, meanSPEI_G, sdSPEI_G)
SPEI_2_5_G <- qnorm(.025, meanSPEI_G, sdSPEI_G)

spei_vals <- c(SPEI_2_5_G, SPEI_97_5_G)
site_vals <- as.character(unique(polyDat$Site))

#for TLP_s
TLP_vals <- seq(min(polyDat$TLP_s, na.rm = TRUE), max(polyDat$TLP_s, na.rm = TRUE), length.out = 20)
TLP_G_dat <- ggpredict(polySurv_TLP, terms = c("TLP_s[TLP_vals]", "SPEI_waterYr_s[spei_vals]", "Site[site_vals]"), type = "fixed", back.transform = TRUE)

#make a data.frame to contain all of the values for each trait
GramDat <- data.frame(trait = c("scaled(Leaf TLP)"),
                      Site = TLP_G_dat$facet, # site identity
                      x = TLP_G_dat$x,  # TLP values
                      GramSurv = TLP_G_dat$predicted, # predicted survival values
                      CI_low = TLP_G_dat$conf.low, # lower boundary of confidence interval
                      CI_high = TLP_G_dat$conf.high, # upper boundary of confidence interval
                      SPEI = TLP_G_dat$group)
#make data for rug plot
RugDat_G <-  data.frame(rug = polyDat$TLP_s, trait = "scaled(Leaf TLP)")

#make a multipanel figure that shows only the graminoid survival probs for 3 traits
gramSurvFigure <- ggplot(data = GramDat) +
  geom_ribbon(aes(x = x, ymin = CI_low, ymax = CI_high, fill = SPEI), alpha = 0.3) +
  geom_line(aes(x=x, GramSurv, col = SPEI))  +
  geom_rug(aes(x = rug), data = RugDat_G) +
  labs(title = "Graminoid Survival") +
  xlab(NULL) +
  ylab("Probability of Survival") +
  scale_y_continuous(limits = c(0,1)) +
  scale_color_manual(labels = c("dry year", "wet year"), values = c("#fc8d62","#66c2a5"), guide = FALSE) +
  scale_fill_manual(values = c("#fc8d62","#66c2a5"), guide = FALSE) +
  facet_wrap(~Site, scales = "free_x", strip.position =  "bottom", ncol =1) +
  theme_classic()+
  theme(legend.position = "bottom", legend.title = element_blank(), legend.background = element_rect(fill="grey95",size=0.5, linetype="solid"), strip.background = element_rect(colour=NA, fill=NA), strip.placement = "outside", strip.text.x = element_text(margin = margin(0, 0, 1.5, 0)), plot.title = element_text(hjust = 0.5, size = 13, face = "bold")) +
  geom_text(data= dat_text, mapping = aes(x = x, y = y, label = label), size = 3, fontface = "bold")

## for LDMC
#for LDMC_s
LDMC_vals <- seq(min(polyDat$LDMC_s, na.rm = TRUE), max(polyDat$LDMC_s, na.rm = TRUE), length.out = 20)
LDMC_G_dat <- ggpredict(polySurv_LDMC, terms = c("LDMC_s[LDMC_vals]", "SPEI_waterYr_s[spei_vals]", "Site[site_vals]"), type = "fixed", back.transform = TRUE)

#make a data.frame to contain all of the values for each trait
GramDat <- data.frame(trait = c("scaled(LDMC)"),
                      Site = LDMC_G_dat$facet, # site identity
                      x = LDMC_G_dat$x,  # LDMC values
                      GramSurv = LDMC_G_dat$predicted, # predicted survival values
                      CI_low = LDMC_G_dat$conf.low, # lower boundary of confidence interval
                      CI_high = LDMC_G_dat$conf.high, # upper boundary of confidence interval
                      SPEI = LDMC_G_dat$group)
#make data for rug plot
RugDat_G <-  data.frame(rug = polyDat$LDMC_s, trait = "scaled(LDMC)")

#make a multipanel figure that shows only the graminoid survival probs for 3 traits
ggplot(data = GramDat) +
  geom_ribbon(aes(x = x, ymin = CI_low, ymax = CI_high, fill = SPEI), alpha = 0.3) +
  geom_line(aes(x=x, GramSurv, col = SPEI))  +
  geom_rug(aes(x = rug), data = RugDat_G) +
  labs(title = "Graminoid Survival") +
  xlab(NULL) +
  ylab("Probability of Survival") +
  scale_y_continuous(limits = c(0,1)) +
  scale_color_manual(labels = c("dry year", "wet year"), values = c("#fc8d62","#66c2a5"), guide = FALSE) +
  scale_fill_manual(values = c("#fc8d62","#66c2a5"), guide = FALSE) +
  facet_wrap(~Site, scales = "free_x", strip.position =  "bottom", ncol =1) +
  theme_classic()+
  theme(legend.position = "bottom", legend.title = element_blank(), legend.background = element_rect(fill="grey95",size=0.5, linetype="solid"), strip.background = element_rect(colour=NA, fill=NA), strip.placement = "outside", strip.text.x = element_text(margin = margin(0, 0, 1.5, 0)), plot.title = element_text(hjust = 0.5, size = 13, face = "bold")) #+
#geom_text(data= dat_text, mapping = aes(x = x, y = y, label = label), size = 3, fontface = "bold")
