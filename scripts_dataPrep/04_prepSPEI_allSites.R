#/////////////////////////
# Prepare SPEI Data for Analysis
# Alice Stears
# 26 March 2022
#/////////////////////////

# load packages
library(tidyverse)

#### load SPEI data ####
## for 12month interval for water year
AZn_SPEI_12 <- read.delim("../../Data/Climate Data/AZn_Climate/AZn_12Month_SPEI_-111.250000_  35.750000.csv", sep = ";") %>% rename(Date = days.since.1900.1.1, SPEI_waterYr = spei) %>%
  mutate(Date = as.Date(Date, "%Y-%m-%d"), Year = lubridate::year(Date), Site = "AZn") %>%
  filter(lubridate::month(Date) == 10)

AZs_SPEI_12 <- read.delim("../../Data/Climate Data/AZs Climate/AZs_12Month_SPEI-110.750000_  31.750000.csv", sep = ";") %>% rename(Date = days.since.1900.1.1, SPEI_waterYr = spei) %>%
  mutate(Date = as.Date(Date, "%Y-%m-%d"), Year = lubridate::year(Date), Site = "AZs") %>%
  filter(lubridate::month(Date) == 10)

CO_SPEI_12 <- read.delim("../../Data/Climate Data/CO Climate/CO_12Month_SPEI-104.750000_  40.750000.csv", sep = ";") %>% rename(Date = days.since.1900.1.1, SPEI_waterYr = spei) %>%
  mutate(Date = as.Date(Date, "%Y-%m-%d"), Year = lubridate::year(Date), Site = "CO") %>%
  filter(lubridate::month(Date) == 10)

ID_SPEI_12 <- read.csv("../../Data/Climate Data/ID Climate/ID_12Month_SPEI-112.250000_  44.250000.csv", sep = ";") %>% rename(Date = days.since.1900.1.1, SPEI_waterYr = spei) %>%
  mutate(Date = as.Date(Date, "%Y-%m-%d"), Year = lubridate::year(Date), Site = "ID") %>%
  filter(lubridate::month(Date) == 10)

NM_SPEI_12 <- read.csv("../../Data/Climate Data/NM Climate/NM_12Month_SPEI-106.750000_  32.750000.csv", sep = ";") %>% rename(Date = days.since.1900.1.1, SPEI_waterYr = spei) %>%
  mutate(Date = as.Date(Date, "%Y-%m-%d"), Year = lubridate::year(Date), Site = "NM") %>%
  filter(lubridate::month(Date) == 10)

MT_SPEI_12 <- read.csv("../../Data/Climate Data/MT Climate/MT_12Month_SPEI-105.750000_  46.250000.csv", sep = ";") %>% rename(Date = days.since.1900.1.1, SPEI_waterYr = spei) %>%
  mutate(Date = as.Date(Date, "%Y-%m-%d"), Year = lubridate::year(Date), Site = "MT") %>%
  filter(lubridate::month(Date) == 10)

KS_SPEI_12 <- read.csv("../../Data/Climate Data/KS Climate/KS_12Month_SPEI-99.250000_  38.750000.csv", sep = ";") %>% rename(Date = days.since.1900.1.1, SPEI_waterYr = spei) %>%
  mutate(Date = as.Date(Date, "%Y-%m-%d"), Year = lubridate::year(Date), Site = "KS") %>%
  filter(lubridate::month(Date) == 10)

# unique intervals for each site
AZn_SPEI_unique <- read.delim("../../Data/Climate Data/AZn_Climate/AZn_5Month_SPEI-111.250000_  35.750000.csv", sep = ";") %>% rename(Date = days.since.1900.1.1, SPEI_unique = spei) %>%
  mutate(Date = as.Date(Date, "%Y-%m-%d"), Year = lubridate::year(Date), Site = "AZn") %>%
  mutate(SPEI_Interval = "5_July_Nov") %>%
  filter(lubridate::month(Date) == 11)

AZs_SPEI_unique <- read.delim("../../Data/Climate Data/AZs Climate/AZs_5Month_SPEI-110.750000_  31.750000.csv", sep = ";") %>% rename(Date = days.since.1900.1.1, SPEI_unique = spei) %>%
  mutate(Date = as.Date(Date, "%Y-%m-%d"), Year = lubridate::year(Date), Site = "AZs") %>%
  mutate(SPEI_Interval = "5_July_Nov") %>%
  filter(lubridate::month(Date) == 11)

CO_SPEI_unique <- read.delim("../../Data/Climate Data/CO Climate/CO_6Month_SPEI-104.750000_  40.750000.csv", sep = ";") %>% rename(Date = days.since.1900.1.1, SPEI_unique = spei) %>%
  mutate(Date = as.Date(Date, "%Y-%m-%d"), Year = lubridate::year(Date), Site = "CO") %>%
  mutate(SPEI_Interval = "6_April_Sept") %>%
  filter(lubridate::month(Date) == 9)

ID_SPEI_unique <- read.csv("../../Data/Climate Data/ID Climate/ID_6Month_SPEI-112.250000_  44.250000.csv", sep = ";") %>% rename(Date = days.since.1900.1.1, SPEI_unique = spei) %>%
  mutate(Date = as.Date(Date, "%Y-%m-%d"), Year = lubridate::year(Date), Site = "ID") %>%
  mutate(SPEI_Interval = "6_April_Sept") %>%
  filter(lubridate::month(Date) == 9)

NM_SPEI_unique <- read.csv("../../Data/Climate Data/NM Climate/NM_5Month_SPEI-106.750000_  32.750000.csv", sep = ";") %>% rename(Date = days.since.1900.1.1, SPEI_unique = spei) %>%
  mutate(Date = as.Date(Date, "%Y-%m-%d"), Year = lubridate::year(Date), Site = "NM") %>%
  mutate(SPEI_Interval = "5_July_Nov") %>%
  filter(lubridate::month(Date) == 11)

MT_SPEI_unique <- read.csv("../../Data/Climate Data/MT Climate/MT_5Month_SPEI-105.750000_  46.250000.csv", sep = ";") %>% rename(Date = days.since.1900.1.1, SPEI_unique = spei) %>%
  mutate(Date = as.Date(Date, "%Y-%m-%d"), Year = lubridate::year(Date), Site = "MT") %>%
  mutate(SPEI_Interval = "5_April_Aug") %>%
  filter(lubridate::month(Date) == 8)

KS_SPEI_unique <- read.csv("../../Data/Climate Data/KS Climate/KS_8Month_SPEI-99.250000_  38.750000.csv", sep = ";") %>% rename(Date = days.since.1900.1.1, SPEI_unique = spei) %>%
  mutate(Date = as.Date(Date, "%Y-%m-%d"), Year = lubridate::year(Date), Site = "KS") %>%
  mutate(SPEI_Interval = "8_March_Oct") %>%
  filter(lubridate::month(Date) == 10)

#### put all data together into one d.f ####
SPEI_all <- rbind(AZn_SPEI_12, AZs_SPEI_12, CO_SPEI_12, ID_SPEI_12, MT_SPEI_12, KS_SPEI_12, NM_SPEI_12) %>%
  select(-Date) %>%
  left_join(rbind(AZn_SPEI_unique, AZs_SPEI_unique, CO_SPEI_unique, ID_SPEI_unique, MT_SPEI_unique, KS_SPEI_unique, NM_SPEI_unique)) %>%
  select(Site, Year, SPEI_waterYr, SPEI_unique, SPEI_Interval)

#### get precip data ####
AZs_precip <- read.csv("C:/Users/astears/Dropbox/Grad School/Research/Trait Project/Data/ChartQuadDatasets/AZs_downloaded data/PRISM_ppt_tmean_stable_4km_1930_2020_31.7798_-110.8429.csv", skip = 10)
AZs_precip <- AZs_precip %>%
  mutate(AnnPrecip_mm = ppt..inches. * 25.4,
         Site = "AZs") %>%
  rename(Year = Date)

CO_precip <- read.csv("/Users/astears/Dropbox/Grad School/Research/Trait Project/Data/ChartQuadDatasets/CO_Downloaded Data/PRISM_ppt_tmean_stable_4km_1994_2020_40.8824_-104.7443.csv", skip = 10)
CO_precip <- CO_precip %>%
  mutate(AnnPrecip_mm = ppt..inches. * 25.4,
         Site = "CO") %>%
  rename(Year = Date)

ID_precip <- read.csv("/Users/astears/Dropbox/Grad School/Research/Trait Project/Data/ChartQuadDatasets/ID_Downloaded Data/ID_total_monthly_ppt.csv")
ID_precip$AnnPrecip_mm = apply(ID_precip, MARGIN = 1, FUN = function(x) sum(as.numeric(x[2:13]))) * 25.4
ID_precip$Site <- "ID"
ID_precip <- ID_precip %>%
  rename(Year = YEAR)

KS_precip <- read.csv("/Users/astears/Dropbox/Grad School/Research/Trait Project/Data/ChartQuadDatasets/KS_Downloaded Data/KS_monthly_ppt.csv")
KS_precip$AnnPrecip_mm = apply(KS_precip, MARGIN = 1, FUN = function(x) sum(as.numeric(x[2:13])))
KS_precip$Site <- "KS"
KS_precip <- KS_precip %>%
  rename(Year = YEAR)

MT_precip <- read.csv("/Users/astears/Dropbox/Grad School/Research/Trait Project/Data/ChartQuadDatasets/MT_Downloaded Data/MT_daily_climate_data.csv")
MT_precip <- MT_precip %>%
  group_by(year) %>%
  summarize(AnnPrecip_mm = sum(as.numeric(precip), na.rm = TRUE),
            Site = "MT") %>%
  rename(Year = year)

NM_precip <- read.csv("/Users/astears/Dropbox/Grad School/Research/Trait Project/Data/ChartQuadDatasets/NM_Downloaded Data/total_monthly_ppt.csv")
NM_precip$AnnPrecip_mm = apply(NM_precip, MARGIN = 1, FUN = function(x) sum(as.numeric(x[2:13]))) * 10
NM_precip$Site <- "NM"
NM_precip <- NM_precip %>%
  rename(Year= year)

AZn_precip <- read.csv("/Users/astears/Dropbox/Grad School/Research/Trait Project/Data/ChartQuadDatasets/AZn_downloaded data/PRISM_ppt_tmean_stable_4km_2002_2020_35.2052_-111.7201.csv", skip = 10)
AZn_precip <- AZn_precip %>%
  mutate(AnnPrecip_mm = ppt..inches. * 25.4,
         Site = "AZn") %>%
  rename(Year = Date)

precip_all <- rbind(AZn_precip[,c("Year", "AnnPrecip_mm", "Site")],
      AZs_precip[,c("Year", "AnnPrecip_mm", "Site")],
      CO_precip[,c("Year", "AnnPrecip_mm", "Site")],
      ID_precip[,c("Year", "AnnPrecip_mm", "Site")],
      MT_precip[,c("Year", "AnnPrecip_mm", "Site")],
      KS_precip[,c("Year", "AnnPrecip_mm", "Site")],
      NM_precip[,c("Year", "AnnPrecip_mm", "Site")])

# write to file
SPEI_all <- SPEI_all %>%
  left_join(precip_all, by = c("Site", "Year"))
ggplot(data = SPEI_all) +
  geom_line(aes(x = Year, y = AnnPrecip_mm, col = Site))

write.csv(SPEI_all, file = "../analysis_data/SPEI_allSites.csv", row.names = FALSE)
