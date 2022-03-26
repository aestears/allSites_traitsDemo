#/////////////////////////
# Prepare SPEI Data for Analysis
# Alice Stears
# 26 March 2022
#/////////////////////////

# load packages
library(tidyverse)

#### load SPEI data ####
AZn_SPEI <- read.csv("../../Data/Climate Data/AZn_Climate/AZn_SPEI_35.25_-111.75.csv")

AZs_SPEI <- read.csv("../../Data/Climate Data/AZs Climate/AZs_SPEI_31.75_-110.75.csv")

CO_SPEI <- read.csv("../../Data/Climate Data/CO Climate/CO_SPEI.csv")

ID_SPEI <- read.csv("../../Data/Climate Data/ID Climate/ID_SPEI_44.25_-112.25.csv")

NM_SPEI <- read.csv("../../Data/Climate Data/NM Climate/NM_SPEI_32.75_-106.75.csv")

MT_SPEI <- read.csv("../../Data/Climate Data/MT Climate/MT_SPEI_46.25_-105.75.csv")

KS_SPEI <- read.csv("../../Data/Climate Data/KS Climate/KS_SPEI_38.75_-99.25.csv")


#### subset data for appropriate interval for each site ####
# do a 12-month water year SPEI interval for each site (12-month SPEI interval ending in the Oct. of the current year)
AZn_watYr <- AZn_SPEI %>%
  filter(str_detect(DATA, pattern = "Oct")) %>%
  select(DATA, SPEI_12) %>%
  mutate(Year = as.numeric(str_extract(DATA, pattern = "[0-9]{4}$")),
         Month = str_extract(DATA, pattern = "[[a-z][A-Z]]*"),
         Location = "AZn",
         SPEI_type = "WaterYr",
         SPEI_interval = "Oct_tmin1_Oct_t") %>%
  rename(SPEI_value = SPEI_12)

AZs_watYr <- AZs_SPEI %>%
  filter(str_detect(DATA, pattern = "Oct")) %>%
  select(DATA, SPEI_12) %>%
  mutate(Year = as.numeric(str_extract(DATA, pattern = "[0-9]{4}$")),
         Month = str_extract(DATA, pattern = "[[a-z][A-Z]]*"),
         Location = "AZs",
         SPEI_type = "WaterYr",
         SPEI_interval = "Oct_tmin1_Oct_t") %>%
  rename(SPEI_value = SPEI_12)


CO_watYr <- CO_SPEI %>%
  filter(str_detect(DATA, pattern = "Oct")) %>%
  select(DATA, SPEI_12) %>%
  mutate(Year = as.numeric(str_extract(DATA, pattern = "[0-9]{1,4}$")) + 1900,
         Month = str_extract(DATA, pattern = "[[a-z][A-Z]]*"),
         Location = "CO",
         SPEI_type = "WaterYr",
         SPEI_interval = "Oct_tmin1_Oct_t") %>%
  rename(SPEI_value = SPEI_12)
CO_watYr[51:68, "Year"] <- (CO_watYr[51:68, "Year"] + 100)

ID_watYr <- ID_SPEI %>%
  filter(str_detect(DATA, pattern = "Oct")) %>%
  select(DATA, SPEI_12) %>%
  mutate(Year = as.numeric(str_extract(DATA, pattern = "[0-9]{4}$")),
         Month = str_extract(DATA, pattern = "[[a-z][A-Z]]*"),
         Location = "ID",
         SPEI_type = "WaterYr",
         SPEI_interval = "Oct_tmin1_Oct_t") %>%
  rename(SPEI_value = SPEI_12)

NM_watYr <- NM_SPEI %>%
  filter(str_detect(DATA, pattern = "Oct")) %>%
  select(DATA, SPEI_12) %>%
  mutate(Year = as.numeric(str_extract(DATA, pattern = "[0-9]{4}$")),
         Month = str_extract(DATA, pattern = "[[a-z][A-Z]]*"),
         Location = "NM",
         SPEI_type = "WaterYr",
         SPEI_interval = "Oct_tmin1_Oct_t") %>%
  rename(SPEI_value = SPEI_12)

MT_watYr <- MT_SPEI %>%
  filter(str_detect(DATA, pattern = "Oct")) %>%
  select(DATA, SPEI_12) %>%
  mutate(Year = as.numeric(str_extract(DATA, pattern = "[0-9]{4}$")),
         Month = str_extract(DATA, pattern = "[[a-z][A-Z]]*"),
         Location = "MT",
         SPEI_type = "WaterYr",
         SPEI_interval = "Oct_tmin1_Oct_t") %>%
  rename(SPEI_value = SPEI_12)

KS_watYr <- KS_SPEI %>%
  filter(str_detect(DATA, pattern = "Oct")) %>%
  select(DATA, SPEI_12) %>%
  mutate(Year = as.numeric(str_extract(DATA, pattern = "[0-9]{4}$")),
         Month = str_extract(DATA, pattern = "[[a-z][A-Z]]*"),
         Location = "KS",
         SPEI_type = "WaterYr",
         SPEI_interval = "Oct_tmin1_Oct_t") %>%
  rename(SPEI_value = SPEI_12)


# do a unique interval for each site based on the growing season
# July - Nov
AZn_unique<- AZn_SPEI %>%
  filter(str_detect(DATA, pattern = "Nov")) %>%
  select(DATA, SPEI_5) %>%
  mutate(Year = as.numeric(str_extract(DATA, pattern = "[0-9]{4}$")),
         Month = str_extract(DATA, pattern = "[[a-z][A-Z]]*"),
         Location = "AZn",
         SPEI_type = "unique",
         SPEI_interval = "July_Nov") %>%
  rename(SPEI_value = SPEI_5)

# July - Nov
AZs_unique <- AZs_SPEI %>%
  filter(str_detect(DATA, pattern = "Nov")) %>%
  select(DATA, SPEI_5) %>%
  mutate(Year = as.numeric(str_extract(DATA, pattern = "[0-9]{4}$")),
         Month = str_extract(DATA, pattern = "[[a-z][A-Z]]*"),
         Location = "AZs",
         SPEI_type = "unique",
         SPEI_interval = "July_Nov") %>%
  rename(SPEI_value = SPEI_5)

# April - September
CO_unique <- CO_SPEI %>%
  filter(str_detect(DATA, pattern = "Sep")) %>%
  select(DATA, SPEI_6) %>%
  mutate(Year = as.numeric(str_extract(DATA, pattern = "[0-9]{1,4}$")) + 1900,
         Month = str_extract(DATA, pattern = "[[a-z][A-Z]]*"),
         Location = "CO",
         SPEI_type = "unique",
         SPEI_interval = "April_Sept") %>%
  rename(SPEI_value = SPEI_6)
CO_unique[51:68, "Year"] <- (CO_unique[51:68, "Year"] + 100)

# April - September
ID_unique <- ID_SPEI %>%
  filter(str_detect(DATA, pattern = "Sep")) %>%
  select(DATA, SPEI_6) %>%
  mutate(Year = as.numeric(str_extract(DATA, pattern = "[0-9]{4}$")),
         Month = str_extract(DATA, pattern = "[[a-z][A-Z]]*"),
         Location = "ID",
         SPEI_type = "unique",
         SPEI_interval = "April_Sept") %>%
  rename(SPEI_value = SPEI_6)

# July - November
NM_unique <- NM_SPEI %>%
  filter(str_detect(DATA, pattern = "Nov")) %>%
  select(DATA, SPEI_5) %>%
  mutate(Year = as.numeric(str_extract(DATA, pattern = "[0-9]{4}$")),
         Month = str_extract(DATA, pattern = "[[a-z][A-Z]]*"),
         Location = "NM",
         SPEI_type = "unique",
         SPEI_interval = "July_Nov") %>%
  rename(SPEI_value = SPEI_5)

# April - August
MT_unique <- MT_SPEI %>%
  filter(str_detect(DATA, pattern = "Aug")) %>%
  select(DATA, SPEI_5) %>%
  mutate(Year = as.numeric(str_extract(DATA, pattern = "[0-9]{4}$")),
         Month = str_extract(DATA, pattern = "[[a-z][A-Z]]*"),
         Location = "MT",
         SPEI_type = "unique",
         SPEI_interval = "April_Aug") %>%
  rename(SPEI_value = SPEI_5)

# March - October
KS_unique <- KS_SPEI %>%
  filter(str_detect(DATA, pattern = "Oct")) %>%
  select(DATA, SPEI_8) %>%
  mutate(Year = as.numeric(str_extract(DATA, pattern = "[0-9]{4}$")),
         Month = str_extract(DATA, pattern = "[[a-z][A-Z]]*"),
         Location = "KS",
         SPEI_type = "unique",
         SPEI_interval = "March_Oct") %>%
  rename(SPEI_value = SPEI_8)

#### put all data together into one d.f ####
SPEI_all <- rbind(AZn_watYr, AZn_unique, AZs_watYr, AZs_unique, CO_watYr, CO_unique, KS_watYr, KS_unique, ID_watYr, ID_unique, MT_watYr, MT_unique, NM_watYr, NM_unique)
