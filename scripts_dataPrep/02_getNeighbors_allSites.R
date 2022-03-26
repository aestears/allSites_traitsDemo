#/////////////////////////
# Using getNeighbors() to get competition data for all chart-quadrat sites
# Alice Stears
# 21 March 2022
#/////////////////////////

# Load Packages -----------------------------------------------------------
# library(tidyverse)
library(sf)
library(devtools)
devtools::install_github("aestears/plantTracker", force = TRUE)
library(plantTracker)

# Load trackSpp Data for each site ----------------------------------------
AZs_dat <- readRDS("../trackSpp_outputData/AZs_buff5_dorm1_demoDat.RDS")
CO_dat <- readRDS("../trackSpp_outputData/CO_buff5_dorm1_demoDat.RDS")
ID_dat <- readRDS("../trackSpp_outputData/ID_buff5_dorm1_demoDat.RDS")
KS_dat <- readRDS("../trackSpp_outputData/KS_buff5_dorm1_demoDat.RDS")
MT_dat <- readRDS("../trackSpp_outputData/MT_buff5_dorm1_demoDat.RDS")
NM_dat <- readRDS("../trackSpp_outputData/NM_buff5_dorm1_demoDat.RDS")

# getNeighbors ----------------------------------------
# fix issue w/ some individuals having an 'na' in the survives column?? checked, and they should be '0's

AZs_dat[AZs_dat$survives_tplus1==.5 & is.na(AZs_dat$survives_tplus1) == FALSE,]$survives_tplus1 <- 0

# for some reason there are duplicates still?  find and remove those

bad <- AZs_dat[duplicated(st_drop_geometry(AZs_dat[, c("trackID", "Year","Quad")])),]

badQuadSpp <- unique(st_drop_geometry(bad[,c("Quad", "Species")]))
badQuadSpp$QuadSpecies <- paste0(badQuadSpp$Quad, "_", badQuadSpp$Species)
# remove the unaggregated 'badQuadYears' from AZs_dat
AZs_temp <- AZs_dat
AZs_temp$QuadSpecies <- paste0(AZs_dat$Quad, "_", AZs_dat$Species)
AZs_good <- AZs_temp[!(AZs_temp$QuadSpecies %in% badQuadSpp$QuadSpecies),]
AZs_bad <- AZs_temp[(AZs_temp$QuadSpecies %in% badQuadSpp$QuadSpecies),]

# maybe the AZs data isn't aggregated?
test01 <- aggregate(AZs_bad[1:10000,"basalArea_genet"], by = list(
  "Site" = AZs_bad[1:10000,]$Site,
  "Quad" = AZs_bad[1:10000,]$Quad,
  "Species" = AZs_bad[1:10000,]$Species,
  "trackID" = AZs_bad[1:10000,]$trackID,
  "Year" = AZs_bad[1:10000,]$Year),
  FUN = sum,
  do_union = TRUE
  )
test03 <- aggregate(AZs_bad[20001:30000,"basalArea_genet"], by = list(
  "Site" = AZs_bad[20001:30000,]$Site,
  "Quad" = AZs_bad[20001:30000,]$Quad,
  "Species" = AZs_bad[20001:30000,]$Species,
  "trackID" = AZs_bad[20001:30000,]$trackID,
  "Year" = AZs_bad[20001:30000,]$Year),
  FUN = sum,
  do_union = TRUE
)
test04 <- aggregate(AZs_bad[30001:40000,"basalArea_genet"], by = list(
  "Site" = AZs_bad[30001:40000,]$Site,
  "Quad" = AZs_bad[30001:40000,]$Quad,
  "Species" = AZs_bad[30001:40000,]$Species,
  "trackID" = AZs_bad[30001:40000,]$trackID,
  "Year" = AZs_bad[30001:40000,]$Year),
  FUN = sum,
  do_union = TRUE
)
test05 <- aggregate(AZs_bad[40001:49187,"basalArea_genet"], by = list(
  "Site" = AZs_bad[40001:49187,]$Site,
  "Quad" = AZs_bad[40001:49187,]$Quad,
  "Species" = AZs_bad[40001:49187,]$Species,
  "trackID" = AZs_bad[40001:49187,]$trackID,
  "Year" = AZs_bad[40001:49187,]$Year),
  FUN = sum,
  do_union = TRUE
)

test <- rbind(test01, test02, test03, test04, test05)

names(test)[6] <- "basalArea_genet"

test2 <- aggregate(st_drop_geometry(AZs_bad[,c("size_tplus1")]), by = list(
  "Site" = AZs_bad$Site,
  "Quad" = AZs_bad$Quad,
  "Species" = AZs_bad$Species,
  "trackID" = AZs_bad$trackID,
  "Year" = AZs_bad$Year),
  FUN = sum,
  do_union = FALSE
)
test3 <- aggregate(st_drop_geometry(AZs_bad[,c("recruit", "survives_tplus1", "age", "nearEdge")]), by = list(
  "Site" = AZs_bad$Site,
  "Quad" = AZs_bad$Quad,
  "Species" = AZs_bad$Species,
  "trackID" = AZs_bad$trackID,
  "Year" = AZs_bad$Year),
  FUN = mean,
  do_union = FALSE
)
badAgg <- left_join(test, test2)
badAgg <- left_join(badAgg, test3)
badAgg[badAgg$survives_tplus1 > 1 & is.na(badAgg$survives) == FALSE,]

AZs_good <- AZs_good %>%
  select(-QuadSpecies)

## add re-aggregated results back w/ AZs_dat
badAgg <- badAgg[,names(AZs_good)]
AZs_datNew <- rbind(AZs_good, badAgg)
AZs_neighbors_buff10_allSpp_count <- getNeighbors(dat = AZs_datNew, buff = .10, method = "count",
                                                  compType = "allSpp", output = "summed")


#### getNeighbors() ####
function (dat, buff, method, compType = "allSpp", output = "summed",
          trackID = "trackID", species = "Species", quad = "Quad",
          year = "Year", site = "Site", geometry = "geometry", ...)
{
  checkedDat <- checkDat(dat = dat, species = species, site = site,
                         quad = quad, year = year, geometry = geometry, reformatDat = TRUE)
  dat <- checkedDat$dat
  if (sum(names(dat) %in% paste0(trackID, "_USER")) > 0) {
    trackIDuserName <- paste0(trackID, "_USER")
    trackIDdefaultName <- "trackID"
    names(dat)[names(dat) == trackIDuserName] <- trackIDdefaultName
    if (sum(is.na(dat$trackID)) != 0) {
      stop("The column in 'dat' that contains trackID information cannot have\n      any NA values")
    }
  } else {
    stop("The 'dat' argument must have a column that contains a unique\n         identifier for each genetic individual (i.e. a 'trackID'). This column\n         must have the same name that you specified in the 'trackID' argument in\n         this function call.  ")
  }
  if (missing(buff)) {
    stop("The 'buff' argument must have a value.")
  } else {
    if (is.numeric(buff) & length(buff) == 1) {
      if (buff < 0 | buff > max(st_bbox(dat)[c("xmax",
                                               "ymax")]) | length(buff) != 1) {
        stop("If 'buff' is not specified for every species, it must be a single\n        numeric value that is greater than or equal to 0")
      }
      buff <- data.frame(Species = unique(dat$Species),
                         buff = buff)
    } else if (is.data.frame(buff)) {
      if (sum(!names(buff) %in% c("Species", "buff")) ==
          0) {
        if (sum(!unique(dat$Species) %in% buff$Species) >
            0 | sum(is.na(dat$buff)) > 0 | !is.numeric(buff$buff) |
            sum(buff$buff < 0) > 0 | round(buff$buff) !=
            buff$buff) {
          stop("If the 'buff' argument is specified by species, it must be a\n          data.frame that includes a 'Species' column with a row for every\n          species in 'dat', and a 'buff' column that contains positive, numeric\n          values for each species with no NAs.")
        }
      } else {
        stop("If the 'buff' argument is specifed by species, the column names\n        must be 'Species' and 'buff'")
      }
    } else {
      stop("The 'buff' argument must be either a single numeric value that is\n      greater than or equal to 0, OR a data.frame that has a 'Species' column\n      with values for each species in 'dat', and a 'buff' column with numeric\n      values for each species.")
    }
  }
  if (missing(method)) {
    stop("The 'method' argument must have a value. It must be a character vector\nwith either the value 'area' (meaning you want to calculate the total basal area\nwithin the specified buffer of the focal individual that is occupied by other\nindividuals) or 'count' (meaning you want to know how many genets are within the\nspecified buffer of the focal individual")
  } else if (is.character(method) & length(method) == 1) {
    method <- tolower(method)
    if (method != "area" & method != "count") {
      stop("The 'method' argument must have a value of 'area' or 'count'.")
    }
  } else {
    stop("'method' must be a character vector of length one.")
  }
  if (is.character(compType) & length(compType) == 1) {
    if (compType != "allSpp" & compType != "oneSpp") {
      stop("The 'compType' argument must have a value of 'allSpp' or 'oneSpp'.")
    }
  } else {
    stop("'compType' must be a character vector of length one.")
  }
  if (is.character(output) & length(output) == 1) {
    if (output != "summed" & output != "bySpecies") {
      stop("The 'output' argument must have a value of 'summed' or 'bySpecies'.")
    }
  } else {
    stop("'output' must be a character vector of length one.")
  }
  if (nrow(unique(dat[, c("Year", "trackID")])) != nrow(dat)) {
    stop("In order to be used in this function, the 'dat' argument must have\n    only one row for each unique individual (genet) in each year. You can use\n    the 'aggregateByGenet()' function to get a dataset with one row per genet\nper year.")
  }
  dat$index <- 1:nrow(dat)
  datStore <- sf::st_drop_geometry(dat[, !names(dat) %in% c("Species",
                                                            "Site", "Quad", "Year", "trackID", "geometry")])
  dat <- dat[, names(dat) %in% c("Species", "Site", "Quad",
                                 "Year", "trackID", "geometry", "index")]
  dat <- merge(dat, buff, by = "Species")
  datBuffTemp <- sf::st_buffer(x = dat, dist = dat$buff)
  tempBuffGeometry <- list()
  for (i in 1:nrow(dat)) {
    tempBuffGeometry[i] <- suppressWarnings(sf::st_difference(x = datBuffTemp[i,
    ], y = dat[i, ])$geometry)
  }
  datBuff <- sf::st_set_geometry(x = datBuffTemp, value = sf::st_as_sfc(tempBuffGeometry))
  datBuff <- sf::st_set_geometry(x = datBuff, value = sf::st_intersection(sf::st_as_sfc(sf::st_bbox(dat)),
                                                                          datBuff))
  if (method == "count") {
    dat$neighbors_count <- NA
    for (i in unique(dat$Site)) {
      for (j in unique(dat[dat$Site == i, "Quad"]$Quad)) {
        for (k in unique(dat[dat$Site == i & dat$Quad ==
                             j, "Year"]$Year)) {
          if (compType == "oneSpp") {
            for (l in unique(dat[dat$Site == i & dat$Quad ==
                                 j & dat$Year == k, "Species"]$Species)) {
              datOneSpp <- dat[dat$Site == i & dat$Quad ==
                                 j & dat$Year == k & dat$Species == l,
              ]
              datOneBuff <- datBuff[datBuff$Site == i &
                                      datBuff$Quad == j & datBuff$Year == k &
                                      datBuff$Species == l, ]
              overlapM <- sf::st_intersects(datOneBuff,
                                            datOneSpp, sparse = FALSE)
              diag(overlapM) <- FALSE
              overlapList <- apply(overlapM, MARGIN = 1,
                                   FUN = function(x) c(which(x == TRUE)))
              if (length(overlapList) > 0) {
                datOneSpp$neighbors <- unlist(lapply(overlapList,
                                                     length))
              } else {
                datOneSpp$neighbors <- 0
              }
              dat[match(datOneSpp$index, dat$index),
              ]$neighbors_count <- datOneSpp$neighbors
            }
          }  else if (compType == "allSpp") {
            datSpp <- dat[dat$Site == i & dat$Quad ==
                            j & dat$Year == k, ]
            datSppBuff <- datBuff[datBuff$Site == i &
                                    datBuff$Quad == j & datBuff$Year == k,
            ]
            overlapM <- sf::st_intersects(datSppBuff,
                                          datSpp, sparse = FALSE)
            diag(overlapM) <- FALSE
            if (output == "summed") {
              overlapList <- apply(X = overlapM, MARGIN = 1,
                                   FUN = function(x) c(which(x == TRUE)))
              if (length(overlapList) > 0) {
                datSpp$neighbors <- unlist(lapply(overlapList,
                                                  length))
              } else {
                datSpp$neighbors <- 0
              }
              dat[match(datSpp$index, dat$index), ]$neighbors_count <- datSpp$neighbors
            }  else if (output == "bySpecies") {
              colnames(overlapM) <- datSpp$Species
              overlapSpp <- matrix(NA, nrow = nrow(overlapM),
                                   ncol = length(unique(colnames(overlapM))))
              colnames(overlapSpp) <- unique(colnames(overlapM))
              for (m in unique(colnames(overlapM))) {
                overlapSpp[, colnames(overlapSpp) ==
                             m] <- rowSums(overlapM[, colnames(overlapM) ==
                                                      m])
              }
              overlapSpp_List <- apply(overlapSpp, MARGIN = 1,
                                       FUN = function(x) as.list(x))
              if (length(overlapSpp_List) > 0) {
                datSpp$neighbors <- overlapSpp_List
              } else {
                datSpp$neighbors <- 0
              }
              dat[match(datSpp$index, dat$index), ]$neighbors_count <- datSpp$neighbors
            }
          }
        }
      }
    }
  } else if (method == "area") {
    dat$neighbors_area <- NA
    dat$nBuff_area <- NA
    tempAreas <- suppressWarnings(sf::st_intersection(x = datBuff,
                                                      y = dat))
    if (compType == "oneSpp") {
      tempAreas2 <- tempAreas[tempAreas$Site == tempAreas$Site.1 &
                                tempAreas$Quad == tempAreas$Quad.1 & tempAreas$Year ==
                                tempAreas$Year.1 & tempAreas$Species == tempAreas$Species.1,
      ]
      temp3 <- aggregate(tempAreas2$geometry, by = list(Site = tempAreas2$Site,
                                                        Quad = tempAreas2$Quad, Species = tempAreas2$Species,
                                                        trackID = tempAreas2$trackID, Year = tempAreas2$Year,
                                                        index = tempAreas2$index), FUN = function(x) sum(sf::st_area(x)))
      datBuff <- datBuff[order(datBuff$index), ]
      temp3 <- temp3[order(temp3$index), ]
      dat[match(temp3$index, dat$index), ]$nBuff_area <- sf::st_area(datBuff)
      dat[match(temp3$index, dat$index), ]$neighbors_area <- temp3$geometry
    } else if (compType == "allSpp") {
      tempAreas2 <- tempAreas[tempAreas$Site == tempAreas$Site.1 &
                                tempAreas$Quad == tempAreas$Quad.1 & tempAreas$Year ==
                                tempAreas$Year.1, ]
      if (output == "summed") {
        temp3 <- aggregate(x = tempAreas2$geometry, by = list(Site = tempAreas2$Site,
                                                              Quad = tempAreas2$Quad, Species = tempAreas2$Species,
                                                              trackID = tempAreas2$trackID, Year = tempAreas2$Year,
                                                              index = tempAreas2$index), FUN = function(x) sum(sf::st_area(x)))
        datBuff <- datBuff[order(datBuff$index), ]
        temp3 <- temp3[order(temp3$index), ]
        dat[match(temp3$index, dat$index), ]$nBuff_area <- sf::st_area(datBuff)
        dat[match(temp3$index, dat$index), ]$neighbors_area <- temp3$geometry
      } else if (output == "bySpecies") {
        temp3_spp <- stats::aggregate(x = tempAreas2$geometry,
                                      by = list(Site = tempAreas2$Site, Quad = tempAreas2$Quad,
                                                Species = tempAreas2$Species, trackID = tempAreas2$trackID,
                                                Year = tempAreas2$Year, index = tempAreas2$index,
                                                Species_neighbor = tempAreas2$Species.1),
                                      FUN = function(x) sum(sf::st_area(x)))
        for (n in unique(temp3_spp$index)) {
          tmp <- temp3_spp[temp3_spp$index == unique(temp3_spp$index)[n],
          ]
          tmpDF <- tmp[1, 1:6]
          tmpL <- list()
          tmpL[[1]] <- as.list(tmp[, 8])
          names(tmpL[[1]]) <- tmp[, 7]
          tmpDF$neighbors_area <- tmpL
          if (n == unique(temp3_spp$index)[1]) {
            bySppDF <- tmpDF
          }  else {
            bySppDF <- rbind(bySppDF, tmpDF)
          }
        }
        datBuff <- datBuff[order(datBuff$index), ]
        bySppDF <- bySppDF[order(bySppDF$index), ]
        dat[match(bySppDF$index, dat$index), ]$nBuff_area <- sf::st_area(datBuff)
        dat[match(bySppDF$index, dat$index), ]$neighbors_area <- bySppDF$neighbors_area
      }
    }
  }
  outputDat <- dat
  outputDat <- outputDat[, names(outputDat) != "buff"]
  userColNames <- c(checkedDat$userColNames[c(1:4)], trackIDuserName,
                    checkedDat$userColNames[c(5)])
  outputDat <- merge(outputDat, datStore, by = "index")
  outputDat <- outputDat[, names(outputDat) != "index"]
  defaultNames <- c("Species", "Site", "Quad", "Year", "trackID",
                    "geometry")
  names(outputDat)[match(defaultNames, names(outputDat))] <- userColNames
  names(outputDat) <- gsub(names(outputDat), pattern = "_USER",
                           replacement = "")
  return(outputDat)
}
