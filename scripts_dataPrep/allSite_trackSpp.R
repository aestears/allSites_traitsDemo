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
load("/Users/Alice/Dropbox/Grad School/Research/Trait Project/Data/QuadratShapefiles/MegaShapeEnvironment.RData")
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

# test w/ subset of data
tempDat <- megaShape[megaShape$Site == "AZs" ,]

AZs_demo_dorm1_buff5_buffG05 <- trackSpp(dat = tempDat, inv = inv, dorm = 1, buff = .05, buffGenet = .005, clonal = clonalDF)


#### trackSpp function ####
checkData <- checkDat(dat = dat, inv = inv, species = species,
                      site = site, quad = quad, year = year, geometry = geometry,
                      reformatDat = TRUE)
dat <- checkData$dat
inv <- checkData$inv
usrNames <- checkData$userColNames
if (missing(dorm)) {
  stop("The 'dorm' argument must have a value.")
} else if (is.numeric(dorm) & length(dorm == 1)) {
  if (dorm < 0 | round(dorm) != dorm | length(dorm) !=
      1) {
    stop("If 'dorm' is not specified for every species, it must be a single\n      numeric value that is a whole number greater than or equal to 0")
  }
  dorm <- data.frame(Species = unique(dat$Species), dorm = dorm)
} else if (is.data.frame(dorm)) {
  if (sum(!names(dorm) %in% c("Species", "dorm")) == 0) {
    if (sum(!unique(dat$Species) %in% dorm$Species) >
        0 | sum(is.na(dat$dorm)) > 0 | !is.numeric(dorm$dorm) |
        sum(dorm$dorm < 0) > 0 | round(dorm$dorm) !=
        dorm$dorm) {
      stop("If the 'dorm' argument is specified by species, it must be a\n        data.frame that includes a 'Species' column with a row for every species\n        in 'dat', and a 'dorm' column that contains positive, whole number\n        values for each species with no NAs.")
    }
  } else {
    stop("If the 'dorm' argument is specifed by species, the column names must\n      be 'Species' and 'dorm'")
  }
} else {
  stop("The 'dorm' argument must be either a single numeric value that is a\n    whole number greater than or equal to 0, OR a data.frame that has a\n    'Species' column with values for each species in 'dat', and a 'dorm' column\n    with numeric, positive whole number values for each species.")
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
if (missing(clonal)) {
  stop("The 'clonal' argument must have a value.")
} else {
  if (is.logical(clonal) & length(clonal == 1)) {
    if (clonal != TRUE & clonal != FALSE | !is.logical(clonal) |
        length(clonal) != 1) {
      stop("If 'clonal' is not specified for every species, it must be a\n        single logical value that is either FALSE or TRUE.")
    }
    clonal <- data.frame(Species = unique(dat$Species),
                         clonal = clonal)
  } else if (is.data.frame(clonal)) {
    if (sum(!names(clonal) %in% c("Species", "clonal")) ==
        0) {
      if (sum(!unique(dat$Species) %in% clonal$Species) >
          0 | length(unique(clonal$Species)) != nrow(clonal) |
          sum(is.na(clonal$clonal)) > 0 | (!is.numeric(clonal$clonal) &
                                           !is.logical(clonal$clonal)) | sum((clonal$clonal !=
                                                                              TRUE & clonal$clonal != FALSE)) > 0) {
        stop("If the 'clonal' argument is specified by species, it must be a\n          data.frame that includes a 'Species' column with a row for every\n          species in 'dat', and a 'clonal' column that contains logical values\n          of either FALSE or TRUE for each species with no NAs. There cannot be\n          multiple rows for the same species.")
      }
    } else {
      stop("If the 'clonal' argument is specifed by species, the column names\n        must be 'Species' and 'clonal'")
    }
  } else {
    stop("The 'clonal' argument must be either a single logical value that is\neither TRUE or FALSE, OR a data.frame that has a 'Species' column with\nvalues for each species in 'dat', and a 'clonal' column that contains logical\nvalues of either FALSE or TRUE for each species with no NAs.")
  }
}
if (sum(clonal$clonal) > 0) {
  if (missing(buffGenet)) {
    stop("The 'buffGenet' argument must have a value.")
  }else {
    if (is.numeric(buffGenet) & length(buffGenet == 1)) {
      if (buffGenet < 0 | buffGenet > max(st_bbox(dat)[c("xmax",
                                                         "ymax")]) | length(buffGenet) != 1) {
        stop("If 'buffGenet' is not specified for every species, it must be\n            a single numeric value that is greater than or equal to 0")
      }
      buffGenet <- data.frame(Species = unique(dat$Species),
                              buffGenet = buffGenet)
    } else if (is.data.frame(buffGenet)) {
      if (sum(!names(buffGenet) %in% c("Species", "buffGenet")) ==
          0) {
        if (sum(!unique(dat$Species) %in% buffGenet$Species) >
            0 | sum(is.na(dat$buffGenet)) > 0 | !is.numeric(buffGenet$buffGenet) |
            sum(buffGenet$buffGenet < 0) > 0) {
          stop("If the 'buffGenet' argument is specified by species, it must\n              be a data.frame that includes a 'Species' column with a row for\n              every species in 'dat', and a 'buffGenet' column that contains\n              positive, numeric values for each species with no NAs.")
        }
      } else {
        stop("If the 'buffGenet' argument is specifed by species, the column\n            names must be 'Species' and 'buffGenet'")
      }
    } else {
      stop("The 'buffGenet' argument must be either a single numeric value\n          that is greater than or equal to 0, OR a data.frame that has a\n          'Species' column with values for each species in 'dat', and a\n          'buffGenet' column with numeric values for each species.")
    }
  }
  if (!is.logical(aggByGenet)) {
    stop("The 'aggByGenet' argument must be a logical value. TRUE\n        means that every row in the output of trackSpp() represents a unique\n        genetic individual (genet) in a given year. FALSE means that every row\n        in the output of trackSpp() represents a unique stem (ramet) in a given\n        year.")
  }
} else {
  buffGenet <- data.frame(Species = unique(dat$Species),
                          buffGenet = NA)
  aggByGenet <- FALSE
}
if (!is.logical(printMessages)) {
  stop("The 'printMessages' argument must be a logical value.")
}
if (!is.logical(flagSuspects) | length(flagSuspects) != 1) {
  stop("'flagSuspects' must be a single logical value that is either\n           FALSE or TRUE.")
}
if (is.numeric(shrink) == FALSE | length(shrink) > 1) {
  stop("'shrink' must be a single numeric value")
}
if (is.numeric(dormSize) == FALSE | length(dormSize) > 1) {
  stop("'dormSize' must be a single numeric value")
}
dat$indexStore <- c(1:nrow(dat))
datStore <- dat[, !names(dat) %in% c("Site", "Quad", "Year",
                                     "Species", "geometry")]
datStore <- sf::st_drop_geometry(datStore)
dat <- dat[, names(dat) %in% c("Site", "Quad", "Year", "Species",
                               "geometry", "indexStore")]
dat$basalArea_ramet <- sf::st_area(dat)
units(dat$basalArea_ramet) <- NULL
for (i in unique(dat$Site)) {
  if (printMessages == TRUE) {
    cat(paste0("Site: ", i, "\n"))
  }
  for (j in unique(dat[dat$Site == i, ]$Quad)) {
    if (printMessages == TRUE) {
      cat(paste0("-- Quadrat: ", j, "\n"))
    }
    if (is.list(inv) == TRUE) {
      invQuad <- inv[[j]]
    }else if (is.vector(inv)) {
      invQuad <- inv
    }
    buffAvg <- mean(buff$buff)
    buffEdgeOutside <- sf::st_as_sfc(sf::st_bbox(dat[dat$Site ==
                                                       i, ]))
    buffEdgeInside <- sf::st_as_sfc(sf::st_bbox(dat[dat$Site ==
                                                      i, ]) + c(buffAvg, buffAvg, -buffAvg, -buffAvg))
    buffEdge <- sf::st_difference(buffEdgeOutside, buffEdgeInside)
    for (k in unique(dat[dat$Site == i & dat$Quad ==
                         j, ]$Species)) {
      datSp <- dat[dat$Site == i & dat$Quad == j &
                     dat$Species == k, ]
      dormK <- dorm[dorm$Species == k, "dorm"]
      clonalK <- clonal[clonal$Species == k, "clonal"]
      buffK <- buff[buff$Species == k, "buff"]
      if (sum(is.na(buffGenet$buffGenet)) == 0) {
        buffGenetK <- buffGenet[buffGenet$Species ==
                                k, "buffGenet"]
      } else {
        buffGenetK <- NA
      }
      datOut <- plantTracker:::assign(dat = datSp,
                                      inv = invQuad, dorm = dormK, buff = buffK,
                                      buffGenet = buffGenetK, clonal = clonalK, flagSuspects = flagSuspects,
                                      shrink = shrink, dormSize = dormSize, inheritsFromTrackSpp = TRUE,
                                      nearEdgeBox = buffEdge)
      if (i == unique(dat$Site)[1] & j == unique(dat[dat$Site ==
                                                     i, ]$Quad)[1] & k == unique(dat[dat$Site ==
                                                                                     i & dat$Quad == j, ]$Species)[1]) {
        trackSppOut <- datOut
      }
      else {
        trackSppOut <- rbind(trackSppOut, datOut)
      }
      if (k == unique(dat[dat$Site == i & dat$Quad ==
                          j, ]$Species)[1]) {
        if (printMessages == TRUE) {
          cat(paste0("---- Species: ", k))
        }
      }
      else if (k == tail(unique(dat[dat$Site == i &
                                    dat$Quad == j, ]$Species), n = 1)) {
        if (printMessages == TRUE) {
          cat(paste0("; ", k, "\n"))
        }
      }
      else {
        if (printMessages == TRUE) {
          cat(paste0("; ", k))
        }
      }
      if (printMessages == TRUE) {
        invComp <- data.frame(inv = c(NA, invQuad),
                              invNext = c(invQuad, NA))
        invComp$diff <- invComp$invNext - invComp$inv
        gapYears <- invComp[invComp$diff > (dormK +
                                              1) & is.na(invComp$diff) == FALSE, "inv"]
        if (length(gapYears) > 0) {
          print(paste0("Also Note: Individuals of the species ",
                       unique(datSp$Species), " in year(s) ",
                       gapYears, " have a value of 'NA' in the 'survives_tplus1' and",
                       " 'size_tplus1' columns because ", gapYears,
                       " is the", " last year of sampling in this quadrat before a gap",
                       " that exceeds the 'dorm' argument for this species."))
        }
      }
    }
    if (printMessages == TRUE) {
      print(paste0("Note: Individuals in year ", max(invQuad),
                   " have a value ", "of 'NA' in the 'survives_tplus1' and 'size_tplus1' columns ",
                   "because ", max(invQuad), " is the last year of sampling in ",
                   "this quadrat."))
    }
  }
}
trackSppOut <- merge(trackSppOut, datStore, by = "indexStore")
trackSppOut <- trackSppOut[, names(trackSppOut) != "indexStore"]
if (aggByGenet == TRUE) {
  trackSppOut <- aggregateByGenet(dat = trackSppOut)
  if (printMessages == TRUE) {
    print(paste0("Note: The output data.frame from this function is shorter",
                 " than your input data.frame because demographic data has",
                 " been aggregated by genet. Because of this, some columns",
                 " that were present in your input data.frame may no longer",
                 " be present. If you don't want the output to be aggregated",
                 " by genet, include the argument 'aggByGenet == FALSE' in",
                 " your call to trackSpp()."))
  }
}
defaultNames <- c("Species", "Site", "Quad", "Year", "geometry")
names(trackSppOut)[match(defaultNames, names(trackSppOut))] <- usrNames
names(tracSppOut) <- gsub(names(trackSppOut), pattern = "_USER",
                           replacement = "")

#### aggregateByGenet ####
if (sum(!sf::st_is(x = st_geometry(dat), type = c("POLYGON",
                                                  "MULTIPOLYGON"))) != 0) {
  stop("The 'dat' argument must be an sf data.frame with only 'POLYGON' or\n'MULTIPOLYGON' geometries.")
} else {
  if (sum(c("basalArea_genet", "age", "recruit", "survives_tplus1",
            "size_tplus1", "nearEdge") %in% names(dat)) != 6) {
    stop("The 'dat' argument must contain columns that are called 'basalArea_genet',\n'age', 'recruit', 'survives_tplus1', 'nearEdge', and 'size_tplus1'.")
  }
}
if (is.numeric(dat$basalArea_genet) == FALSE | sum(!dat$basalArea_genet >
                                                   0, na.rm = TRUE) != 0) {
  stop("The 'basalArea_genet' column in 'dat' must be a numeric vector with\n         only positive values.")
}
if (is.numeric(dat$age) == FALSE | sum(dat$age < 0, na.rm = TRUE) !=
    0) {
  stop("The 'age' column in 'dat' must be a numeric vector with positive values.")
}
if (is.numeric(dat$recruit) == FALSE | sum((dat$recruit ==
                                            0 | dat$recruit == 1 | is.na(dat$recruit))) != nrow(dat)) {
  stop("The 'recruit' column in 'dat' must be a numeric vector with values of only\n1, 0, or NA.")
}
if (is.numeric(dat$survives_tplus1) == FALSE | sum((dat$survives_tplus1 ==
                                                    0 | dat$survives_tplus1 == 1 | is.na(dat$survives_tplus1))) !=
    nrow(dat)) {
  stop("The 'survives_tplus1' column in 'dat' must be a numeric vector with values\nof only 1, 0, or NA.")
}
if (is.numeric(dat$size_tplus1) == FALSE | sum(!dat$size_tplus1 >
                                               0, na.rm = TRUE) != 0) {
  stop("The 'size_tplus1' column in 'dat' must be a numeric vector with only\npositive values (or NA).")
}
if (is.logical(dat$nearEdge) == FALSE) {
  stop("The 'nearEdge' column in 'dat' must be a logical vector.")
}
if ("Suspect" %in% names(dat)) {
  if (is.logical(dat$Suspect) == FALSE) {
    stop("The 'Suspect' column in 'dat' must be a logical vector.")
  }
}
newNames <- list(species = species, site = site, quad = quad,
                 year = year, trackID = trackID)
if (sum(sapply(newNames, is.character)) != 5) {
  badArgs <- paste("'", names(which(sapply(newNames, is.character) ==
                                      FALSE)), "'", collapse = ", and ")
  stop(paste0("The argument(s) ", badArgs, " must each contain a single character\nstring that gives the name(s) of the column(s) in 'dat' that contain the data\nfor ",
              badArgs))
} else {
  if (sum(unlist(newNames) %in% names(dat)) != 5) {
    badBadArgs <- paste("'", names(newNames)[which(!unlist(newNames) %in%
                                                     names(dat))], "'", collapse = ", and ")
    stop(paste0("The argument(s) ", badBadArgs, " contain values that are not column\nnames in 'dat'. These arguments must be character vectors that give the name(s)\nof the column(s) in 'dat' that contain the data for ",
                badBadArgs, ". Check for\nspelling errors, or make sure that you have included values for these arguments\nthat give the name of the columns in 'dat' that contain these data types."))
  }
}
usrNames <- unlist(newNames)
defaultNames <- c("Species", "Site", "Quad", "Year", "trackID")
names(dat)[match(usrNames, names(dat))] <- defaultNames
if ("Suspect" %in% names(dat)) {
  if (sum(names(dat) == "Type_USER") == 1) {
    datOut_1 <- aggregate(x = dat[, c("basalArea_ramet")],
                          by = list(Site = dat$Site, Quad = dat$Quad, Species = dat$Species,
                                    trackID = dat$trackID, Year = dat$Year, type = dat$Type_USER),
                          do_union = TRUE, FUN = sum)
    names(datOut_1)[names(datOut_1) == "basalArea_ramet"] <- "basalArea_genet"
    dat <- st_drop_geometry(dat)
    datOut_2 <- aggregate(x = dat[, c("recruit", "survives_tplus1",
                                      "age", "size_tplus1", "nearEdge", "Suspect")],
                          by = list(Site = dat$Site, Quad = dat$Quad, Species = dat$Species,
                                    trackID = dat$trackID, Year = dat$Year, type = dat$Type_USER),
                          FUN = mean)
    datOut <- merge(datOut_1, datOut_2, by = c("Site",
                                               "Quad", "Species", "trackID", "Year", "type"))
  }
  else {
    datOut_1 <- aggregate(x = dat[, c("basalArea_ramet")],
                          by = list(Site = dat$Site, Quad = dat$Quad, Species = dat$Species,
                                    trackID = dat$trackID, Year = dat$Year), do_union = TRUE,
                          FUN = sum)
    names(datOut_1)[names(datOut_1) == "basalArea_ramet"] <- "basalArea_genet"
    dat <- st_drop_geometry(dat)
    datOut_2 <- aggregate(x = dat[, c("recruit", "survives_tplus1",
                                      "age", "size_tplus1", "nearEdge", "Suspect")],
                          by = list(Site = dat$Site, Quad = dat$Quad, Species = dat$Species,
                                    trackID = dat$trackID, Year = dat$Year), FUN = mean)
    datOut <- merge(datOut_1, datOut_2, by = c("Site",
                                               "Quad", "Species", "trackID", "Year"))
  }
} else {
  if (sum(names(dat) == "Type_USER") == 1) {
    datOut_1 <- aggregate(x = dat[, c("basalArea_ramet")],
                          by = list(Site = dat$Site, Quad = dat$Quad, Species = dat$Species,
                                    trackID = dat$trackID, Year = dat$Year, type = dat$Type_USER),
                          do_union = TRUE, FUN = sum)
    names(datOut_1)[names(datOut_1) == "basalArea_ramet"] <- "basalArea_genet"
    dat <- st_drop_geometry(dat)
    datOut_2 <- aggregate(x = dat[, c("recruit", "survives_tplus1",
                                      "age", "size_tplus1", "nearEdge")], by = list(Site = dat$Site,
                                                                                    Quad = dat$Quad, Species = dat$Species, trackID = dat$trackID,
                                                                                    Year = dat$Year, type = dat$Type_USER), FUN = mean)
    datOut <- merge(datOut_1, datOut_2, by = c("Site",
                                               "Quad", "Species", "trackID", "Year", "type"))
  }
  else {
    datOut_1 <- aggregate(x = dat[, c("basalArea_ramet")],
                          by = list(Site = dat$Site, Quad = dat$Quad, Species = dat$Species,
                                    trackID = dat$trackID, Year = dat$Year), do_union = TRUE,
                          FUN = sum)
    names(datOut_1)[names(datOut_1) == "basalArea_ramet"] <- "basalArea_genet"
    dat <- st_drop_geometry(dat)
    datOut_2 <- aggregate(x = dat[, c("recruit", "survives_tplus1",
                                      "age", "size_tplus1", "nearEdge")], by = list(Site = dat$Site,
                                                                                    Quad = dat$Quad, Species = dat$Species, trackID = dat$trackID,
                                                                                    Year = dat$Year), FUN = mean)
    datOut <- merge(datOut_1, datOut_2, by = c("Site",
                                               "Quad", "Species", "trackID", "Year"))
  }
}
datOut[datOut$nearEdge > 0, "nearEdge"] <- 1
datOut$nearEdge <- as.logical(datOut$nearEdge)
if ("Suspect" %in% names(datOut)) {
  datOut$Suspect <- as.logical(datOut$Suspect)
}
datFinal <- datOut
names(datFinal)[match(defaultNames, names(datFinal))] <- usrNames
return(datFinal)

#### assign ####
ifClonal <- function(cloneDat, clonal, buffGenet, ...) {
  if (clonal == TRUE) {
    cloneDat$genetID <- plantTracker::groupByGenet(cloneDat,
                                                   buffGenet)
    tempCloneDat <- stats::aggregate(basalArea_ramet ~
                                       genetID, sum, data = cloneDat)
    names(tempCloneDat) <- c("tempGenetID", "basalArea_genet")
    cloneDat$basalArea_genet <- tempCloneDat[match(cloneDat$genetID,
                                                   tempCloneDat$tempGenetID), "basalArea_genet"]
  }
  else {
    cloneDat$genetID <- 1:nrow(cloneDat)
    cloneDat$basalArea_genet <- cloneDat$basalArea_ramet
  }
  return(cloneDat)
}
if (exists("assignOut") == TRUE) {
  suppressWarnings(rm("assignOut"))
}
dat <- sf::st_as_sf(x = dat, sf_column_name = "geometry")
dat$trackID <- as.character(NA)
dat$age <- as.integer(NA)
dat$size_tplus1 <- as.numeric(NA)
dat$recruit <- as.integer(NA)
dat$survives_tplus1 <- as.integer(NA)
dat$ghost <- as.integer(NA)
dat$basalArea_genet <- as.numeric(NA)
dat$genetID <- as.character(NA)
if (sum(dat$basalArea_ramet) == 0) {
  dat$basalArea_ramet <- sf::st_area(dat)
}
if (flagSuspects == TRUE) {
  dat$Suspect <- FALSE
}
if (sum(grepl(pattern = "[[:space:]]", x = dat$Species)) >
    0) {
  dat$sp_code_6 <- sapply(strsplit(dat$Species, " "), function(x) paste0(substr(toupper(x[1]),
                                                                                1, 3), substr(toupper(x[2]), 1, 3)))
} else if (sum(grepl(pattern = "_", x = dat$Species)) > 0) {
  dat$sp_code_6 <- sapply(strsplit(dat$Species, "_"), function(x) paste0(substr(toupper(x[1]),
                                                                                1, 3), substr(toupper(x[2]), 1, 3)))
}else {
  dat$sp_code_6 <- dat$Species
}
dat$index <- c(1:nrow(dat))
firstDatYear <- min(dat$Year)
firstYearIndex <- which(inv == firstDatYear)
tempPreviousYear <- dat[dat$Year == firstDatYear, ]
tempPreviousYear <- ifClonal(cloneDat = tempPreviousYear,
                             clonal = clonal, buffGenet = buffGenet)
IDs <- data.frame(genetID = sort(unique(tempPreviousYear$genetID)),
                  trackID = paste0(unique(dat$sp_code_6), "_", unique(tempPreviousYear$Year),
                                   "_", c(1:length(unique(tempPreviousYear$genetID)))))
tempPreviousYear$trackID <- IDs[match(tempPreviousYear$genetID,
                                      IDs$genetID), "trackID"]
tempPreviousYear$ghost <- 0
if (min(dat$Year) > inv[1]) {
  tempPreviousYear$recruit <- 1
  tempPreviousYear$age <- 0
}
if (min(dat$Year) == inv[1]) {
  tempPreviousYear$recruit <- NA
  tempPreviousYear$age <- NA
}
if (min(dat$Year) < inv[1]) {
  stop("Quadrat inventory dataset does not match years in the sample dataset")
}
if (inv[firstYearIndex] < max(inv)) {
  for (i in (firstYearIndex + 1):length(inv)) {
    if (inv[i] - inv[i - 1] > (dorm + 1)) {
      if (exists("assignOut") == TRUE) {
        assignOut <- rbind(assignOut, tempPreviousYear)
      } else {
        assignOut <- tempPreviousYear
      }
      ## is there any more data? if not, then end the loop
      if (inv[i] > max(dat$Year)) {
        break
      }
      tempPreviousYear <- dat[dat$Year == inv[i], ]
      if (nrow(tempPreviousYear) < 1) {
        tempPreviousYear <- dat[dat$Year == inv[i +
                                                  1], ]
        if (nrow(tempPreviousYear) > 0) {
          tempPreviousYear <- ifClonal(cloneDat = tempPreviousYear,
                                       clonal = clonal, buffGenet = buffGenet)
          tempPreviousYear$trackID <- paste0(tempPreviousYear$sp_code_6,
                                             "_", tempPreviousYear$Year, "_", tempPreviousYear$genetID)
        }
        next
      }
      if (nrow(tempPreviousYear) > 0) {
        if (sum(is.na(tempPreviousYear$genetID == TRUE)) >
            1) {
          tempPreviousYear <- ifClonal(cloneDat = tempPreviousYear,
                                       clonal = clonal, buffGenet = buffGenet)
          IDs <- data.frame(genetID = sort(unique(tempPreviousYear$genetID)),
                            trackID = paste0(unique(dat$sp_code_6),
                                             "_", unique(tempPreviousYear$Year), "_",
                                             c(1:length(unique(tempPreviousYear$genetID)))))
          tempPreviousYear$trackID <- IDs[match(tempPreviousYear$genetID,
                                                IDs$genetID), "trackID"]
        }
      }
    } else {
      tempCurrentYear <- sf::st_as_sf(dat[dat$Year ==
                                            inv[i], ])
      if (nrow(tempPreviousYear) < 1) {
        if (nrow(tempCurrentYear) > 0) {
          tempTrackIDs <- tempCurrentYear[is.na(tempCurrentYear$trackID) ==
                                            TRUE, ]
          tempTrackIDs <- ifClonal(tempTrackIDs, clonal = clonal,
                                   buffGenet = buffGenet)
          tempCurrentYear[is.na(tempCurrentYear$trackID) ==
                            TRUE, "trackID"] <- paste0(tempTrackIDs$sp_code_6,
                                                       "_", tempTrackIDs$Year, "_", tempTrackIDs$genetID)
          tempCurrentYear[is.na(tempCurrentYear$trackID) ==
                            TRUE, "basalArea_genet"] <- tempTrackIDs$basalArea_genet
          tempCurrentYear[(tempCurrentYear$Year - inv[i -
                                                        1]) < 2, c("age")] <- 0
          tempCurrentYear[(tempCurrentYear$Year - inv[i -
                                                        1]) < 2, c("recruit")] <- 1
        }
        if (inv[i] == max(inv)) {
          if (exists("assignOut") == TRUE) {
            assignOut <- rbind(assignOut, tempCurrentYear)
          } else {
            assignOut <- tempCurrentYear
          }
        } else {
          tempPreviousYear <- sf::st_as_sf(tempCurrentYear)
          next
        }
      } else {
        tempPreviousBuff <- sf::st_buffer(tempPreviousYear,
                                          buff)
        if (nrow(tempCurrentYear) < 1) {
          if (inv[i] != max(inv)) {
            ghosts <- tempPreviousYear[((inv[i + 1] -
                                           tempPreviousYear$Year) <= (dorm + 1)),
            ]
            if (nrow(ghosts) > 0) {
              ghosts$ghost <- "1"
            }
            deadGhosts <- tempPreviousYear[((inv[i +
                                                   1] - tempPreviousYear$Year) > (dorm +
                                                                                    1)), names(dat)]
            tempPreviousYear <- ghosts
            if (nrow(deadGhosts) > 0) {
              deadGhosts$survives_tplus1 <- 0
              if (exists("assignOut") == TRUE) {
                assignOut <- rbind(assignOut, deadGhosts)
              } else {
                assignOut <- deadGhosts
              }
            }
          } else {
            ghosts <- tempPreviousYear[(((inv[i] +
                                            1) - tempPreviousYear$Year) <= (dorm +
                                                                              1)), ]
            deadGhosts <- tempPreviousYear[(((inv[i] +
                                                1) - tempPreviousYear$Year) > (dorm +
                                                                                 1)), c(names(dat))]
            if (nrow(deadGhosts) > 0) {
              deadGhosts$survives_tplus1 <- 0
              if (exists("assignOut") == TRUE) {
                assignOut <- rbind(assignOut, deadGhosts)
              } else {
                assignOut <- deadGhosts
              }
            }
            if (nrow(ghosts) > 0) {
              ghosts$ghost <- "1"
              ghosts$survives_tplus1 <- NA
              ghosts$size_tplus1 <- NA
              if (exists("assignOut") == TRUE) {
                assignOut <- rbind(assignOut, ghosts)
              } else {
                assignOut <- ghosts
              }
            }
          }
          next
        } else {
          overlapArea <- suppressWarnings(sf::st_intersection(tempPreviousBuff,
                                                              tempCurrentYear))
          if (nrow(overlapArea) > 0) {
            overlapArea$parentName <- paste0(overlapArea$trackID,
                                             "__", overlapArea$index)
            overlapArea$childName <- paste0("childPoly__",
                                            overlapArea$index.1)
            overlapArea$overlappingArea <- sf::st_area(overlapArea$geometry)
            overlapArea <- sf::st_drop_geometry(overlapArea)
            overlaps <- overlapArea[, c("parentName",
                                        "childName", "overlappingArea")]
            overlapsTemp <- stats::reshape(overlaps,
                                           v.names = "overlappingArea", idvar = "parentName",
                                           timevar = "childName", direction = "wide")
            overlapsTemp$parentName <- sapply(strsplit(overlapsTemp$parentName,
                                                       "__"), unlist)[1, ]
            if (clonal == TRUE) {
              oldNames <- names(overlapsTemp)
              overlapsTemp <- stats::aggregate(x = overlapsTemp[,
                                                                2:ncol(overlapsTemp)], by = list(parentName = overlapsTemp$parentName),
                                               FUN = sum, na.rm = TRUE)
              overlapsTemp[which(overlapsTemp == 0,
                                 arr.ind = TRUE)] <- NA
              names(overlapsTemp) <- oldNames
            }
            rownames(overlapsTemp) <- as.character(overlapsTemp$parentName)
            names(overlapsTemp) <- c("parentName",
                                     sapply(strsplit(names(overlapsTemp)[2:ncol(overlapsTemp)],
                                                     "overlappingArea."), unlist)[2, ])
            overlaps <- as.data.frame(overlapsTemp[,
                                                   2:ncol(overlapsTemp)])
            rownames(overlaps) <- rownames(overlapsTemp)
            names(overlaps) <- names(overlapsTemp)[2:ncol(overlapsTemp)]
            if (clonal == TRUE) {
              multParents <- apply(X = overlaps, MARGIN = 2,
                                   FUN = function(x) sum(is.na(x) == FALSE))
              if (sum(multParents > 1) != 0) {
                ties <- names(multParents[multParents >
                                            1])
                if (length(ties) > 1) {
                  for (m in 1:ncol(as.data.frame(overlaps[,
                                                          ties]))) {
                    winner <- max(overlaps[, ties][,
                                                   m], na.rm = TRUE)
                    if (length(winner) == 1) {
                      overlaps[, ties][, m][overlaps[,
                                                     ties][, m] != winner] <- NA
                    } else if (length(winner) > 1) {
                      badChild_name <- names(overlaps[,
                                                      ties])[m]
                      badChild <- suppressWarnings(sf::st_centroid(tempCurrentYear[tempCurrentYear$index ==
                                                                                     as.numeric(strsplit(badChild_name,
                                                                                                         "__")[[1]][2]), ]))
                      badParents_names <- rownames(overlaps[is.na(overlaps[,
                                                                           ties][, m]) == FALSE, ])
                      badParents <- tempPreviousYear[tempPreviousYear$trackID %in%
                                                       badParents_names, ]
                      badParents <- suppressWarnings(sf::st_centroid(stats::aggregate(x = badParents[,
                                                                                                     "trackID"], by = list(trackID = badParents$trackID),
                                                                                      FUN = nrow, do_union = TRUE)))
                      dists <- sf::st_distance(badChild,
                                               badParents, which = "Euclidean")
                      rownames(dists) <- badChild_name
                      colnames(dists) <- badParents_names
                      smallDist <- colnames(dists)[which(dists ==
                                                           min(dists))]
                      overlaps[rownames(overlaps) !=
                                 smallDist, badChild_name] <- NA
                    }
                  }
                } else if (length(ties) == 1) {
                  winner <- max(overlaps[, ties], na.rm = TRUE)
                  if (length(which(overlaps == winner)) ==
                      1) {
                    overlaps[, ties][overlaps[, ties] !=
                                       winner] <- NA
                  } else if (length(which(overlaps ==
                                        winner)) > 1) {
                    badChild_name <- ties
                    badChild <- suppressWarnings(sf::st_centroid(tempCurrentYear[tempCurrentYear$index ==
                                                                                   as.numeric(strsplit(badChild_name,
                                                                                                       "__")[[1]][2]), ]))
                    badParents_names <- rownames(overlaps)[!is.na(overlaps[,
                                                                           ties])]
                    badParents <- tempPreviousYear[tempPreviousYear$trackID %in%
                                                     badParents_names, ]
                    badParents <- suppressWarnings(sf::st_centroid(stats::aggregate(x = badParents[,
                                                                                                   "trackID"], by = list(trackID = badParents$trackID),
                                                                                    FUN = nrow, do_union = TRUE)))
                    dists <- sf::st_distance(badChild,
                                             badParents, which = "Euclidean")
                    rownames(dists) <- badChild_name
                    colnames(dists) <- badParents_names
                    smallDist <- colnames(dists)[which(dists ==
                                                         min(dists))]
                    overlaps[rownames(overlaps) !=
                               smallDist, badChild_name] <- NA
                  }
                }
              }
              nameDF <- utils::stack(apply(X = t(overlaps),
                                           MARGIN = 1, function(x) names(x[which(is.na(x) ==
                                                                                   FALSE)])))
              names(nameDF) <- c("parentTrackID", "childIndex")
              nameDF$childIndex <- as.numeric(sapply(strsplit(as.character(nameDF$childIndex),
                                                              split = "__"), unlist)[2, ])
              tempCurrentYear[match(nameDF$childIndex,
                                    tempCurrentYear$index), "trackID"] <- nameDF$parentTrackID
            } else if (clonal == FALSE) {
              whileOverlaps <- overlaps
              done <- FALSE
              counter <- 0
              while (!done) {
                maxInds <- which(whileOverlaps == max(whileOverlaps,
                                                      na.rm = TRUE), arr.ind = TRUE)
                if (nrow(maxInds) > 1) {
                  maybeParents_names <- rownames(whileOverlaps)[unique(maxInds[,
                                                                               "row"])]
                  maybeChildren_names <- names(whileOverlaps)[unique(maxInds[,
                                                                             "col"])]
                  maybeChildren <- suppressWarnings(sf::st_centroid(tempCurrentYear[tempCurrentYear$index %in%
                                                                                      as.numeric(as.vector(data.frame(strsplit(x = maybeChildren_names,
                                                                                                                               split = "__"))[2, ])), ]))
                  maybeParents <- tempPreviousYear[tempPreviousYear$trackID %in%
                                                     maybeParents_names, ]
                  maybeParents <- suppressWarnings(sf::st_centroid(stats::aggregate(x = maybeParents[,
                                                                                                     "trackID"], by = list(trackID = maybeParents$trackID),
                                                                                    FUN = nrow, do_union = TRUE)))
                  dists <- sf::st_distance(maybeChildren,
                                           maybeParents, which = "Euclidean")
                  rownames(dists) <- maybeChildren_names
                  colnames(dists) <- maybeParents_names
                  smallDistInds <- which(dists == min(dists),
                                         arr.ind = TRUE)
                  smallChild <- rownames(dists)[smallDistInds[,
                                                              "row"]]
                  smallParent <- colnames(dists)[smallDistInds[,
                                                               "col"]]
                  tempCurrentYear[tempCurrentYear$index ==
                                    strsplit(x = smallChild, split = "__")[[1]][2],
                                  "trackID"] <- smallParent
                  whileOverlaps[smallParent, smallChild] <- 0
                  whileOverlaps[smallParent, ] <- 0
                  whileOverlaps[, smallChild] <- 0
                } else {
                  maxParent <- rownames(whileOverlaps)[maxInds[1,
                                                               1]]
                  maxChild <- as.numeric(strsplit(colnames(whileOverlaps)[maxInds[1,
                                                                                  2]], "__")[[1]][2])
                  tempCurrentYear[tempCurrentYear$index ==
                                    maxChild, "trackID"] <- maxParent
                  whileOverlaps[maxInds[1, 1], maxInds[1,
                                                       2]] <- 0
                  whileOverlaps[maxInds[1, 1], ] <- 0
                  whileOverlaps[, maxInds[1, 2]] <- 0
                }
                counter <- counter + 1
                if (counter > 500) {
                  stop("tracking 'while' loop is running out of control!")
                }
                if (sum(whileOverlaps, na.rm = TRUE) ==
                    0) {
                  done <- TRUE
                }
              }
            }
          }
          if (flagSuspects == TRUE) {
            smallPrevious <- stats::aggregate(x = sf::st_drop_geometry(tempPreviousYear[,
                                                                                        "basalArea_ramet"]), by = list(Year_prev = tempPreviousYear$Year,
                                                                                                                       trackID = tempPreviousYear$trackID),
                                              FUN = sum)
            names(smallPrevious)[3] <- "Area"
            smallCurrent <- stats::aggregate(x = sf::st_drop_geometry(tempCurrentYear[,
                                                                                      "basalArea_ramet"]), by = list(Year_curr = tempCurrentYear$Year,
                                                                                                                     trackID = tempCurrentYear$trackID), FUN = sum)
            names(smallCurrent)[3] <- "Area"
            shrinkage <- merge(smallPrevious, smallCurrent,
                               by = "trackID")
            if (nrow(shrinkage) > 0) {
              shrinkers <- shrinkage$trackID[(shrinkage$Area.y/shrinkage$Area.x) <=
                                               shrink]
              if (length(shrinkers) > 0) {
                tempPreviousYear[tempPreviousYear$trackID %in%
                                   shrinkers, "Suspect"] <- TRUE
              }
            }
            if (dorm >= 1 & length(unique(round(dat$basalArea_ramet,
                                                5))) > 3) {
              if (nrow(shrinkage) > 0) {
                dormants <- shrinkage[shrinkage$trackID %in%
                                        which((shrinkage$Year_curr - shrinkage$Year_prev) >
                                                1), ]
                if (nrow(dormants) > 0) {
                  dormants <- dormants[round(dormants$Area.x,
                                             8) != round(dormants$Area.y, 8),
                  ]
                  smallest <- exp(qnorm(p = dormSize,
                                        mean = mean(log(dat$Area)), sd = sd(log(dat$Area))))
                  tooSmallIDs <- dormants[dormants$Area.x <
                                            smallest, "trackID"]
                  tempPreviousYear[tempPreviousYear$trackID %in%
                                     tooSmallIDs, "Suspect"] <- TRUE
                }
              }
            }
          }
          orphans <- tempCurrentYear[is.na(tempCurrentYear$trackID) ==
                                       TRUE, ]
          if (nrow(orphans) > 0) {
            orphans$trackID <- paste0(orphans$sp_code_6,
                                      "_", orphans$Year, "_", 1:nrow(orphans))
            orphans$basalArea_genet <- orphans$basalArea_ramet
            if (inv[i] - inv[i - 1] <= 1) {
              orphans$recruit <- 1
              orphans$age <- 0
            }
          }
          parents <- tempPreviousYear[tempPreviousYear$trackID %in%
                                        tempCurrentYear$trackID, ]
          children <- tempCurrentYear[tempCurrentYear$trackID %in%
                                        tempPreviousYear$trackID, ]
          if (nrow(children) > 0) {
            if (inv[i] - inv[i - 1] <= 1) {
              children$recruit <- parents[match(children$trackID,
                                                parents$trackID), ]$recruit
              if (nrow(children[children$recruit ==
                                1 & is.na(children$recruit) == FALSE,
              ]) > 0) {
                children[children$recruit == 1 & is.na(children$recruit) ==
                           FALSE, ]$recruit <- 0
              }
            } else {
              children$recruit <- NA
            }
            tempParents <- sf::st_drop_geometry(parents[,
                                                        c("trackID", "age")])
            tempParents$age <- (tempParents$age + 1)
            names(tempParents) <- c("trackIDtemp",
                                    "age")
            children$age <- tempParents[match(children$trackID,
                                              tempParents$trackIDtemp), ]$age
            childGenet_area <- stats::aggregate(children$basalArea_ramet,
                                                by = list(trackID = children$trackID),
                                                sum)
            names(childGenet_area) <- c("trackID",
                                        "basalArea_genet")
            children <- merge(x = children[, names(children) !=
                                             "basalArea_genet"], y = childGenet_area,
                              by = "trackID")
          }
          if (nrow(parents) > 0) {
            parents[, "survives_tplus1"] <- 1
            parents[, "ghost"] <- 0
            childSizeTemp <- unique(sf::st_drop_geometry(children[,
                                                                  c("trackID", "basalArea_genet")]))
            names(childSizeTemp) <- c("trackIDtemp",
                                      "size_tplus1")
            parents$size_tplus1 <- childSizeTemp[match(parents$trackID,
                                                       childSizeTemp$trackIDtemp), ]$size_tplus1
          }
          if (dorm > 0) {
            ghostsTemp <- tempPreviousYear[!(tempPreviousYear$trackID %in%
                                               tempCurrentYear$trackID), ]
            if (inv[i] != max(inv)) {
              ghosts <- ghostsTemp[((inv[i + 1] - ghostsTemp$Year) <=
                                      (dorm + 1)), ]
              deadGhosts <- ghostsTemp[((inv[i + 1] -
                                           ghostsTemp$Year) > (dorm + 1)), ]
            } else {
              ghosts <- ghostsTemp[inv[i] + 1 - ghostsTemp$Year <=
                                     (dorm + 1), ]
              deadGhosts <- ghostsTemp[inv[i] + 1 -
                                         ghostsTemp$Year > (dorm + 1), ]
            }
            if (nrow(ghosts) > 0) {
              ghosts$ghost <- 1
              ghosts$survives_tplus1 <- NA
            }
            if (nrow(deadGhosts) > 0) {
              deadGhosts$survives_tplus1 <- 0
              deadGhosts$ghost <- 0
            }
            ghosts <- ghosts[, names(children)]
            deadGhosts <- deadGhosts[, names(children)]
          } else {
            deadGhosts <- tempPreviousYear[!(tempPreviousYear$trackID %in%
                                               tempCurrentYear$trackID), ]
            if (nrow(deadGhosts) > 0) {
              deadGhosts$survives_tplus1 <- 0
            }
            deadGhosts <- deadGhosts[, names(children)]
            ghosts <- NULL
          }
          orphans <- orphans[, names(children)]
          parents <- parents[, names(children)]
          tempCurrentYear <- rbind(children, orphans,
                                   ghosts)
          if (exists("assignOut") == TRUE) {
            assignOut <- rbind(assignOut, parents,
                               deadGhosts)
          }  else {
            assignOut <- suppressWarnings(rbind(parents,
                                                deadGhosts))
          }
          if (inv[i] == max(inv)) {
            assignOut <- rbind(assignOut, tempCurrentYear)
          }
          tempPreviousYear <- tempCurrentYear
        }
      }
    }
  }
} else if (inv[firstYearIndex] == max(inv)) {
  tempPreviousYear[, c("size_tplus1", "survives_tplus1")] <- NA
  assignOut <- tempPreviousYear
}
assignOut$nearEdge <- FALSE
if (inheritsFromTrackSpp == FALSE) {
  buffEdgeOutside <- sf::st_as_sfc(sf::st_bbox(assignOut))
  buffEdgeInside <- sf::st_as_sfc(sf::st_bbox(assignOut) +
                                    c(buff, buff, -buff, -buff))
  buffEdge <- sf::st_difference(buffEdgeOutside, buffEdgeInside)
} else if (inheritsFromTrackSpp == TRUE) {
  buffEdge <- nearEdgeBox
}
assignOut[sf::st_intersects(assignOut, buffEdge, sparse = FALSE),
          "nearEdge"] <- TRUE
assignOut <- assignOut[is.na(assignOut$Species) == FALSE,
                       !(names(assignOut) %in% c("ghost", "genetID", "index",
                                                 "sp_code_6"))]
