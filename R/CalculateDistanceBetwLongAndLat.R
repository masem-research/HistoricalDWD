#' Distance between lat-long coordinates
#'
#' @details: Great-circle distance between points at lat-long coordinates.
#' The core is a copy of OSMscale::earthDist Version 0.5.3 (2017-04-19).
#' <https://github.com/brry/OSMscale/blob/master/R/earthDist.R#L57-L102>.
#' No checks of coordinates etc.
#'
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Aug 2016 + Jan 2017.
#'         Thank you very much!
#'         Angle formula from Diercke Weltatlas 1996, Page 245
#'
#' @param ObjectLat numeric. Latitude (North/South) coordinates in decimal degrees
#' @param ObjectLong numeric. Longitude (East/West) coordinates in decimal degrees
#' @param DFLongLat data.frame. data.frame with the columns `LAT` and `LONG`
#' @param RadiusOfEarth radius of the earth. Could be given in miles. DEFAULT: 6371 (km)
#' @param i Integer: Index element against which all coordinate pairs
#'          are computed. DEFAULT: 1
#'
#' @return Vector with distance(s) in km (or units of `r`, if `r` is changed)
#' @export
#'
#' @examples
#' PropertyData <- data.frame(Proj_key = c(1021, 1378, 1672, 1732, 1862),
#'                            ObjectName = c("Helogland (Reede)", "Wyk auf Föhr (Alte Mole)",
#'                                           "Konstanz (Hafen)", "Greetsiel (Hafen)",
#'                                           "Dornumersiel Hafen"),
#'                            LATITUDE = c(54.179837, 54.692713, 47.659868, 53.50214, 53.68001),
#'                            LONGITUDE = c(7.892211, 8.573618, 9.178899, 7.09950, 7.48596))
#'
#' StationData <- data.frame(StationName = c("WARNEMUENDE", "JEVER", "LEUCHTTURM KIEL", "NAObject", "NORDERNEY"),
#'                           LAT =  c(54.11, 53.32, 54.30, NA, 53.43),
#'                           LONG = c(12.05,  7.53, 10.16, NA,  7.09))
#'
#' CalculateDistanceBetwLongAndLat(ObjectDF = PropertyData,
#'                                 ObjectLatName = "LATITUDE",
#'                                 ObjectLongName = "LONGITUDE",
#'                                 StationDF = StationData,
#'                                 StationLatName = "LAT",
#'                                 StationLongName = "LONG",
#'                                 RadiusAroundObject = 100,
#'                                 RadiusOfEarth = 6371)
CalculateDistanceBetwLongAndLat <- function(
    ObjectDF,
    ObjectLatName,
    ObjectLongName,
    StationDF,
    StationLatName,
    StationLongName,
    RadiusAroundObject = 100,
    RadiusOfEarth = 6371) {

  ## Validate the data
  #   Delete all incomplete pairs
  #    Object data.frame
  MissingRowsInObjectDF <- complete.cases(ObjectDF)
  if (any(!MissingRowsInObjectDF)) {
    message("Missing pairs in object data.frame removed.")
    ObjectDF <- ObjectDF[!MissingRowsInObjectDF,]
  }
  #    Station data.frame
  MissingRowsInStationDF <- complete.cases(StationDF)
  if (any(!MissingRowsInStationDF)) {
    message("Missing pairs in station data.frame removed.")
    StationDF <- StationDF[MissingRowsInStationDF,]
  }

  ## Convert degree angles to radians
  #   Object data.frame
  # TODO: Change the names
  ObjectDF$LatInRadiansObject  <- ObjectDF[, ObjectLatName]  * pi/180  # y2
  ObjectDF$LongInRadiansObject <- ObjectDF[, ObjectLongName] * pi/180  # x2
  #   Station data.frame
  StationDF$LatInRadiansStation  <- StationDF[, StationLatName]  * pi/180  # y1
  StationDF$LongInRadiansStation <- StationDF[, StationLongName] * pi/180  # x1

  ## Get the distances
  # Empty list for results
  DistanceObjectStationsList <- list()
  # Station by station
  for (i in 1:nrow(ObjectDF)) {

    # Merge StationDF and ObjectDF
    StationObjectDF <- merge.data.frame(x = StationDF, y = ObjectDF[i,])

    # angle preparation (numerical inaccuracies may lead to 1.0000000000000002):
    StationObjectDF$cosinusangle <- sin(StationObjectDF$LatInRadiansStation) * sin(StationObjectDF$LatInRadiansObject) +
      cos(StationObjectDF$LatInRadiansStation) * cos(StationObjectDF$LatInRadiansObject) *
      cos(StationObjectDF$LongInRadiansStation - StationObjectDF$LongInRadiansObject)
    StationObjectDF$cosinusangle <- replace(StationObjectDF$cosinusangle, StationObjectDF$cosinusangle > 1, 1)
    # angle between lines from earth center to coordinates:
    StationObjectDF$angle <- acos(StationObjectDF$cosinusangle)
    # set distance between the same coordinates to exactly zero:
    tol <- sqrt(.Machine$double.eps) # equality tolerance
    StationObjectDF$samepoint <- abs(StationObjectDF$LongInRadiansObject - StationObjectDF$LongInRadiansStation) < tol &
      abs(StationObjectDF$LatInRadiansObject - StationObjectDF$LatInRadiansStation) < tol
    StationObjectDF$angle[StationObjectDF$samepoint] <- 0 # again, to compensate numerical inaccuracies
    # compute great-circle-distance:
    StationObjectDF$DistanceKM <- RadiusOfEarth * StationObjectDF$angle
    # Clean-up variables
    StationObjectDF[, c("LatInRadiansStation", "LongInRadiansStation",
                        "LatInRadiansObject", "LongInRadiansObject",
                        "cosinusangle", "angle", "samepoint")] <- NULL
    # Clean-up entries due to radius
    CleanUpDueToRadius <- TRUE
    if (CleanUpDueToRadius) {
      StationObjectDF <- StationObjectDF[StationObjectDF$DistanceKM < RadiusAroundObject,]
    }
    # Sort by distance
    StationObjectDF <- StationObjectDF[order(StationObjectDF$DistanceKM),]
    # Write into list
    DistanceObjectStationsList[[i]] <- StationObjectDF
  }

  ## Combine all data.frames
  DistanceObjectStationsDF <- do.call(rbind, DistanceObjectStationsList)

  ## return
  return(DistanceObjectStationsDF)
  }
