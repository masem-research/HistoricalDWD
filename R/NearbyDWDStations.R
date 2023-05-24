#' Identify nearby DWD stations
#'
#' @description The first Nearby DWD station of a serviced object is always identified.
#'
#' @param propertyData data.frame. Properties with longitude and latitude.
#' @param latitude character. Variable name latitude. default `LATITUDE`
#' @param longitude character. Variable name longitude. default `LONGITUDE`
#' @param radius integer. Radius in kilometers to be considered around longitude and latitude.
#' @param minDate character. Minimum date. Format: 'YY-MM-DD', e.g. '2021-12-25'. default: `2021-01-01`
#'
#' @return data.frame. Identified near-by stations with added information.
#' @export
#'
#' @examples
#' # Example 1:
#' PropertyData.1 <- data.frame(Proj_key = c(1021, 1378, 1672),
#'                              NAME = c("Helogland (Reede)", "Wyk auf Föhr (Alte Mole)", "Konstanz (Hafen)"),
#'                              LATITUDE = c(54.179837, 54.692713, 47.659868),
#'                              LONGITUDE = c(7.892211, 8.573618, 9.178899))
#'
#' NearbyDWDStationsDataFrame <- NearbyDWDStations(propertyData = PropertyData.1, radius = 50)
NearbyDWDStations <- function(propertyData,
                              latitude = "LATITUDE",
                              longitude = "LONGITUDE",
                              radius = 50,
                              minDate = '2021-01-01'){
  ## Get the nearby data to the objects in the propertyData file
  #   The longitude and latitude data of the nearby data are used.
  #   Query using lappyl function
  NearbyStationsList <- lapply(X = 1:nrow(propertyData), function(x) {
    rdwd::nearbyStations(propertyData[,latitude][x],
                         propertyData[,longitude][x],
                         radius = radius,
                         res = c("daily"),
                         var = c("kl") ,
                         mindate = as.Date(minDate))})

  ## Name elements in the list
  names(NearbyStationsList) <- propertyData$NAME

  ## Delete all recent entries in lists
  for (i in 1:length(NearbyStationsList)) {
    NearbyStationsList[[i]] <- NearbyStationsList[[i]][NearbyStationsList[[i]]$per == "historical",]
  }

  ## Append the sequence number and the Proj_key to each data.frame
  for (i in 1:length(NearbyStationsList)) {
    NearbyStationsList[[i]]$LfdNr <- 1:nrow(NearbyStationsList[[i]])
    NearbyStationsList[[i]]$NAME <- propertyData$NAME[i]
    NearbyStationsList[[i]]$Proj_key <- propertyData$Proj_key[i]
  }

  ## Generate data.frame
  NearbyStationsDataFrame <- do.call(rbind, NearbyStationsList)

  # Clean Stations_id --> GetDWDNetaData() functions returns a list with 5-digits-character values with leading zeros
  NearbyStationsDataFrame$Stations_id <- base::formatC(NearbyStationsDataFrame$Stations_id,
                                                       width = 5,
                                                       format = "d",
                                                       flag = "0")

  # return
  return(NearbyStationsDataFrame)

}
