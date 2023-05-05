#' Extract IDs of the closest identified DWD weather stations as vector
#'
#' @param NearbyDWDStationsDataFrame data.frame. Downloaded NearBy DWD weather stations.
#' @param DWDIDColumn character. Column with the DWD weather station ID. default: `Stations_id`
#' @param RunningNoColumn character. Column with the sequence number of the extracted DWD weather stations. default: `LfdNr`
#' @param ExtractedRunningNo integer. Sequence number of the identified near-by stations to be extracted. default `2`
#'
#' @return character. Vector with the DWD weather station IDs, five digits. Three and four digit IDs are padded with leading zeros.
#' @export
#'
#' @examples
#'
#' # Generate some test data
#' PropertyData.1 <- data.frame(Proj_key = c(1021, 1378, 1672),
#'                              NAME = c("Helogland (Reede)", "Wyk auf Föhr (Alte Mole)", "Konstanz (Hafen)"),
#'                              LATITUDE = c(54.179837, 54.692713, 47.659868),
#'                              LONGITUDE = c(7.892211, 8.573618, 9.178899))
#' # Get DWD meta data
#' NearbyDWDStationsDataFrame <- NearbyDWDStations(propertyData = PropertyData.1, radius = 50)
#'
#' # Extract IDs of the closest identified DWD wweather stations
#' IDExtractionDWDWeatherStations(NearbyDWDStationsDataFrame = NearbyDWDStationsDataFrame, ExtractedRunningNo = 2)
IDExtractionDWDWeatherStations <- function(NearbyDWDStationsDataFrame, DWDIDColumn = "Stations_id",
                                           RunningNoColumn = "LfdNr", ExtractedRunningNo = 2) {
  # The IDs of the identified DWD weather stations are extracted a
  IDsAllStations <- NearbyDWDStationsDataFrame[NearbyDWDStationsDataFrame[RunningNoColumn] == ExtractedRunningNo, DWDIDColumn]
  # Add leading zeros and convert to character with formatC() function
  IDsAllStationsAsVectorWith5Characters <-
    formatC(IDsAllStations, width = 5, format = "d", flag = "0")
  # return
  return(IDsAllStationsAsVectorWith5Characters)
}
