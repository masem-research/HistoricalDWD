#' Check if forecasting data is available
#'
#' @descriptions Checks if forecasting data for selected parameters is available. Add the availability information
#' to the weatherstation metadata data.frame
#'
#' @param DWDForecastingDatList
#' @param DWDForecastingMetaDataFromKMLObject
#' @param DistancesObjectWXStations
#'
#' @return data.frame. Weatherstation metadata.
#' @export
#'
#' @examples
CheckIfForecastingDataIsAvailable <- function(DWDForecastingDatList = DWDForecastingDatList,
                                              DWDForecastingMetaDataFromKMLObject = DWDForecastingMetaDataFromKMLObject,
                                              DistancesObjectWXStations = DistancesObjectWXStations) {
  ## Check if data is available:
  # Number of entries in each column
  ParameterIsAvailable <- lapply(X = DWDForecastingDatList, function(x) colSums(!is.na(x)))
  # Add station name
  names(ParameterIsAvailable) <- DWDForecastingMetaDataFromKMLObject$name
  # Convert into data.frames
  ParameterIsAvailableDFs <- lapply(ParameterIsAvailable, data.frame)
  # Add a ParameterName column
  str(ParameterIsAvailable)
  for (i in 1:length(ParameterIsAvailableDFs)) {
    names(ParameterIsAvailableDFs[[i]]) <- names(ParameterIsAvailableDFs[i])
  }
  # Copy the parameter names (row.names) into a variable called ParameterName
  for (i in 1:length(ParameterIsAvailableDFs)) {
    ParameterIsAvailableDFs[[i]]$"ParameterName" <- row.names(ParameterIsAvailableDFs[[i]])
  }

  ## Extract DWD Forecasting weather data
  ParameterIsAvailableDF <- Reduce(function(...) merge(..., all = TRUE, sort = FALSE), ParameterIsAvailableDFs)
  # set row.names
  row.names(ParameterIsAvailableDF) <- ParameterIsAvailableDF$ParameterName
  # delete parameter variable, otherwise all variables in transposed data.frame are character variables
  ParameterIsAvailableDF$ParameterName <- NULL
  # transpose
  ParameterIsAvailableDF <- t(ParameterIsAvailableDF)
  ParameterIsAvailableDF <- as.data.frame(ParameterIsAvailableDF)
  # Set stationnames as variable
  ParameterIsAvailableDF$StationName <- row.names(ParameterIsAvailableDF)
  ParameterIsAvailableDF
  # merge data to complete data.frame
  head(DistancesObjectWXStations) # ID
  DistancesObjectWXStations <- merge.data.frame(x = DistancesObjectWXStations,
                                                y = ParameterIsAvailableDF,
                                                by.x = "ID", by.y = "StationName",
                                                all.x = TRUE )

  # return the final meta data.frame
  return(DistancesObjectWXStations)
}
