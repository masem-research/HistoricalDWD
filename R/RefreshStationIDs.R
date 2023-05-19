#' Update DWD historical weather station IDs if NAs above threshold
#'
#' @param ValidationResultsDF
#' @param ExtractedRunningNo
#' @param IDExtractedWeatherStations
#'
#' @return character. Character vector with update of station ids to get historical DWD weather data for
#' @export
#'
#' @examples
RefreshStationIDs <- function(ValidationResultsDF,
                              NearbyDWDStationsDataFrame,
                              ExtractedRunningNo,
                              IDExtractedWeatherStations) {
  # Start
  print("[Message] Current run starts: new weather station IDs will be extracted")
  # Validation results in this round:
  print(ValidationResultsDF)

  # Build a new data.frame
  DFStationID <- data.frame(STATION_ID = ValidationResultsDF$STATION_ID,
                            ExtractedRunningNo = ExtractedRunningNo)
  # Correct STATION_ID
  DFStationID$STATION_ID <-
    base::formatC(DFStationID$STATION_ID, width = 5, format = "d", flag = "0")
  # Replace
  DFStationID$ReplaceStation <- ValidationResultsDF$GetNewWeatherStation
  # Increase ExtractedRunningNo
  DFStationID$ExtractedRunningNo[DFStationID$ReplaceStation] <- DFStationID$ExtractedRunningNo[DFStationID$ReplaceStation] + 1
  # Correct order of stations: Reshuffle by original vector with extracted stations
  DFStationID <- DFStationID[order(match(DFStationID[[1]], IDExtractedWeatherStations)), ]
  # Take the highest value in ExtractedRunningNo and get the relevant stations again
  DFStationID$NewStationID <-
    IDExtractionDWDWeatherStations(NearbyDWDStationsDataFrame = NearbyDWDStationsDataFrame,
                                   ExtractedRunningNo = max(DFStationID$ExtractedRunningNo))
  # Combine existing and NewStationID and extract vector to extract the weather-stations
  DFStationID
  # Copy the old vector with IDs first
  DFStationID$NewVectorToExtract <-  DFStationID$STATION_ID
  # Replace existing IDs corresponding to ReplaceStation = TRUE-Flag
  DFStationID$NewVectorToExtract[DFStationID$ReplaceStation] <- DFStationID$NewStationID[DFStationID$ReplaceStation]

  # print
  print("[Message] data.frame with new vector to extract generated:\n")
  print(DFStationID)

  # return: data.frame
  return(DFStationID)

}
