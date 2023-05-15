#' Main function: Get a data.frame with historical weather data from DWD
#'
#' @param DataFrame
#' @param Proj_key
#' @param NAME
#' @param LATITUDE
#' @param LONGITUDE
#'
#' @return
#' @export
#'
#' @examples
#'
#' PropertyData.1 <- data.frame(Proj_key = c(1021, 1378, 1672),
#'                              NAME = c("Helogland (Reede)", "Wyk auf Föhr (Alte Mole)", "Konstanz (Hafen)"),
#'                              LATITUDE = c(54.179837, 54.692713, 47.659868),
#'                              LONGITUDE = c(7.892211, 8.573618, 9.178899))
#'
#' HistDWDDF <- HistoricalDWDWeatherData(DataFrame = PropertyData.1,
#'                                       Proj_key = Proj_key,
#'                                       NAME = NAME,
#'                                       LATITUDE = LATITUDE,
#'                                       LONGITUDE = LONGITUDE,
#'                                       ExtractedRunningNo = 2,
#'                                       StartYear = 2018,
#'                                       EndYear = 2022)
HistoricalDWDWeatherData <- function(DataFrame = PropertyData.1,
                                     Proj_key = Proj_key,
                                     NAME = NAME,
                                     LATITUDE = LATITUDE,
                                     LONGITUDE = LONGITUDE,
                                     ExtractedRunningNo = 2,
                                     StartYear = 2018,
                                     EndYear = 2022) {

  # Empty list for results
  ListWithResults <- list()

  ## Get Nearby DWD Stations
  NearbyDWDStationsDataFrame <- NearbyDWDStations(propertyData = PropertyData.1, radius = 50)

  # Write into list
  ListWithResults[["NearbyDWDStationsDataFrame"]] <- NearbyDWDStationsDataFrame


  ## Get DWD Metadata
  DWDHistoricalMetaDataAllStations <- GetDWDMetaData(OnlyCurrentDate = FALSE)

  # Write into list
  ListWithResults[["DWDHistoricalMetaDataAllStations"]] <- DWDHistoricalMetaDataAllStations


  ## Extract IDs of the closest identified DWD wweather stations
  IDExtractedWeatherStations <-
    IDExtractionDWDWeatherStations(NearbyDWDStationsDataFrame = ListWithResults$NearbyDWDStationsDataFrame,
                                   ExtractedRunningNo = ExtractedRunningNo)

  # Write into list
  ListWithResults[["IDExtractedWeatherStations"]] <- IDExtractedWeatherStations


  ## Get the historical data from the identified weather stations
  HistoricalWeatherDataDF <- GetHistoricalDWDWeatherData(VectorWithIDsDWDWeatherStations =
                                                           ListWithResults$IDExtractedWeatherStations)

  # Write into list
  ListWithResults[["HistoricalWeatherDataDF"]] <- HistoricalWeatherDataDF


  ## First and latest entries
  print(aggregate(x = ListWithResults$HistoricalWeatherDataDF[,"MESS_DATUM", drop = F],
                  by =  list(ListWithResults$HistoricalWeatherDataDF$STATIONS_ID), max))

  # Check the oldest entry
  print(aggregate(x = ListWithResults$HistoricalWeatherDataDF[,"MESS_DATUM", drop = F],
                  by =  list(ListWithResults$HistoricalWeatherDataDF$STATIONS_ID), min))

  ## Check if the weather data is complete over the given timeframe
  #   The data.frames should have an entry for every day


  ## Return
  return(ListWithResults)


}



