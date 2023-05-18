#' Main function: Get a data.frame with historical weather data from DWD
#'
#' @param DataFrame
#' @param Proj_key
#' @param NAME
#' @param LATITUDE
#' @param LONGITUDE
#'
#' @return list. Returns a list with the different data.frames
#' @export
#'
#' @examples
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
#'                                       EndYear = 2022,
#'                                       PrintMessages = TRUE)
HistoricalDWDWeatherData <- function(DataFrame = PropertyData.1,
                                     Proj_key = Proj_key,
                                     NAME = NAME,
                                     LATITUDE = LATITUDE,
                                     LONGITUDE = LONGITUDE,
                                     ExtractedRunningNo = 2,
                                     StartYear = 2018,
                                     EndYear = 2022,
                                     PrintMessages = TRUE) {

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


  ## Reduced historical data from identified weather stations - between start and end date
  #   and the core variables
  ListWithResults[["HistoricalWeatherDataDFReduced"]] <-
    HistoricalWeatherDataDF[as.numeric(format(HistoricalWeatherDataDF$MESS_DATUM, "%Y")) >= StartYear &
                              as.numeric(format(HistoricalWeatherDataDF$MESS_DATUM, "%Y")) <= EndYear ,
                            c("STATIONS_ID", "MESS_DATUM", "RSK", "TMK")]

  ## DWD historical data.frame complete or not?
  ## Generate a complete time.series between start- and end-year
  #   Missing Entries will be replaced by NA
  WXValidationDF <- CheckIfWeatherDataIsComplete(HistoricalWeatherDataFrameToTest =
                                                 ListWithResults$HistoricalWeatherDataDFReduced,
                                                 StartYear = StartYear,
                                                 EndYear = EndYear,
                                                 silent = !PrintMessages)
  # Write list into list
  ListWithResults[["WXValidationDF"]] <- WXValidationDF

  # Get the data.frame with missing values
  browser()


  # Threshold: 3% of missing values are okay and will be imputed using imputeTS
  # Calculate Threshold
  # Theoretical number of entires per station
  ListWithResults$WXValidationDF$TheoreticalNumberOfValidEntries


  ## Argument (TRUE/FALSE): Replace incomplete data.frame by next ID in list?

  ## Generate map with all stations using tmap


  ## return
  return(ListWithResults)


}



