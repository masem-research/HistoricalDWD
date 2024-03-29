#' Main function: Get a data.frame with historical weather data from DWD
#'
#' @details Aggregated data.frame is stored in generated list in object `HistoricalWeatherDataAggregated`
#'
#' @param DataFrame
#' @param Proj_key
#' @param NAME
#' @param LATITUDE
#' @param LONGITUDE
#' @param ExtractedRunningNo integer. Extracted DWD weather station in downloaded list. It is the list with smallest
#' path to object coordinates. default: `2`
#' @param StartYear. integer. default: `2018`
#' @param EndYear. integer. default: `2022`
#' @param PrintMessages. boolean. default: `TRUE`
#' @param ThresholdNAs. double. default: `0.05`
#'
#' @return list. Returns a list with the different data.frames
#' @export
#'
#' @examples
#' PropertyData.1 <- data.frame(Proj_key = c(1021, 1378, 1672, 1732, 1862),
#'                              NAME = c("Helogland (Reede)", "Wyk auf Föhr (Alte Mole)",
#'                                       "Konstanz (Hafen)", "Greetsiel (Hafen)",
#'                                       "Dornumersiel Hafen"),
#'                              LATITUDE = c(54.179837, 54.692713, 47.659868, 53.50214, 53.68001),
#'                              LONGITUDE = c(7.892211, 8.573618, 9.178899, 7.09950, 7.48596))
#'
#' HistDWDDF <- HistoricalDWDWeatherData(DataFrame = PropertyData.1,
#'                                       Proj_key = Proj_key,
#'                                       NAME = NAME,
#'                                       LATITUDE = "LATITUDE",
#'                                       LONGITUDE = "LONGITUDE",
#'                                       ExtractedRunningNo = 2,
#'                                       StartYear = 2018,
#'                                       EndYear = 2022,
#'                                       PrintMessages = TRUE,
#'                                       ThresholdNAs = 0.05)
HistoricalDWDWeatherData <- function(DataFrame = PropertyData.1,
                                     Proj_key = Proj_key,
                                     NAME = NAME,
                                     LATITUDE = LATITUDE,
                                     LONGITUDE = LONGITUDE,
                                     ExtractedRunningNo = 2,
                                     StartYear = 2018,
                                     EndYear = 2022,
                                     PrintMessages = TRUE,
                                     ThresholdNAs = 0.05) {


  ## Empty list for results
  ListWithResults <- list()

  ## Get Nearby DWD Stations
  #  Build a vector with object information from input data.frame
  df_args <- c(DataFrame, sep = " ")
  VectorWithObjectInformation <- do.call(paste, df_args)
  # empty list for nearby DWD stations
  NearbyDWDStationsList <- list()
  # Get the DWD stations - one by one
  for (i in 1:nrow(DataFrame)) {
    # print message
    print(paste("[Message] Requesting object:", VectorWithObjectInformation[i]))
    # request data and write it into a list
    NearbyDWDStationsList[[i]] <- NearbyDWDStations(propertyData = DataFrame[i,],
                                                    radius = 50,
                                                    latitude = LATITUDE,
                                                    longitude = LONGITUDE)
    # Set names
    names(NearbyDWDStationsList)[[i]] <- DataFrame[i, "Proj_key"]
  }

  # Write into list
  ListWithResults[["NearbyDWDStationsList"]] <- NearbyDWDStationsList


  ## Get DWD Metadata
  DWDHistoricalMetaDataAllStations <- GetDWDMetaData(OnlyCurrentDate = FALSE)
  # Please note: Stations_id is a 5-digit-character with leading zeros

  # Write into list
  ListWithResults[["DWDHistoricalMetaDataAllStations"]] <- DWDHistoricalMetaDataAllStations


  # One by one: get the ids of all DWD WX stations of the first object
  IdentifiedDWDStationIDs <- list()
  for (i in 1:nrow(DataFrame)) {
    # Extract all IDs in a new list
    IdentifiedDWDStationIDs[[i]] <- ListWithResults$NearbyDWDStationsList[[i]][-1,"Stations_id"] # 1st entry will be dropped
    # Add object id
    # Set names
    names(IdentifiedDWDStationIDs)[[i]] <- paste0("Proj_key_", DataFrame[i, "Proj_key"])
  }

  ListWithResults[["ObjectSpecificDWDStationIDsList"]] <- IdentifiedDWDStationIDs


  ## Loop over different objects
  #   Empty lists
  HistoricalWeatherDataList <- list() # time series
  WXValidationList <- list()          # meta-data
  IdentifiedWeatherStations <- list() # identified weather stations
  #   while()-loop
  for (j in 1:length(ListWithResults[["ObjectSpecificDWDStationIDsList"]])) {
    ## messages
    print("Request WX data for object:")
    print(names(ListWithResults[["ObjectSpecificDWDStationIDsList"]][j]))
    print(VectorWithObjectInformation[j])

    ## Loop over the WX station until the NA-threshold value is reached for TMK and RSK
    # Startvalue
    GetNewWeatherStation <- TRUE   # Start value while loop
    StationNoInList <- 1           # Start value weather station in list
    while (GetNewWeatherStation) {
      print(paste("Get new WX station?:", GetNewWeatherStation))

      # Set Station No in List
      print(paste("Station No in List:", StationNoInList))

      #   1st object
      #  Get the historical weather data for first DWD station ID
      HistoricalWeatherDataDF <-
        GetHistoricalDWDWeatherData(VectorWithIDsDWDWeatherStations =
                                      ListWithResults[["ObjectSpecificDWDStationIDsList"]][[j]][StationNoInList])
      # Correct Station-IDs: rdwd package function returns station id as integer without leading nulls!
      HistoricalWeatherDataDF$STATIONS_ID <-
        base::formatC(HistoricalWeatherDataDF$STATIONS_ID, width = 5, format = "d", flag = "0")


      ## Reduced historical data from identified weather stations - between start and end date
      #   and the core variables
      HistoricalWeatherDataDF <-
        HistoricalWeatherDataDF[as.numeric(format(HistoricalWeatherDataDF$MESS_DATUM, "%Y")) >= StartYear &
                                  as.numeric(format(HistoricalWeatherDataDF$MESS_DATUM, "%Y")) <= EndYear ,
                                c("STATIONS_ID", "MESS_DATUM", "RSK", "TMK")]

      # Check the data if complete or not - if NA > Threshold, pick the next ID, else: impute NAs and
      #  write back into the list
      WXValidationDF <- CheckIfWeatherDataIsComplete(HistoricalWeatherDataFrameToTest =
                                                     HistoricalWeatherDataDF,
                                                     StartYear = StartYear,
                                                     EndYear = EndYear,
                                                     IDExtractedWeatherStations = ExtractedRunningNo,
                                                     silent = !PrintMessages,
                                                     OrderIDsWXStations =  ListWithResults[["ObjectSpecificDWDStationIDsList"]][[1]][1])

      # increase station number in list by 1
      if (WXValidationDF$ValidationAggrDFPercentageValues$GetNewWeatherStation) {
        StationNoInList <- StationNoInList + 1
        # set GetNewWeatherStation to TRUE
      }

      # Change the counter
      GetNewWeatherStation <- WXValidationDF$ValidationAggrDFPercentageValues$GetNewWeatherStation

    } # end of while() loop: WX station below threshold

    #browser()

    ## identified data.frame into list
    WXValidationList[[j]] <- WXValidationDF
    names(WXValidationList)[[j]] <- WXValidationDF$ValidationAggrDFPercentageValues$STATION_ID
    HistoricalWeatherDataList[[j]] <- HistoricalWeatherDataDF
    names(HistoricalWeatherDataList)[[j]] <- WXValidationDF$ValidationAggrDFPercentageValues$STATION_ID
    IdentifiedWeatherStations[[j]] <- WXValidationDF$ValidationAggrDFPercentageValues
    names(IdentifiedWeatherStations)[[j]] <- WXValidationDF$ValidationAggrDFPercentageValues$STATION_ID

  } # end of for() loop: WX stations for all objects


  ## Write results into list
  ListWithResults[["WXValidationList"]] <- WXValidationList
  ListWithResults[["HistoricalWeatherDataList"]] <- HistoricalWeatherDataList
  ListWithResults[["IdentifiedWeatherStations"]] <- IdentifiedWeatherStations


  ## Add project keys to IdentifiedWeatherStations
  ListWithResults$IdentifiedWeatherStationsDF <- do.call(rbind, IdentifiedWeatherStations)
  # Add project keys to final list
  ListWithResults$IdentifiedWeatherStationsDF$Proj_key <- DataFrame["Proj_key"]
  ListWithResults$IdentifiedWeatherStationsDF$Proj_Name <- DataFrame["NAME"]


  ## Aggregate by STATIONS_ID and MESS_DATUM
  ListWithResults$HistoricalWeatherDataCompleteList <-
    GenerateCompleteTimeSeriesDataFrame(StartYear = StartYear, EndYear = EndYear,
                                        HistDataList = ListWithResults$HistoricalWeatherDataList)

  ## Imputation
  for (i in 1:length(ListWithResults$HistoricalWeatherDataCompleteList)) {
    # RSK
    ListWithResults$HistoricalWeatherDataCompleteList[[i]]$RSKImputed <-
      imputeTS::na_kalman(ListWithResults$HistoricalWeatherDataCompleteList[[i]]$RSK)
    # TMK
    ListWithResults$HistoricalWeatherDataCompleteList[[i]]$TMKImputed <-
      imputeTS::na_kalman(ListWithResults$HistoricalWeatherDataCompleteList[[i]]$TMK)

  }

  ## Aggregate by day and month
  ListWithResults[["HistoricalWeatherDataImputedAggregated"]] <-
    lapply(X = ListWithResults[["HistoricalWeatherDataCompleteList"]],
           FUN = function(x) {AggregateByStationIDAndYear(DFToAggregate = x,
                                                          AggregationFunction = "mean")})

  ## Final check
  print("[Message] Number of missing values in time.series RSK:")
  print(sapply(X = ListWithResults$HistoricalWeatherDataImputedAggregated, FUN = function(x) {sum(is.na(x$RSKmean))}))
  print("[Message] Number of missing values in time.series TMK:")
  print(sapply(X = ListWithResults$HistoricalWeatherDataImputedAggregated, FUN = function(x) {sum(is.na(x$TMKmean))}))

  ## Final Mapping List
  cat("--------------------------------------------------------------------------------------------------------------------------------------")
  cat("\n\nFinal list with identified weather stations corresponding to given Project-Keys:\n")
  print(ListWithResults$IdentifiedWeatherStationsDF)
  cat("\n\n")
  cat("--------------------------------------------------------------------------------------------------------------------------------------")


  ## Function stop
  return(ListWithResults)


}



