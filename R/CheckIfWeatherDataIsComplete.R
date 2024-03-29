#' Checks if a DWD historical weather data.frame has complete data in between a given start and end date
#'
#' @details
#' Checks whole years and if data is available on every day; Start year starts at 1st of January, end of year
#' 31st of December
#'
#' @param HistoricalWeatherDataFrameToTest data.frame.
#' @param StartYear integer. default: `2022`
#' @param EndYear integer. default: `2022`
#' @param silent boolean. Should messages be print? default: `TRUE`
#' @param thresholdNAs double. Above which relative amount of NAs (in points) should a time.series get rejected?
#' default: `0.05`
#' @param IDExtractedWeatherStations interger. default value is `2` (corresponds to first entry in list with weather stations)
#' @param OrderIDsWXStations character vector. Vector with weather-station IDs in original order.
#'
#' @return list.
#' @export
#'
#' @examples
#' # Some weather station IDs
#' VectorIDsSomeWeatherStations <- c("02115", "01963", "02712")
#'
#' # Get the data from the weather stations: historical and recent
#' HistoricalWeatherDataDF <- GetHistoricalDWDWeatherData(VectorWithIDsDWDWeatherStations = VectorIDsSomeWeatherStations)
#' # First six rows
#' head(HistoricalWeatherDataDF)
#' CheckIfWeatherDataIsComplete(HistoricalWeatherDataFrameToTest = HistoricalWeatherDataDF)
CheckIfWeatherDataIsComplete <- function(HistoricalWeatherDataFrameToTest,
                                         StartYear = 2022,
                                         EndYear = 2022,
                                         silent = TRUE,
                                         thresholdNAs = 0.05,
                                         IDExtractedWeatherStations = 2,
                                         OrderIDsWXStations) {

  #browser()

  ## Empty list for results
  ListValidateData <- list()

  ## Print out first and latest entries
  # First entry
  FirstEntry <- aggregate(x = HistoricalWeatherDataFrameToTest[,"MESS_DATUM", drop = F],
                          by =  list(HistoricalWeatherDataFrameToTest$STATIONS_ID), max)
  # Check the oldest entry
  LatestEntry <- aggregate(x = HistoricalWeatherDataFrameToTest[,"MESS_DATUM", drop = F],
                           by =  list(HistoricalWeatherDataFrameToTest$STATIONS_ID), min)
  # Combine and rename
  CombinedDF <- merge.data.frame(x = LatestEntry, y = FirstEntry, by = "Group.1")
  colnames(CombinedDF) <- c("WX_ID", "First_Entry", "Latest_Entry")
  # write into list
  ListValidateData[["CombinedDF"]] <- CombinedDF

  # print
  if (!silent) print(CombinedDF)


  ## Validate and Generate:
  #   Validate: the time.series should be complete for two parameters TMK and RSK between start and end date
  #   Generate: generate a combined data.frame which is basis for validation

  # Step: Build start and end date
  StartDate <- paste0(StartYear, "-01-01")
  EndDate <- paste0(EndYear, "-12-31")

  # Step: Generate a complete data.frame with all days between start and end year
  SeriesOfDays <- seq(from = as.Date(StartDate), to = as.Date(EndDate), by = 1)
  print(paste("[Message] Number of days in timeframe:", length(SeriesOfDays)))
  # Write into list
  ListValidateData[["TheoreticalNumberOfEachWeatherStation"]] <- length(SeriesOfDays)
  # Step: Add all Study IDs to prevent missing dates
  StationIDs <- unique(HistoricalWeatherDataFrameToTest$STATIONS_ID)
  print(paste("[Message] Number of Days x Stations:", length(SeriesOfDays) * length(StationIDs)))
  # Step: Convert into data.frame
  SeriesOfDaysWithStationIDs <- merge(SeriesOfDays, StationIDs)
  colnames(SeriesOfDaysWithStationIDs) <- c("Day", "STATIONS_ID")
  print(paste("[Message] The final data.frame should have", nrow(SeriesOfDaysWithStationIDs), "rows"))
  # Write into list
  ListValidateData[["TheoreticalNumberOfValidEntries"]] <- nrow(SeriesOfDaysWithStationIDs)


  ## Generate data.frame to validate
  #  Note: Sometime there is more than one entry for a day --> will aggregate the data first
  # Step: Reduce: only TMK and RSK
  HistWXData <- HistoricalWeatherDataFrameToTest[,c("STATIONS_ID", "MESS_DATUM", "TMK", "RSK")]
  print(paste("[Message] The data.frame with historical DWD weather has", nrow(HistWXData), "entries."))
  nrow(HistWXData)
  # Step: Convert MESS_DATUM into data format
  HistWXData$MESS_DATUM <- as.Date(HistWXData$MESS_DATUM)
  # Step: Aggregate by MESS_DATUM and STATIONS_ID using the arithmetic mean
  HistWXDataAggr <- aggregate.data.frame(x = HistWXData[,c("TMK", "RSK")],
                                         by = list(HistWXData$MESS_DATUM, HistWXData$STATIONS_ID),
                                         mean,
                                         na.rm = TRUE)
  # Rename
  colnames(HistWXDataAggr)[1] <- "MESS_DATUM"
  colnames(HistWXDataAggr)[2] <- "STATIONS_ID"
  # Replace NaN through NA
  HistWXDataAggr$TMK[is.nan(HistWXDataAggr$TMK)] <- NA
  HistWXDataAggr$RSK[is.nan(HistWXDataAggr$RSK)] <- NA
  # Message
  print(paste("[Message] The aggregated DWD historical weather data.frame has", nrow(HistWXDataAggr), "entries"))
  # Step: Merge against data.frame SeriesOfDaysWithStationIDs: LEFT JOIN
  ValidationDF <- merge.data.frame(x = SeriesOfDaysWithStationIDs,
                                   y = HistWXDataAggr,
                                   by.x = c("Day", "STATIONS_ID"),
                                   by.y = c("MESS_DATUM", "STATIONS_ID"),
                                   all.x = TRUE)
  print(paste("The merged data.frame has", nrow(ValidationDF),"entries"))
  # Step: sort by STATIONS_ID und Day
  ValidationDF <- ValidationDF[order(ValidationDF$STATIONS_ID, ValidationDF$Day),]
  # print
  if (!silent) print(head(ValidationDF))

  # Write into list
  ListValidateData[["HistoricalWXDataValidated"]] <- ValidationDF

  ## Number of missing values in time.series
  # Step: Aggregate to get the number of missing values:
  ValidationAggrDF <- aggregate.data.frame(ValidationDF, by = list(ValidationDF$STATIONS_ID),
                                           FUN = function(x) sum(is.na(x)))
  ValidationAggrDF$Day <- NULL # Variable Day not needed any more
  ValidationAggrDF$STATIONS_ID <- NULL # Variable STATIONS_ID not needed any more
  # Change colnames
  colnames(ValidationAggrDF) <- c("STATIONS_ID", "na.TMK", "na.RSK")
  # Change back to original order
  ValidationAggrDF <- ValidationAggrDF[order(match(ValidationAggrDF[[1]], OrderIDsWXStations)), ]
  # write into list
  ListValidateData[["ValidationAggrDF"]] <- ValidationAggrDF
  # print
  if (!silent) print(ValidationAggrDF)

  # table with relative values
  DFWithNAsOfEachWXStation <- data.frame(STATION_ID = ValidationAggrDF$STATIONS_ID,
                                         STATION_ID_POSITION = IDExtractedWeatherStations,
                                         P.na.RSK = round(ValidationAggrDF$na.TMK /
                                                            length(SeriesOfDays) * 100, 1),
                                         P.na.TMK = round(ValidationAggrDF$na.RSK /
                                                            length(SeriesOfDays) * 100, 1))

  # table with evaluation (okay / NAs below threshold / NAs above threshold)
  DFWithNAsOfEachWXStation$Eval.na.RSK[DFWithNAsOfEachWXStation$P.na.RSK == 0] <- "okay"
  DFWithNAsOfEachWXStation$Eval.na.RSK[DFWithNAsOfEachWXStation$P.na.RSK > 0.0 &
                                         DFWithNAsOfEachWXStation$P.na.RSK <= 5.0 ] <- "NAs below threshold"
  DFWithNAsOfEachWXStation$Eval.na.RSK[DFWithNAsOfEachWXStation$P.na.RSK > 5.0] <- "NAs above threshold"

  DFWithNAsOfEachWXStation$Eval.na.TMK[DFWithNAsOfEachWXStation$P.na.TMK == 0] <- "okay"
  DFWithNAsOfEachWXStation$Eval.na.TMK[DFWithNAsOfEachWXStation$P.na.TMK > 0.0 &
                                         DFWithNAsOfEachWXStation$P.na.TMK <= 5.0 ] <- "NAs below threshold"
  DFWithNAsOfEachWXStation$Eval.na.TMK[DFWithNAsOfEachWXStation$P.na.TMK > 5.0] <- "NAs above threshold"

  ## Evaluation: If one entry is NAs above threshold --> get entry number 3 and so forth, until the data.frame is
  #   complete
  #  Extract IDs of the closest identified DWD wweather stations
  DFWithNAsOfEachWXStation$GetNewWeatherStation <-
    DFWithNAsOfEachWXStation$Eval.na.RSK %in% "NAs above threshold" |
    DFWithNAsOfEachWXStation$Eval.na.TMK %in% "NAs above threshold"

  DFWithNAsOfEachWXStation$ImputationRSK <- DFWithNAsOfEachWXStation$Eval.na.RSK %in% "NAs below threshold"
  DFWithNAsOfEachWXStation$ImputationTMK <- DFWithNAsOfEachWXStation$Eval.na.TMK %in% "NAs below threshold"

  # write into list
  ListValidateData[["ValidationAggrDFPercentageValues"]] <- DFWithNAsOfEachWXStation
  # print
  if (!silent) print(DFWithNAsOfEachWXStation)

  # Change back to original order
  DFWithNAsOfEachWXStation <- DFWithNAsOfEachWXStation[order(match(DFWithNAsOfEachWXStation[[1]], OrderIDsWXStations)), ]
  ## return: list
  return(ListValidateData)

}
