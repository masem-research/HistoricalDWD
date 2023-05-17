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
                                         silent = TRUE) {

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

  ## Validate: the time.series should be complete for two parameters TMK and RSK between start and end date

  # Build start and end date
  StartDate <- paste0(StartYear, "-01-01")
  EndDate <- paste0(EndYear, "-12-31")

  # Generate a complete data.frame with all days between start and end year
  SeriesOfDays <- seq(from = as.Date(StartDate), to = as.Date(EndDate), by = 1)
  # Convert into data.frame
  SeriesOfDays <- data.frame(SeriesOfDays)
  colnames(SeriesOfDays) <- "Day"

  # Validate against HistoricalWeatherDataFrameToTest
  HistWXData <- HistoricalWeatherDataFrameToTest[,c("STATIONS_ID", "MESS_DATUM", "TMK", "RSK")]
  HistWXData$MESS_DATUM <- as.Date(HistWXData$MESS_DATUM)

  # Mergen gegen Referenzdaten: LEFT JOIN
  ValidationDF <- merge.data.frame(x = SeriesOfDays, y = HistWXData, by.x = "Day", by.y = "MESS_DATUM")
  # Return this data.frame as a corrected data.frame
  # write into list
  ListValidateData[["HistoricalWXDataValidated"]] <- ValidationAggrDF

  # Aggregate to validate:
  ValidationAggrDF <- aggregate.data.frame(ValidationDF, by = list(ValidationDF$STATIONS_ID),
                                           FUN = function(x) sum(is.na(x)))
  ValidationAggrDF$Day <- NULL # Variable Day not needed any more
  ValidationAggrDF$STATIONS_ID <- NULL # Variable STATIONS_ID not needed any more
  # Change colnames
  colnames(ValidationAggrDF) <- c("STATIONS_ID", "na.TMK", "na.RSK")
  # write into list
  ListValidateData[["ValidationAggrDF"]] <- ValidationAggrDF
  # print
  if (!silent) print(ValidationAggrDF)

  #browser()

  # check theoretical number of valid entries: SeriesOfDays * TMK and RSK (multiply by 2)
  TheoreticalNumberOfValidEntries <- nrow(SeriesOfDays) * 2
  # Write into list
  ListValidateData[["TheoreticalNumberOfValidEntries"]] <- TheoreticalNumberOfValidEntries

  ## return: list
  return(ListValidateData)

}
