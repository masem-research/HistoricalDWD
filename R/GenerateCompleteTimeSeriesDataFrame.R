#' Generates a complete time series data frame between start and end year
#'
#' @details Checks if the data.frame with historical weather data from DWD is complete due to all dates between
#' start and end date. If in the data.frame dates are missing, they will be added. Corresponding values of `RSK` and
#' `TMK` will be set to NA for these missing dates. NAs can be imputet in an extra step.
#'
#' @param StartYear integer. Start year.
#' @param EndYear integer. End year.
#' @param HistDataList list. List with data.frames for every DWD historical weather station.
#'
#' @return list. List with DWD historical weather station with a complete set of dates between startdate and enddate.
#' @export
#'
#' @examples
#' GenerateCompleteTimeSeriesDataFrame(StartYear = StartYear, EndYear = EndYear,
#'                                     HistDataList = ListWithResults$HistoricalWeatherDataList)
GenerateCompleteTimeSeriesDataFrame <- function(StartYear = StartYear, EndYear = EndYear,
                                                HistDataList = ListWithResults$HistoricalWeatherDataList) {
  # Check the data
  print("[Message] Prüfung auf Dubletten:")
  print(lapply(X = HistDataList, FUN = function(x) {table(duplicated(x = x[c("STATIONS_ID", "MESS_DATUM")]))}))
  # Aggregate
  HistDataList <- lapply(X = HistDataList, FUN = function(x) {
    aggregate.data.frame(x = x[,c("RSK", "TMK")], by = list(x$STATIONS_ID, x$MESS_DATUM), FUN = mean, na.rm = TRUE)})
  # Check number of entries
  print("Number of rows in each time.series.data.frame:")
  print(sapply(X = HistDataList, FUN = nrow))

  # Create complete time.series.data.frame by StationID
  EmptyTimeSeriesDF <- CreateEmptyTimeSeriesDF(StartYear = StartYear, EndYear = EndYear, StationIDs = names(HistDataList))
  # Convert list into data.frame
  HistoricalWeatherDataDf <- do.call(rbind, HistDataList)
  # delete row.names
  row.names(HistoricalWeatherDataDf) <- NULL
  # set first two names in data.frame
  names(HistoricalWeatherDataDf)[c(1,2)] <-  c("STATIONS_ID", "Day")
  # Set date-variable to Date
  HistoricalWeatherDataDf$Day <- as.Date(HistoricalWeatherDataDf$Day)

  # Merge DWD time series data.frames against the time series data.frame
  HistoricalWeatherDataComplete <- merge.data.frame(x = EmptyTimeSeriesDF, y = HistoricalWeatherDataDf, by = c("STATIONS_ID", "Day"), all.x = TRUE)
  print("Check for complete data.sets, should be number of years times approx. 365 days/year (on average 365.25 days):")
  print(tapply(X = HistoricalWeatherDataComplete$Day, INDEX = HistoricalWeatherDataComplete$STATIONS_ID, FUN = length))
  # sort the data.frame
  # HistoricalWeatherDataComplete[order(c(HistoricalWeatherDataComplete$STATIONS_ID, HistoricalWeatherDataComplete$Day)),]
  # Convert into list
  HistoricalWeatherDataCompleteList <- split(x = HistoricalWeatherDataComplete, f = HistoricalWeatherDataComplete$STATIONS_ID)

  # Check for missing values
  print(sapply(X = HistoricalWeatherDataCompleteList, FUN = function(df) {
    c(NA.TMK = sum(is.na(df$TMK)), NA.RSK = sum(is.na(df$RSK)))}))

  # return
  return(HistoricalWeatherDataCompleteList)
  }
