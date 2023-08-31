#' Create an Empty Time Series data.frame between Start- and Endyear for each station
#'
#' @param StartYear integer. Should be the start year. Start year starts at 1st of January. default = `2019`
#' @param EndYear integer. Should be the end year. End year ends at 31st of December. default = `2022`
#' @param StationIDs character. Character vector with all DWD historical weather data station ids.
#'
#' @return data.frame. data.frame with the complete time series for each station id.
#' @export
#'
#' @examples
#' CreateEmptyTimeSeriesDF(StartYear = 2019, EndYear = 2022, StationIDs = c("05640", "05839"))
CreateEmptyTimeSeriesDF <- function(StartYear = 2019, EndYear = 2022, StationIDs) {
  # Step: Build start and end date
  StartDate <- paste0(StartYear, "-01-01")
  EndDate <- paste0(EndYear, "-12-31")

  # Step: Generate a complete data.frame with all days between start and end year
  SeriesOfDays <- seq(from = as.Date(StartDate), to = as.Date(EndDate), by = 1)
  print(paste("[Message] Number of days in timeframe:", length(SeriesOfDays)))

  # Step: Add all Study IDs to prevent missing dates using merge function
  SeriesOfDaysWithStationIDs <- merge(SeriesOfDays, StationIDs)
  print(paste("[Message] Number of days in data.frame with Station ID:", nrow(SeriesOfDaysWithStationIDs)))

  # Step: Add column names into data.frame
  colnames(SeriesOfDaysWithStationIDs) <- c("Day", "STATIONS_ID")

  # Return
  return(SeriesOfDaysWithStationIDs)

}
