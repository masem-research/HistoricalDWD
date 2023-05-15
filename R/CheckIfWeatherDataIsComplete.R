#' Checks if a DWD historical weather data.frame has complete data in between a given start and end date
#'
#' @details
#' Checks whole years and if data is available on every day; Start year starts at 1st of January, end of year
#' 31st of December
#'
#' @return
#' @export
#'
#' @examples
CheckIfWeatherDataIsComplete <- function(StartYear = 2022, EndYear = 2022) {

  # Build start and end date
  StartDate <- paste0(StartYear, "-01-01")
  EndDate <- paste0(EndYear, "-12-31")


  # Generate a complete data.frame with all days between start and end year
  SeriesOfDays <- seq(from = as.Date(StartDate), to = as.Date(EndDate), by = 1)
  # Reference value stores the number of days between start and end year
  #  Can be compared to real data
  ReferenceValue <- length(SeriesOfDays)

  # return

}
