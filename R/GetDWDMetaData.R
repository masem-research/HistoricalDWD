#' Obtaining the metadata of the DWD weather stations
#'
#' @param OnlyCurrentDate bool. Should only weather stations be displayed that provide weather data until
#'  today? default: `FALSE`
#'
#' @return data.frame. DWD weather stations metadata.
#' @export
#'
#' @examples
#'
#' # Obtain all weather stations
#' DWDHistoricalMetaDataAllStations <- GetDWDMetaData(OnlyCurrentDate = FALSE)
#' head(DWDHistoricalMetaDataAllStations)
#' str(DWDHistoricalMetaDataAllStations)
#'
#' # Obtain only weather stations that provide data until yesterday
#' DWDHistoricalMetaData <- GetDWDMetaData(OnlyCurrentDate = TRUE)
#' head(DWDHistoricalMetaData)
#' str(DWDHistoricalMetaData)
GetDWDMetaData <- function(OnlyCurrentDate = FALSE) {
  ## Table with metadata is on the DWD opendata server
  # Link: https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/daily/kl/historical/KL_Tageswerte_Beschreibung_Stationen.txt
  LinkToDWDMetaData <-
    "https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/daily/kl/historical/KL_Tageswerte_Beschreibung_Stationen.txt"

  ## Download txt-file
  # Please note: File is in fixed format
  #  header is first set to FALSE and then added manually to avoid any import problems
  DWDMetaData <- utils::read.fwf(file = LinkToDWDMetaData,
                                 widths = c(6, 8, 19, 8, 10, 9, 40, 24),
                                 header = FALSE, skip = 2, fileEncoding = "ascii")
  ## Set German colnames
  colnames(DWDMetaData) <- c("Stations_id", "von_datum", "bis_datum", "Stationshoehe",
                             "geoBreite", "geoLaenge", "Stationsname", "Bundesland")
  ## Adjust date variables
  DWDMetaData$von_datum <- strptime(x = DWDMetaData$von_datum, format = "%Y%m%d")
  DWDMetaData$bis_datum <- strptime(x = DWDMetaData$bis_datum, format = "%Y%m%d")
  # Adjust StationsID
  DWDMetaData$Stations_id <-
    base::formatC(DWDMetaData$Stations_id, width = 5, format = "d", flag = "0")
  # Adjust station name
  DWDMetaData$Stationsname <- base::trimws(x = DWDMetaData$Stationsname, which = "both")
  DWDMetaData$Bundesland <- base::trimws(x = DWDMetaData$Bundesland, which = "both")

  ## Return only records up to the current date?
  if (OnlyCurrentDate) {
    DWDMetaData <- DWDMetaData[as.Date(DWDMetaData$bis_datum) == Sys.Date() - 1 |
                                 as.Date(DWDMetaData$bis_datum) == Sys.Date() - 2, ]
  }

  ## Return data.frame with DWD station metadata
  return(DWDMetaData)
}
