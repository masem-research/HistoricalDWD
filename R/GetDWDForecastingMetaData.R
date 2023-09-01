#' Obtaining the metadata of the DWD Forecasting (MOSMIXL) weather stations
#'
#' @description The raw data are obtained from an opendata server of the DWD. The obtained data are in fixed format.
#'
#' @return data.frame. DWD Forecasting weather stations metadata.
#' @export
#'
#' @examples
#' DWDForecastingMetaData <- GetDWDForecastingMetaData()
GetDWDForecastingMetaData <- function() {
  ## Pull table with metadata from DWD opendata server
  # Link: https://www.dwd.de/DE/leistungen/met_verfahren_mosmix/mosmix_stationskatalog.cfg;jsessionid=C902F00ACCB423075BF87DB22E384568.live11042?view=nasPublication&nn=16102
  LinkToDWDForecastingMetaData <-
    "https://www.dwd.de/DE/leistungen/met_verfahren_mosmix/mosmix_stationskatalog.cfg;jsessionid=C902F00ACCB423075BF87DB22E384568.live11042?view=nasPublication&nn=16102"
  ## Download txt-file
  # Please note: File is in fixed format
  #  header is first set to FALSE and then added manually due to import problems
  DWDMetaForecastData <- utils::read.fwf(file = LinkToDWDForecastingMetaData,
                                         widths = c(6, 5, 21, 7, 8, 5),
                                         header = FALSE,
                                         skip = 2,
                                         fileEncoding = "ascii")
  ## Set variable names
  colnames(DWDMetaForecastData) <- c("ID", "ICAO", "NAME", "LAT", "LON", "ELEV")
  ## Clean data.frame
  #  1: Delete trailing and leading white spaces in station IDs
  DWDMetaForecastData$ID <- trimws(DWDMetaForecastData$ID)
  #  2: Delete trailing and leading white spaces in station names
  DWDMetaForecastData$NAME <- trimws(DWDMetaForecastData$NAME)

  # Print first six rows of data
  print("First six rows of metadata forecasting stations:")
  print(head(DWDMetaForecastData))

  ## modify variables
  # set StationsID to five signs using formatC-function
  #DWDMetaForecastData$id <-
  #  base::formatC(DWDMetaForecastData$id, width = 5, format = "d", flag = "0")

  ## return data.frame
  return(DWDMetaForecastData)
}
