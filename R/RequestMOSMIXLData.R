#' Download the forecast weather data of the DWD
#'
#' @param RequestURLDataFrame data.frame. data.frame with the query URL and the DWD weather station ID
#' @param ColumnWeatherID character. DWD forecasting weatherstation ID
#' @param ColumnForecastingURL character. URL request.
#'
#' @return character. Returns the path and folder of the download.
#' @export
#'
#' @examples
#' TestRequestDF = data.frame(
#'   ID = "11120",
#'   ForecastingURL = "https://opendata.dwd.de/weather/local_forecasts/mos/MOSMIX_L/single_stations/11120/kml/MOSMIX_L_LATEST_11120.kmz")
#' TestRequestDF
#' # Request DWD forecast weather data
#' RequestMOSMIXLData(RequestURLDataFrame = TestRequestDF)
RequestMOSMIXLData <- function(RequestURLDataFrame, ColumnWeatherID = "ID", ColumnForecastingURL = "ForecastingURL") {
  # Latest L-type DWD MOSMIX forecast file
  # MOSMIX-L: produced 4 times a day up to 240 hours ahead, provides
  # up to 115 parameters (location dependent).

  ## Create path to folder with file name
  #   File(s) saved in tempdir()
  #   Please note, however, that this does not save the files permanently, since the tempdir will be reset after
  #    the termination of the current R session.
  FileName <- paste0(RequestURLDataFrame[,ColumnWeatherID], ".kmz")
  PathToFolderAndFileName <- paste0(tempdir(), "\\", FileName)
  message(paste("The forcast file is saved in:", PathToFolderAndFileName))
  ## Download the file to the temp directory
  utils::download.file(url = RequestURLDataFrame[,ColumnForecastingURL],
                       destfile = PathToFolderAndFileName,
                       method = "curl")
  ## Return path and filename on success
  if (any(FileName %in%  dir(tempdir()))) {
    message(paste("File", FileName, "is successfully saved in the folder ", PathToFolderAndFileName))
    return(PathToFolderAndFileName)
  }
  ## If not successful, return NA
  message("File could not be downloaded, return NA")
  return(NA)
}
