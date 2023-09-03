#' Extract data from kml forecasting file
#'
#' @param DWDForecastXMLDatei XMLInternalDocument / XMLAbstractDocument. Extracted XML document from DWD kmz file.
#'
#' @return character. Variable names of the parameters to extract.
#' @export
#'
#' @examples
#' ## Station K057
#' TestRequestDF <-
#'   data.frame(ID = "K057",
#'              URL = "https://opendata.dwd.de/weather/local_forecasts/mos/MOSMIX_L/single_stations/K057/kml/MOSMIX_L_LATEST_K057.kmz")
#' # Request the DWD forecasting weather data
#' PathToFolderAndFile <- RequestMOSMIXLData(RequestURLDataFrame = TestRequestDF, ColumnWeatherID = "ID", ColumnForecastingURL = "URL")
#' # Extract XML
#' ExtractedForecastKMLObject <- UnpackDWDForecastFile(PathToFolderAndFile = PathToFolderAndFile)
#' class(ExtractedForecastKMLObject)
#' # Extract station data
#' ExtractedForecastData <- ExtractDWDForecastStationDataFromKML(ExtractedDWDKMLFile = ExtractedForecastKMLObject)
#' # Stationsdaten extrahieren
#' head(ExtractDataFromForecastingKml(ExtractedDWDKMLFile = ExtractedForecastKMLObject))
ExtractDataFromForecastingKml <- function(ExtractedDWDKMLFile,
                                                 ParametersToExtract = c("RR1c", "ww", "T5cm",
                                                                         "TTT", "TN", "TX", "FF",
                                                                         "SunD", "N")){
  ## Check if the file is an XML object
  stopifnot(inherits(x = ExtractedDWDKMLFile, what = c("XMLInternalDocument", "XMLAbstractDocument")))
  ## Extract variable names
  #   Create empty list
  ExtractedInformationList <- list()
  #   Extract data
  ExtractedInformationList$datetime <- mosmix::get_datetime(ExtractedDWDKMLFile)
  ExtractedInformationList$meta     <- mosmix::get_meta_info(ExtractedDWDKMLFile)
  ExtractedInformationList$stations <- mosmix::get_station_information(ExtractedDWDKMLFile)
  ## Extract selected parameter and write it to the list
  fcst1 <- mosmix::get_forecasts(station = paste0("'",ExtractedInformationList$stations$name,"'"),
                                 doc = ExtractedDWDKMLFile,
                                 datetime = ExtractedInformationList$datetime,
                                 meta = ExtractedInformationList$meta,
                                 as.zoo = FALSE,
                                 parameter = ParametersToExtract)
  ## Return extracted names - Attention: Is always a subset of the parameters
  #   Strictly speaking, it should be checked in the XML object which parameters are actually present
  return(as.data.frame(fcst1))
}
