#' Extract DWD forecasting station data from extracted kml file
#'
#' @param DWDForecastXMLDatei XMLInternalDocument / XMLAbstractDocument. Extracted XML document from DWD kmz file.
#'
#' @return SpatialPointsDataFrame. Objekt with `coordinates`, `name`, `desc` and `alt`
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
#' ExtractDWDForecastStationDataFromKML(ExtractedDWDKMLFile = ExtractedForecastKMLObject)
ExtractDWDForecastStationDataFromKML <- function(ExtractedDWDKMLFile){
  ## Check if the file is an XML object
  stopifnot(inherits(x = ExtractedDWDKMLFile, what = c("XMLInternalDocument", "XMLAbstractDocument")))
  ## Extract station data
  station <- mosmix::get_station_information(ExtractedDWDKMLFile)
  ## Zurückgeben: Stationsnamen
  return(station)
}
