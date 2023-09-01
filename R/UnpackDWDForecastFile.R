#' Unpack DWD Forecasting File - kmz-Format - and return as XML document
#'
#' @param PathToFolderAndFile character. Folder where the kmz file is stored.
#'
#' @return XMLInternalDocument / XMLAbstractDocument. Generated XML document for further processing.
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
#' ExtrahierteForecastXML <- DWDForecastDateiEntpacken(PathToFolderAndFile = PathToFolderAndFile)
#' class(ExtrahierteForecastXML)
UnpackDWDForecastFile <- function(PathToFolderAndFile) {
  ## Unzip kml file in folder
  kml <- utils::unzip(zipfile = PathToFolderAndFile, exdir = tempdir())
  ## Parsing the unzip kml file (XML format) into object doc
  doc <- XML::xmlParse(kml)
  ## return: XML object
  return(doc)
}
