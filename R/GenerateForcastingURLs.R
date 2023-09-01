#' Creates a URL to query the DWD Forecasting weather data based on the ID of the weather station
#'
#' @param IDWeatherStation character. Weather station ID as character.
#'
#' @return data.frame. ID of the weather station with URL.
#' @export
#'
#' @examples
#' GenerateForcastingURLs(IDWeatherStation = "E273")
#' GenerateForcastingURLs(IDWeatherStation = "E355")
GenerateForcastingURLs <- function(IDWeatherStation) {
  # Generates an URL like: "https://opendata.dwd.de/weather/local_forecasts/mos/MOSMIX_L/single_stations/01001/kml/MOSMIX_L_LATEST_01001.kmz"
  message(paste("\nCreate query URL for the weather station ID", IDWeatherStation))
  # URL textstring
  urlTextString <- paste0("https://opendata.dwd.de/weather/local_forecasts/mos/MOSMIX_L/single_stations/",
                          IDWeatherStation , "/kml/MOSMIX_L_LATEST_")
  # URL erzeugen
  QueryURL <- paste0(urlTextString, IDWeatherStation, ".kmz")
  # Message
  message(paste("\nURL generated with ID of the weather station:", QueryURL))
  # data.frame erzeugen: Abfrage URL und ID
  RequestURLIDdf <- data.frame(IDWeatherStation = IDWeatherStation, URL = QueryURL)
  # zurückgegen
  return(RequestURLIDdf)
}
