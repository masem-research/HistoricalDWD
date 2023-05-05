#' Get historical DWD weather data based on nearby IDs
#'
#' @param VectorWithIDsDWDWeatherStations integer. Vector with the IDs of the DWD weather stations
#'
#' @return data.frame. List with a match of the extracted data.frames of the historical data of each DWD
#'  weather station by ID from a list. The query returns a list of individual data.frames, which are then merged
#'  into a common data.frame.
#' @export
#'
#' @examples
#' # Some weather station IDs
#' VectorIDsSomeWeatherStations <- c("02115", "01963", "02712")
#'
#' # Get the data from the weather stations: historical and recent
#' HistoricalWeatherDataDF <- GetHistoricalDWDWeatherData(VectorWithIDsDWDWeatherStations = VectorIDsSomeWeatherStations)
#' # First six rows
#' head(HistoricalWeatherDataDF)
#'
#' # Checking the most recent entry
#' aggregate(x = HistoricalWeatherDataDF[,"MESS_DATUM", drop = F], by =  list(HistoricalWeatherDataDF$STATIONS_ID), max)
#'
#' # Check the oldest entry
#' aggregate(x = HistoricalWeatherDataDF[,"MESS_DATUM", drop = F], by =  list(HistoricalWeatherDataDF$STATIONS_ID), min)
GetHistoricalDWDWeatherData <- function(VectorWithIDsDWDWeatherStations) {
  # First, the URL for querying the historical weather stations is generated:
  VectorWithURLs <- sapply(X = 1:length(VectorWithIDsDWDWeatherStations), function(x) {
    rdwd::selectDWD(id = VectorWithIDsDWDWeatherStations[x],
                    res = "daily",
                    var = "kl",
                    per = "hr")})
  # Generate List
  DWDWeatherData <- lapply(X = 1:length(VectorWithURLs), function(x) {
    print(VectorWithURLs[x])
    print("\n")
    rdwd::dataDWD(VectorWithURLs[x], dir = tempdir(), progbar = TRUE)
  })

  ## Set name sin list
  #   Since a name should not start with a number, the prefix ID_ is prepended
  VectorWithNames <- paste0("ID_", VectorWithIDsDWDWeatherStations)
  names(DWDWeatherData) <- VectorWithNames

  ## convert into a data.frame
  HistoricalDWDWeatherDataDF <- do.call(rbind, DWDWeatherData)

  ## remove row.names
  row.names(HistoricalDWDWeatherDataDF) <- NULL

  # return
  return(HistoricalDWDWeatherDataDF)
}
