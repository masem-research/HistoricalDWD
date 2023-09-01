#' Title
#'
#' @param UseBuildInDWDForecastingMetaData
#'
#' @return
#' @export
#'
#' @examples
#' PropertyData <- data.frame(Proj_key = c(1021, 1378, 1672, 1732, 1862),
#'                            ObjectName = c("Helogland (Reede)", "Wyk auf Föhr (Alte Mole)",
#'                                           "Konstanz (Hafen)", "Greetsiel (Hafen)",
#'                                           "Dornumersiel Hafen"),
#'                            LATITUDE = c(54.179837, 54.692713, 47.659868, 53.50214, 53.68001),
#'                            LONGITUDE = c(7.892211, 8.573618, 9.178899, 7.09950, 7.48596))
#'
#' ForecastDWDWeatherData(UseBuildInDWDForecastingMetaData = FALSE,
#'                        ObjectDataDF = PropertyData,
#'                        VarNameProjKeyInObjectDataDF = "Proj_key",
#'                        VarNameObjectNameInObjectDataDF = "ObjectName",
#'                        VarNameLATInObjectDataDF = "LATITUDE",
#'                        VarNameLONGInObjectDataDF = "LONGITUDE")
ForecastDWDWeatherData <- function(UseBuildInDWDForecastingMetaData = FALSE,
                                   ObjectDataDF,
                                   VarNameProjKeyInObjectDataDF = "Proj_key",
                                   VarNameObjectNameInObjectDataDF = "ObjectName",
                                   VarNameLATInObjectDataDF = "LATITUDE",
                                   VarNameLONGInObjectDataDF = "LONGITUDE",
                                   RadiusAroundObject = 40) {
  # Main function to get DWD weather forecasts

  ## Request DWD Forecasting MetaData
  if (!UseBuildInDWDForecastingMetaData) {
    DWDForecastingMetaData <- GetDWDForecastingMetaData()}
  # TODO: If this fails or if flag UseBuildInDWDForecastingMetaData ios set to TRUE, use the data.frame in package
  #  Copy the current list in data-folder and build a build-in R data.frame with all data

  ## Check the DWD Forecasting Metadata data.frame
  #   Required are
  #   - ID
  #   - NAME
  #   - LAT
  #   - LON
  if (any(!c("ID", "NAME", "LAT", "LON") %in% names(DWDForecastingMetaData))) {
    message("Something went wrong. The required variables are not available. You can try to use the
            build-in list. Set UseBuildInDWDForecastingMetaData to TRUE.")}

  ## Calculate the distance to the objects
  DistancesObjectWXStations <- CalculateDistanceBetwLongAndLat(ObjectDF = ObjectDataDF,
                                                               ObjectLatName = VarNameLATInObjectDataDF,
                                                               ObjectLongName = VarNameLONGInObjectDataDF,
                                                               StationDF = DWDForecastingMetaData,
                                                               StationLatName = "LAT" ,
                                                               StationLongName = "LON",
                                                               RadiusAroundObject = RadiusAroundObject,
                                                               RadiusOfEarth = 6371)

  ## Generate Forecasting URLs
  for (i in 1:nrow(DistancesObjectWXStations)) {
    DistancesObjectWXStations$ForecastingURL[i] <-
      GenerateForcastingURLs(IDWeatherStation = DistancesObjectWXStations$ID[i])[,2]

  } # end for-loop

  ## Download MOSMIXL files
  for (i in 1:nrow(DistancesObjectWXStations)) {
    DistancesObjectWXStations$PathAndFileName[i] <- RequestMOSMIXLData(RequestURLDataFrame = DistancesObjectWXStations[i,],
                                                                       ColumnWeatherID = "ID",
                                                                       ColumnForecastingURL = "ForecastingURL")
  } # end for-loop




  #View(DistancesObjectWXStations)
  # TODO: Implement selection process: which station delivers complete forecasting data?
  return(DistancesObjectWXStations)

}
