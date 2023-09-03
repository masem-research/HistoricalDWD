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

  ## Unpack DWD Forecasting File - kmz-Format - and return as XML document
  #   store into a list
  DWDForecastingXMLFiles <- list()
  for (i in 1:nrow(DistancesObjectWXStations)) {
    DWDForecastingXMLFiles[[i]] <- UnpackDWDForecastFile(PathToFolderAndFile = DistancesObjectWXStations$PathAndFileName[i])
  }
  # set names to ID
  names(DWDForecastingXMLFiles) <- DistancesObjectWXStations$ID


  ## Unpack DWD forecasting station metadata
  DWDForecastingMetaDataFromKMLObject <- list()
  for (i in 1:nrow(DistancesObjectWXStations)) {
    DWDForecastingMetaDataFromKMLObject[[i]] <- ExtractDWDForecastStationDataFromKML(ExtractedDWDKMLFile = DWDForecastingXMLFiles[[i]])
  }
  # build data.frame
  DWDForecastingMetaDataFromKMLObject <- as.data.frame(do.call(rbind, DWDForecastingMetaDataFromKMLObject))
  #str(DWDForecastingMetaDataFromKMLObject)
  ## Validate station list
  if (any(!DWDForecastingMetaDataFromKMLObject$name == DistancesObjectWXStations$ID)) {
    message("Some of the requested stations are not available. Please check the data.")
  }

  ## Extract data from kml file
  DWDForecastingDatList <- list()
  for (i in 1:length(DWDForecastingXMLFiles)) {
    DWDForecastingDatList[[i]] <- ExtractDataFromForecastingKml(ExtractedDWDKMLFile = DWDForecastingXMLFiles[[i]])
  }

  # Build function: CheckIfForecastingDataIsAvailable()
  DistancesObjectWXStations <- CheckIfForecastingDataIsAvailable(DWDForecastingDatList = DWDForecastingDatList,
                                                                 DWDForecastingMetaDataFromKMLObject = DWDForecastingMetaDataFromKMLObject,
                                                                 DistancesObjectWXStations = DistancesObjectWXStations)

  ## Add a flag if certain parameters are available
  #   TTT
  #   RR1c
  # Number of entries must be 247 for the whole 10 days
  DistancesObjectWXStations$TTT_RR1c_complete <- FALSE
  DistancesObjectWXStations$TTT_RR1c_complete[DistancesObjectWXStations$TTT == 247 & DistancesObjectWXStations$RR1c == 247] <- TRUE

  ## Generate the final list with selected stations
  #   Sort dataset by Proj_Key and distance
  DistancesObjectWXStations <- DistancesObjectWXStations[order(DistancesObjectWXStations$Proj_key, DistancesObjectWXStations$DistanceKM),]
  #   Select only TTT_RR1c_complete == TRUE entries
  DistancesObjectWXStationsWithWXParameter <- DistancesObjectWXStations[DistancesObjectWXStations$TTT_RR1c_complete == TRUE,]
  #   Add a group specific id by Proj_key and DistanceKM
  DistancesObjectWXStationsWithWXParameter$GroupID <- ave(DistancesObjectWXStationsWithWXParameter$DistanceKM,
                                                          DistancesObjectWXStationsWithWXParameter$Proj_key, FUN = seq_along)
  # Select the relvant entry: 1
  DistancesObjectWXStationsWithWXParameterFinalList <- DistancesObjectWXStationsWithWXParameter[DistancesObjectWXStationsWithWXParameter$GroupID == 1,]

  # return the final list
  return(DistancesObjectWXStationsWithWXParameterFinalList)

}
