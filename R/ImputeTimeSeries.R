#' [Deprecated] Impute time series with NA values to get complete data
#'
#' @param WXValidationDF
#' @param TimeSeriesDF
#'
#' @return data.frame. complete time.series data.frame.
#' @export
#'
#' @examples
ImputeTimeSeries <- function(WXValidationDF = ListWithResults[["WXValidationDF"]]$ValidationAggrDFPercentageValues,
                             TimeSeriesDF = ListWithResults[["HistoricalWeatherDataDFReduced"]]) {
  # Input is the overview validation table + data.frame with time series data
  # Identify, which time series should be imputet
  TimeSeriesToBeImputed_RSK <- WXValidationDF$STATION_ID[WXValidationDF$ImputationRSK == TRUE]
  print(paste("RSK imputation for stations:", TimeSeriesToBeImputed_RSK))
  TimeSeriesToBeImputed_TMK <- WXValidationDF$STATION_ID[WXValidationDF$ImputationTMK == TRUE]
  print(paste("TMK imputation for stations:", TimeSeriesToBeImputed_TMK))
  # Impute RSK
  for (i in 1:length(TimeSeriesToBeImputed_RSK)) {
    TimeSeriesDF[TimeSeriesDF$STATIONS_ID == TimeSeriesToBeImputed_RSK[i], "RSK"] <-
      imputeTS::na_kalman(TimeSeriesDF[TimeSeriesDF$STATIONS_ID == TimeSeriesToBeImputed_RSK[i], "RSK"])
    #plot(imputeTS::na_kalman(TimeSeriesDF[TimeSeriesDF$STATIONS_ID == TimeSeriesToBeImputed_RSK[i], "RSK"]), type = "l")
  }
  # Impute TMK
  for (i in 1:length(TimeSeriesToBeImputed_TMK)) {
    TimeSeriesDF[TimeSeriesDF$STATIONS_ID == TimeSeriesToBeImputed_TMK[i], "TMK"] <-
      imputeTS::na_kalman(TimeSeriesDF[TimeSeriesDF$STATIONS_ID == TimeSeriesToBeImputed_TMK[i], "TMK"])
    #plot(imputeTS::na_kalman(TimeSeriesDF[TimeSeriesDF$STATIONS_ID == TimeSeriesToBeImputed_TMK[i], "TMK"]), type = "l")
  }
  return(TimeSeriesDF)
}
