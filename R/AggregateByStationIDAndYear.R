#' Aggregate the complete time.series by STATIONS_ID and Day/Month
#'
#' @param DFToAggregate
#' @param AggregationFunction character. Aggregation function. Should be the arithmetic mean, median etc..
#' default: `mean`
#'
#' @return data.frame. Aggregated `TMK` and `RSK` values by STATIONS_ID and Day/Month
#' @export
#'
#' @examples
AggregateByStationIDAndYear <- function(DFToAggregate,
                                        AggregationFunction = "mean") {
  # Step 1: Extract Date without Year
  DFToAggregate$DayMonth <- format(x = DFToAggregate$MESS_DATUM, format = "%d-%m")
  # Step 2: Aggregate by STATIONS_ID and DayMonth
  DFAggregated <- aggregate.data.frame(x = DFToAggregate[,c("RSK", "TMK")],
                                       by = list(DFToAggregate$STATIONS_ID, DFToAggregate$DayMonth),
                                       FUN = AggregationFunction)
  # Step 3: set colnames
  colnames(DFAggregated) <- c("STATIONS_ID", "DayMonth", "RSKmean", "TMKmean")
  # Step Validate: Number of cases of each STATIONS_ID
  print(aggregate.data.frame(x = DFAggregated[,c("RSKmean","TMKmean")],
                             by = list(DFAggregated$STATIONS_ID),
                             FUN = length))
  # Step 4: Sort
  DFAggregated <- DFAggregated[order(DFAggregated$STATIONS_ID, DFAggregated$DayMonth),]
  # Step 5. Resest row.numbers
  rownames(DFAggregated) <- NULL
  # Step: Validate: Print first six entries of aggregated data.frame
  print("[Message] Aggregated data.frame: First six entries:")
  print(head(DFAggregated))

  # return aggregated data.frame
  return(DFAggregated)

}
