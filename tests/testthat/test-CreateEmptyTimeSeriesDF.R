test_that("Create an empty time series data.frame between a start and enddate for eeach station id", {
  # one station, one year: 365 days --> 365 rows in generated data.frame
  expect_equal(nrow(CreateEmptyTimeSeriesDF(StartYear = 2022, EndYear = 2022, StationIDs = c("05640"))),
               365)
  # two stations, one year: 365 times 2 rows expected: 730 rows
  expect_equal(nrow(CreateEmptyTimeSeriesDF(StartYear = 2021, EndYear = 2021, StationIDs = c("05640", "05839"))),
               365*2)
  # two stations, two years: 365 days times 2 years times 2 stations = 1,460 rows expected
  expect_equal(nrow(CreateEmptyTimeSeriesDF(StartYear = 2021, EndYear = 2022, StationIDs = c("05640", "05839"))),
               365*2*2)
  })
