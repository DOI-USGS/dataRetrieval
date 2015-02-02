context("Unit values")

test_that("Unit value data returns correct types", {
  testthat::skip_on_cran()
  
  siteNumber <- '05114000'
  parameterCd <- '00060'
  startDate <- "2014-10-10"
  endDate <- "2014-10-10"

  rawData <- readNWISuv(siteNumber,parameterCd,startDate,endDate)
  
  timeZoneChange <- readNWISuv(c('04024430','04024000'),parameterCd,
                               "2013-11-03","2013-11-03")

  expect_is(rawData$dateTime, 'POSIXct')
  
  
})