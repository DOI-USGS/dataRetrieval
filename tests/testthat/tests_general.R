context("General functions")

test_that("General NWIS retrievals working", {
  testthat::skip_on_cran()
  
  multiSite <- readNWISdata(sites=c("04025500","040263491"), service="iv", 
                            parameterCd="00060")
  expect_is(multiSite$dateTime, 'POSIXct')
  
  bBoxEx <- readNWISdata(bBox=c(-83,36.5,-81,38.5), parameterCd="00010")
  expect_that(length(unique(bBoxEx$site_no)) > 1, is_true())
  
  startDate <- as.Date("2013-10-01")
  endDate <- as.Date("2014-09-30")
  waterYear <- readNWISdata(bBox=c(-83,36.5,-81,38.5), parameterCd="00010", 
                   service="dv", startDate=startDate, endDate=endDate)
  expect_is(waterYear$Date, 'Date')
  
  siteInfo <- readNWISdata(stateCd="WI", parameterCd="00010",hasDataTypeCd="iv", 
                           service="site")
  
})