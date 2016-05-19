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
  expect_is(waterYear$dateTime, 'POSIXct')
  
  siteInfo <- readNWISdata(stateCd="WI", parameterCd="00010",hasDataTypeCd="iv", 
                           service="site")
  expect_is(siteInfo$station_nm, "character")
  
  qwData <- readNWISdata(bBox=c(-82.5,41.52,-81,41),startDate=as.Date("2000-01-01"),
                   drain_area_va_min=50, qw_count_nu=50,qw_attributes="expanded",
                    qw_sample_wide="wide",list_of_search_criteria=c("lat_long_bounding_box",
                    "drain_area_va","obs_count_nu"),service="qw")
  expect_is(qwData$startDateTime, "POSIXct")
  
  url <- "http://waterservices.usgs.gov/nwis/dv/?site=09037500&format=rdb&ParameterCd=00060&StatCd=00003&startDT=1985-10-02&endDT=2012-09-06"
  dv <- importRDB1(url, asDateTime = FALSE)
  
  urlEmpty <- "http://nwis.waterdata.usgs.gov/nwis/qwdata?multiple_site_no=413437087150601&sort_key=site_no&group_key=NONE&inventory_output=0&begin_date=&end_date=&TZoutput=0&param_group=NUT,INN&qw_attributes=0&format=rdb&qw_sample_wide=0&rdb_qw_attributes=expanded&date_format=YYYY-MM-DD&rdb_compression=value&list_of_search_criteria=multiple_site_no"
  dv <- importRDB1(urlEmpty, asDateTime = FALSE)
  expect_that(nrow(dv) == 0, is_true())
})


test_that("General WQP retrievals working", {
  testthat::skip_on_cran()
  # testthat::skip_on_cran()
  # Bring back when WQP is back
  nameToUse <- "pH"
  pHData <- readWQPdata(siteid="USGS-04024315",characteristicName=nameToUse)
  expect_is(pHData$ActivityStartDateTime, 'POSIXct')
  
  # Known slow query for WQP:
#   pHDataExpanded2 <- readWQPdata(bBox=c(-90.1,42.9,-89.9,43.1),
#                                  characteristicName=nameToUse)
#   expect_is(pHDataExpanded2$ActivityStartDateTime, 'POSIXct')
  
  startDate <- as.Date("2013-01-01")
  nutrientDaneCounty <- readWQPdata(countycode="US:55:025",startDate=startDate,
                         characteristicType="Nutrient")
  expect_is(nutrientDaneCounty$ActivityStartDateTime, 'POSIXct')
})

test_that("WQP head query retrieval working", {
  testthat::skip_on_cran()
  # testthat::skip_on_cran()
  # Bring back when WQP is back
  nameToUse <- "pH"
  pHDataQueryResults <- readWQPdata(siteid="USGS-04024315",
                                    characteristicName=nameToUse, 
                                    querySummary=TRUE)
  expect_false(is.null(pHDataQueryResults$`total-site-count`))
  expect_is(pHDataQueryResults$`total-site-count`, 'character')
  expect_false(is.null(pHDataQueryResults$`total-result-count`))
  expect_is(pHDataQueryResults$`total-result-count`, 'character')
  
})
