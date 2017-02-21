context("General functions")

test_that("General NWIS retrievals working", {
  testthat::skip_on_cran()
  
  multiSite <- readNWISdata(sites=c("04025500","040263491"), service="iv", 
                            parameterCd="00060")
  expect_is(multiSite$dateTime, 'POSIXct')
  # saveRDS(multiSite, "rds/multiSite.rds")
  
  bBoxEx <- readNWISdata(bBox=c(-83,36.5,-81,38.5), parameterCd="00010")
  expect_that(length(unique(bBoxEx$site_no)) > 1, is_true())
  # saveRDS(bBoxEx, "rds/bBoxEx.rds")
  
  startDate <- as.Date("2013-10-01")
  endDate <- as.Date("2014-09-30")
  waterYear <- readNWISdata(bBox=c(-83,36.5,-81,38.5), parameterCd="00010", 
                   service="dv", startDate=startDate, endDate=endDate)
  # saveRDS(waterYear, "rds/waterYear.rds")
  expect_is(waterYear$dateTime, 'POSIXct')
  
  siteInfo <- readNWISdata(stateCd="WI", parameterCd="00010",hasDataTypeCd="iv", 
                           service="site")
  # saveRDS(siteInfo,"rds/siteInfo.rds")
  expect_is(siteInfo$station_nm, "character")
  
  qwData <- readNWISdata(bBox=c(-82.5,41.52,-81,41),startDate=as.Date("2000-01-01"),
                   drain_area_va_min=50, qw_count_nu=50,qw_attributes="expanded",
                    qw_sample_wide="wide",list_of_search_criteria=c("lat_long_bounding_box",
                    "drain_area_va","obs_count_nu"),service="qw")
  # saveRDS(qwData, "rds/qwData.rds")
  expect_is(qwData$startDateTime, "POSIXct")
  
  url <- "https://waterservices.usgs.gov/nwis/dv/?site=09037500&format=rdb&ParameterCd=00060&StatCd=00003&startDT=1985-10-02&endDT=2012-09-06"
  dv <- importRDB1(url, asDateTime = FALSE)
  
  urlEmpty <- "https://nwis.waterdata.usgs.gov/nwis/qwdata?multiple_site_no=413437087150601&sort_key=site_no&group_key=NONE&inventory_output=0&begin_date=&end_date=&TZoutput=0&param_group=NUT,INN&qw_attributes=0&format=rdb&qw_sample_wide=0&rdb_qw_attributes=expanded&date_format=YYYY-MM-DD&rdb_compression=value&list_of_search_criteria=multiple_site_no"
  dv <- importRDB1(urlEmpty, asDateTime = FALSE)
  # saveRDS(dv, "rds/emptyDV.rds")
  expect_that(nrow(dv) == 0, is_true())
  
  dailyStat <- readNWISdata(site=c("03112500","03111520"),service="stat",statReportType="daily",
                           statType=c("p25","p50","p75","min","max"),parameterCd="00065",convertType=FALSE)
  # saveRDS(dailyStat,"rds/dailyStat.rds")
  expect_that(length(dailyStat$min_va) > 1, is_true())
  expect_is(dailyStat$p25_va,"character")
  
  waterYearStat <- readNWISdata(site=c("03112500"),service="stat",statReportType="annual",
                                statYearType="water", missingData="on")
  # saveRDS(waterYearStat, "rds/waterYearStat.rds")
  expect_is(waterYearStat$mean_va,"numeric")
  expect_is(waterYearStat$parameter_cd,"character")
  
  #2 data descriptors, but some random empty "values" tag:
  urlTest <- "https://nwis.waterservices.usgs.gov/nwis/iv/?site=11447650&format=waterml,1.1&ParameterCd=63680&startDT=2016-12-13&endDT=2016-12-13"
  x <- importWaterML1(urlTest)
  expect_equal(ncol(x), 8)
  
  #Test list:
  args <- list(sites="05114000", service="iv", 
               parameterCd="00060", 
               startDate="2014-05-01T00:00Z",
               endDate="2014-05-01T12:00Z")
  
  instData <- readNWISdata(args)
  
  args <- list(sites="05114000", service="dv", 
               parameterCd="00060", 
               startDate="2014-05-01",
               endDate="2014-05-01")
  
  dailyData <- readNWISdata(args)
  expect_lt(nrow(dailyData), nrow(instData))
  args <- list(stateCd="OH",parameterCd="00665")
  sites <- whatNWISsites(args)
  expect_type(sites, "list")
})


test_that("General WQP retrievals working", {
  testthat::skip_on_cran()
  nameToUse <- "pH"
  pHData <- readWQPdata(siteid="USGS-04024315",characteristicName=nameToUse)
  expect_is(pHData$ActivityStartDateTime, 'POSIXct')
  
  #testing lists:
  startDate <- as.Date("2013-01-01")
  secchi.names = c("Depth, Secchi disk depth",
                   "Depth, Secchi disk depth (choice list)",
                   "Secchi Reading Condition (choice list)",
                   "Secchi depth",
                   "Water transparency, Secchi disc")
  args_2 <- list('startDateLo' = startDate,
               'startDateHi' = "2013-12-31",
                statecode="WI",
                characteristicName=secchi.names)

  wqp.summary <- readWQPdata(args_2, querySummary = TRUE)
  expect_true("list" %in% class(wqp.summary))
  
  #pretty sloooow:
  wqp.data <- readWQPdata(args_2, querySummary = FALSE)
  expect_false("list" %in% class(wqp.data))
  
  # Testing multiple lists:
  arg_3 <- list('startDateLo' = startDate,
               'startDateHi' = "2013-12-31")
  arg_4 <- list(statecode="WI",
                characteristicName=secchi.names)
  wqp.summary <- readWQPdata(arg_3, arg_4, querySummary=TRUE)
  expect_true("list" %in% class(wqp.summary))
  
  lakeSites <- whatWQPsites(args_2)
  expect_type(lakeSites, "list")
  
  # Known slow query for WQP:
  # pHDataExpanded2 <- readWQPdata(bBox=c(-90.1,42.9,-89.9,43.1),
  #                                characteristicName=nameToUse, querySummary = TRUE)
  # expect_is(pHDataExpanded2, 'list')
  
  # Super slow:
  # startDate <- as.Date("2013-01-01")
  # nutrientDaneCounty <- readWQPdata(countycode="US:55:025",startDate=startDate,
  #                        characteristicType="Nutrient")
  # expect_is(nutrientDaneCounty$ActivityStartDateTime, 'POSIXct')
})

test_that("WQP head query retrievals working", {
  testthat::skip_on_cran()
  nameToUse <- "pH"
  pHDataQueryResults <- readWQPdata(siteid="USGS-04024315",
                                    characteristicName=nameToUse, 
                                    querySummary=TRUE)
  expect_false(is.null(pHDataQueryResults$date))
  expect_is(pHDataQueryResults$date, 'Date')
  expect_false(is.null(pHDataQueryResults$`total-site-count`))
  expect_is(pHDataQueryResults$`total-site-count`, 'numeric')
  expect_false(is.null(pHDataQueryResults$`total-result-count`))
  expect_is(pHDataQueryResults$`total-result-count`, 'numeric')
  
  pHDataQueryResults <- readWQPqw(siteNumbers="USGS-04024315",
                                  parameterCd=nameToUse, 
                                  querySummary=TRUE)
  expect_false(is.null(pHDataQueryResults$date))
  expect_is(pHDataQueryResults$date, 'Date')
  expect_false(is.null(pHDataQueryResults$`total-site-count`))
  expect_is(pHDataQueryResults$`total-site-count`, 'numeric')
  expect_false(is.null(pHDataQueryResults$`total-result-count`))
  expect_is(pHDataQueryResults$`total-result-count`, 'numeric')
})

test_that("zeroPad handles NAs", {
  toPad <- c(1,5,55,NA)
  padded <- zeroPad(toPad,3)
  expect_true(identical(c("001","005","055",NA),padded))
})

test_that("Dates with no days can be handled", {
  testthat::skip_on_cran()
  expect_error(readNWISgwl("425957088141001", startDate = "1980-01-01"))
 })

context("whatWQPsamples")
test_that("whatNWISsites working", {
  testthat::skip_on_cran()
  siteInfo <- whatWQPsamples(siteid="USGS-01594440")
  expect_true(nrow(siteInfo) > 0)
  
  })

context("whatNWISsites")
test_that("whatNWISsites working", {
  testthat::skip_on_cran()
  siteListPhos <- whatNWISsites(stateCd="OH",parameterCd="00665")
  expect_true(nrow(siteListPhos) > 0)
  expect_true(is.numeric(siteListPhos$dec_lat_va))
  
  bboxSites <- whatNWISsites(bbox = c(-92.5, 45.4, -87, 47), parameterCd="00060")
  expect_true(nrow(bboxSites) > 0)
  expect_true(is.numeric(bboxSites$dec_lat_va))
})

context("readWQPdots")
test_that("readWQPdots working", {
  testthat::skip_on_cran()
  
  # bbox vector turned into single string with coords separated by semicolons
  formArgs_bbox <- dataRetrieval:::readWQPdots(bbox = c(-92.5, 45.4, -87, 47))
  expect_true(length(formArgs_bbox) == 2)
  expect_true(length(gregexpr(";", formArgs_bbox)[[1]]) == 3)
  
  # NWIS names (siteNumber) converted to WQP expected names (siteid)
  formArgs_site <- dataRetrieval:::readWQPdots(siteNumber="04010301")
  expect_true(length(formArgs_site) == 2)
  expect_true("siteid" %in% names(formArgs_site))
  expect_false("siteNumber" %in% names(formArgs_site))
  
  # NWIS names (stateCd) converted to WQP expected names (statecode)
  formArgs <- dataRetrieval:::readWQPdots(stateCd="OH",parameterCd="00665")
  expect_true(length(formArgs) == 3)
  expect_true("statecode" %in% names(formArgs))
  expect_false("stateCd" %in% names(formArgs))
})
