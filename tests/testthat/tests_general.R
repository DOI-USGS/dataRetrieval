context("General functions")

test_that("General NWIS retrievals working", {
  testthat::skip_on_cran()

  multiSite <- readNWISdata(
    sites = c("04025500", "040263491"), service = "iv",
    parameterCd = "00060",
    startDate = "2020-11-01", endDate = "2020-11-02"
  )
  expect_is(multiSite$dateTime, "POSIXct")

  recent_uv <- readNWISdata(
    siteNumber = "04025500", parameterCd = "00060",
    service = "uv",
    startDate = as.Date(Sys.Date() - 10),
    endDate = Sys.Date()
  )
  expect_equal(grep(
    x = attr(recent_uv, "url"),
    pattern = "https://waterservices.usgs.gov/nwis/iv/"
  ), 1)

  older_uv <- readNWISdata(
    siteNumber = "04025500",
    parameterCd = "00060",
    service = "uv",
    startDate = "2016-01-01",
    endDate = "2016-01-02"
  )
  expect_equal(grep(
    x = attr(older_uv, "url"),
    pattern = "https://nwis.waterservices.usgs.gov/nwis/iv/"
  ), 1)


  expect_error(readNWISdata(), "No arguments supplied")
  expect_error(readNWISdata(siteNumber = NA), "NA's are not allowed in query")

  bBoxEx <- readNWISdata(bBox = c(-83, 36.5, -81, 38.5), parameterCd = "00010")
  expect_true(length(unique(bBoxEx$site_no)) > 1)

  startDate <- as.Date("2013-10-01")
  endDate <- as.Date("2014-09-30")
  waterYear <- readNWISdata(
    bBox = c(-83, 36.5, -81, 38.5),
    parameterCd = "00010",
    service = "dv",
    startDate = startDate,
    endDate = endDate
  )
  expect_is(waterYear$dateTime, "POSIXct")

  siteInfo <- readNWISdata(
    stateCd = "WI",
    parameterCd = "00010",
    hasDataTypeCd = "iv",
    service = "site"
  )
  expect_is(siteInfo$station_nm, "character")

  # nolint start: line_length_linter
  url <- "https://waterservices.usgs.gov/nwis/dv/?site=09037500&format=rdb&ParameterCd=00060&StatCd=00003&startDT=1985-10-02&endDT=2012-09-06"
  dv <- importRDB1(url, asDateTime = FALSE)
  # nolint end
  dailyStat <- readNWISdata(
    site = c("03112500", "03111520", "02319394"),
    service = "stat", statReportType = "daily",
    statType = c("p25", "p50", "p75", "min", "max"),
    parameterCd = "00065",
    convertType = FALSE
  )
  expect_true(length(dailyStat$min_va) > 1)
  expect_is(dailyStat$p25_va, "character")

  waterYearStat <- readNWISdata(
    site = c("01646500"),
    service = "stat", statReportType = "annual",
    statYearType = "water", missingData = "on"
  )
  expect_is(waterYearStat$mean_va, "numeric")
  expect_is(waterYearStat$parameter_cd, "character")

  # Empty data
  # note....not empty anymore!
  # nolint start: line_length_linter
  urlTest <- "https://nwis.waterservices.usgs.gov/nwis/iv/?site=11447650&format=waterml,1.1&ParameterCd=63680&startDT=2016-12-13&endDT=2016-12-13"
  x <- importWaterML1(urlTest)
  expect_true(all(c("agency_cd", "site_no", "dateTime", "tz_cd") %in% names(x)))
  # nolint end

  # Test list:
  args <- list(
    sites = "05114000", service = "iv",
    parameterCd = "00060",
    startDate = "2014-05-01T00:00Z",
    endDate = "2014-05-01T12:00Z"
  )

  instData <- readNWISdata(args)

  args <- list(
    sites = "05114000", service = "dv",
    parameterCd = "00060",
    startDate = "2014-05-01",
    endDate = "2014-05-01"
  )

  dailyData <- readNWISdata(args)
  expect_lt(nrow(dailyData), nrow(instData))
  args <- list(stateCd = "OH", parameterCd = "00665")
  sites <- whatNWISsites(args)
  expect_type(sites, "list")

  # Test counties:
  dailyStaffordVA <- readNWISdata(
    stateCd = "Virginia",
    countyCd = "Stafford",
    parameterCd = "00060",
    startDate = "2015-01-01",
    endDate = "2015-01-30"
  )
  expect_gt(nrow(dailyStaffordVA), 1)

  AS <- readNWISdata(stateCd = "AS", service = "site")
  expect_gt(nrow(AS), 0)

  site_id <- "01594440"
  rating_curve <- readNWISdata(
    service = "rating",
    site_no = site_id,
    file_type = "base"
  )
  rating_curve2 <- readNWISrating(
    siteNumber = site_id,
    type = "base"
  )
  expect_equal(
    attr(rating_curve, "url"),
    "https://waterdata.usgs.gov/nwisweb/get_ratings/?site_no=01594440&file_type=base"
  )
  expect_equal(rating_curve$INDEP, rating_curve2$INDEP)

  state_rating_list <- readNWISdata(
    service = "rating",
    file_type = "base",
    period = 24
  )
  expect_true(all(names(state_rating_list) %in% c(
    "agency_cd",
    "site_no",
    "type",
    "update_time",
    "url"
  )))

  multi_hucs <- c("07130007", "07130011")
  multi_huc <- dataRetrieval::readNWISdata(
    huc = multi_hucs,
    parameterCd = "63680",
    startDate = "2015-06-18",
    endDate = "2015-06-18",
    service = "dv"
  )
  expect_equal(2, nrow(multi_huc))


  peak_data <- readNWISdata(
    service = "peak",
    state_cd = "PA"
  )
  expect_lt(nrow(peak_data), 100000)

  peak_data <- readNWISdata(
    service = "peak",
    huc2_cd = "20"
  )
  expect_lt(nrow(peak_data), 100000)
})

test_that("whatNWISdata", {

  # no service specified:
  availableData <- whatNWISdata(siteNumber = "05114000")
  expect_equal(ncol(availableData), 24)

  uvData <- whatNWISdata(siteNumber = "05114000", service = "uv")
  expect_equal(unique(uvData$data_type_cd), "uv")

  # multiple services
  uvDataMulti <- whatNWISdata(
    siteNumber = c("05114000", "09423350"),
    service = c("uv", "dv")
  )
  expect_true(all(unique(uvDataMulti$data_type_cd) %in% c("uv", "dv")))

  # state codes:
  flowAndTemp <- whatNWISdata(
    stateCd = "WI", service = c("uv", "dv"),
    parameterCd = c("00060", "00010"),
    statCd = "00003"
  )
  expect_true(all(unique(flowAndTemp$data_type_cd) %in% c("uv", "dv")))
  expect_true(all(unique(flowAndTemp$parm_cd) %in% c("00060", "00010")))
  expect_true(all(unique(flowAndTemp$stat_cd) %in% c("00003", NA)))

  # site service
  sites <- whatNWISdata(stateCd = "WI", service = "site")
  expect_true(all(c("gw", "sv", "qw", "dv", "pk", "uv")
                  %in% unique(sites$data_type_cd)))
})

test_that("General WQP retrievals working", {
  testthat::skip_on_cran()
  nameToUse <- "pH"
  pHData <- readWQPdata(siteid = "USGS-04024315", characteristicName = nameToUse)
  expect_is(pHData$Activity_StartDateTime, "POSIXct")

  # testing lists:
  startDate <- as.Date("2023-01-01")
  secchi.names <- c(
    "Depth, Secchi disk depth",
    "Depth, Secchi disk depth (choice list)",
    "Secchi Reading Condition (choice list)",
    "Water transparency, Secchi disc"
  )
  args_2 <- list(
    "startDateLo" = startDate,
    "startDateHi" = "2013-12-31",
    statecode = "WI",
    characteristicName = secchi.names
  )

  # Testing multiple lists:
  arg_3 <- list(
    "startDateLo" = startDate,
    "startDateHi" = "2023-12-31"
  )
  arg_4 <- list(
    statecode = "WI",
    characteristicName = secchi.names
  )

  lakeSites <- whatWQPsites(args_2)
  expect_type(lakeSites, "list")

  wqp.summary_no_atts <- readWQPdata(
    siteid = "USGS-04024315",
    characteristicName = nameToUse,
    ignore_attributes = TRUE
  )
  expect_true(!all(c("siteInfo", "variableInfo") %in% names(attributes(wqp.summary_no_atts))))
  
  rawPcode <- readWQPqw("USGS-01594440", "01075", "", "")
  expect_true(all(c("url", "queryTime", "siteInfo", "headerInfo") %in%
                    names(attributes(rawPcode))))
  
  # This means wqp_check_status was called:
  expect_true("dataProviders" %in% names(attr(rawPcode, "headerInfo")))
  
  rawPcode2 <- readWQPqw("USGS-01594440", "01075", "", "", ignore_attributes = TRUE)
  expect_true(all(!c( "queryTime", "siteInfo") %in%
                    names(attributes(rawPcode2))))
  
  # This means wqp_check_status wasn't called:
  expect_false("dataProviders" %in% names(attr(rawPcode2, "headerInfo")))
  
  pHData <- readWQPdata(siteid = "USGS-04024315",
                        characteristicName = "pH")
  expect_true(all(c("url", "queryTime", "siteInfo", "headerInfo") %in%
                    names(attributes(pHData))))
  
  # This means wqp_check_status was called:
  expect_true("dataProviders" %in% names(attr(pHData, "headerInfo")))
  
  pHData2 <- readWQPdata(siteid = "USGS-04024315",
                        characteristicName = "pH",
                        ignore_attributes = TRUE)
  expect_true(all(!c("queryTime", "siteInfo") %in%
                    names(attributes(pHData2))))
  
  # This means wqp_check_status was called:
  expect_false("dataProviders" %in% names(attr(pHData2, "headerInfo")))
  
  rawPcode <- readWQPqw("USGS-01594440", "01075", ignore_attributes = TRUE)
  headerInfo <- attr(rawPcode, "headerInfo")
  wqp_request_id <- headerInfo$`wqp-request-id`
  count_info <- wqp_check_status(wqp_request_id)
  
  expect_true("dataProviders" %in% names(count_info))
  
})


test_that("zeroPad handles NAs", {
  toPad <- c(1, 5, 55, NA)
  padded <- zeroPad(toPad, 3)
  expect_true(identical(c("001", "005", "055", NA), padded))
})

test_that("Dates with no days can be handled", {
  testthat::skip_on_cran()
  empty_df <- readNWISgwl("425957088141001", startDate = "1980-01-01")
  expect_true(nrow(empty_df) > 0)
})

context("whatWQPsamples")
test_that("whatWQPsamples working", {
  testthat::skip_on_cran()
  # The warning is caused by a confirmed bug in WQP
  siteInfo <- whatWQPsamples(siteid = "USGS-01594440")
  expect_true(nrow(siteInfo) > 0)
})

context("whatWQPmetrics")
test_that("whatWQPmetrics working", {
  testthat::skip_on_cran()
  type <- "Stream"
  siteInfo <- whatWQPmetrics(countycode = "US:55:025", siteType = type)
  expect_true(ncol(siteInfo) >= 21)
})

context("whatWQPdata")
test_that("whatWQPdata working", {
  testthat::skip_on_cran()

  site1 <- whatWQPdata(siteid = "USGS-01594440")
  expect_is(site1, "data.frame")
  expect_equal(1, nrow(site1))

  type <- "Stream"
  sites <- whatWQPdata(countycode = "US:55:025", siteType = type)
  expect_gt(nrow(sites), 1)

  lakeSites <- whatWQPdata(siteType = "Lake, Reservoir, Impoundment", statecode = "US:55")
  expect_is(lakeSites$activityCount, "numeric")
})

context("whatNWISsites")
test_that("whatNWISsites working", {
  testthat::skip_on_cran()
  siteListPhos <- whatNWISsites(stateCd = "OH", parameterCd = "00665")
  expect_true(nrow(siteListPhos) > 0)
  expect_true(is.numeric(siteListPhos$dec_lat_va))

  bboxSites <- whatNWISsites(bbox = c(-92.5, 45.4, -87, 47), parameterCd = "00060")
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
  formArgs_site <- dataRetrieval:::readWQPdots(siteNumber = "04010301")
  expect_true(length(formArgs_site) == 2)
  expect_true("siteid" %in% names(formArgs_site$values))
  expect_false("siteNumber" %in% names(formArgs_site$values))

  # NWIS names (stateCd) converted to WQP expected names (statecode)
  formArgs <- dataRetrieval:::readWQPdots(stateCd = "OH", parameterCd = "00665")
  expect_true(length(formArgs$values) == 2)
  expect_true("statecode" %in% names(formArgs$values))
  expect_false("stateCd" %in% names(formArgs$values))
})


context("getWebServiceData")
test_that("long urls use POST", {
  testthat::skip_on_cran()
  baseURL <- dataRetrieval:::drURL("Result")
  url <- paste0(baseURL,
    rep("reallylongurl", 200),
    collapse = ""
  )
  with_mock(
    RETRY = function(method, ...) {
      return(method == "POST")
    },
    status_code = function(resp) 200,
    headers = function(resp) list(`content-type` = "logical"),
    content = function(resp, encoding) resp,
    expect_true(getWebServiceData(url)),
    .env = "httr"
  )
})

test_that("ngwmn urls don't use post", {
  testthat::skip_on_cran()
  baseURL <- dataRetrieval:::drURL("NGWMN")
  url <- paste0(baseURL,
    rep("urlwithngwmn", 200),
    collapse = ""
  )
  with_mock(
    RETRY = function(method, ...) {
      return(method == "POST")
    },
    status_code = function(resp) 200,
    headers = function(resp) list(`content-type` = "logical"),
    content = function(resp, encoding) resp,
    expect_false(getWebServiceData(url)),
    .env = "httr"
  )
})

test_that("400 errors return a verbose error", {
  testthat::skip_on_cran()
  # nolint start: line_length_linter
  url <- "https://waterservices.usgs.gov/nwis/site/?stateCd=IA&bBox=-92.821445,42.303044,-92.167168,42.646524&format=mapper"
  # nolint end
  expect_message(getWebServiceData(url))
})

test_that("internal functions", {

  # get empty_col type
  expect_equal(dataRetrieval:::empty_col("numeric"), numeric())
  expect_equal(dataRetrieval:::empty_col("character"), character())
  expect_equal(dataRetrieval:::empty_col("Date"), as.Date(numeric(), origin = "1970-01-01"))

  # add empty columns
  df1 <- data.frame(
    a = 1:2,
    b = c("a", "b"),
    c = as.Date(c("2010-01-01", "2010-01-02")),
    stringsAsFactors = FALSE
  )
  df2 <- data.frame(
    d = 1:2,
    b = c("a", "b"),
    e = factor(c("c", "d")),
    stringsAsFactors = FALSE
  )

  df2_ret <- dataRetrieval:::add_empty_col(df2, df1, c("a", "c"))
  df1_ret <- dataRetrieval:::add_empty_col(df1, df2, c("d", "e"))

  expect_true(ncol(df2_ret) == 5)
  expect_true(ncol(df1_ret) == 5)
  expect_true(all(is.na(df2_ret[, c("a", "c")])))
  expect_true(all(is.na(df1_ret[, c("d", "e")])))

  bound_df <- dataRetrieval:::r_bind_dr(df1, df2)

  expect_true(nrow(bound_df) == 4)
  expect_true(class(bound_df$c) == "Date")
  expect_true(class(bound_df$a) == "integer")
  expect_true(class(bound_df$b) == "character")
})

test_that("profiles", {
  # Data profiles: "Organization Data"
  org_data <- readWQPdata(
    statecode = "WI",
    countycode = "Dane",
    service = "Organization"
  )

  expect_true(all(c(
    "OrganizationIdentifier",
    "OrganizationFormalName"
  ) %in% names(org_data)))

  # Data profiles: "Site Data Only"
  site_data <- readWQPdata(
    statecode = "WI",
    countycode = "Dane",
    service = "StationWQX"
  )

  expect_true(all(c("ProviderName", "Location_Identifier") %in% names(site_data)))

  # Data profiles: "Project Data"
  project_data <- readWQPdata(
    statecode = "WI",
    countycode = "Dane",
    service = "Project"
  )

  expect_true(all(c(
    "OrganizationIdentifier",
    "OrganizationFormalName"
  ) %in% names(project_data)))

  # Data profiles: "Project Monitoring Location Weighting Data"
  proj_mlwd <- readWQPdata(
    statecode = "WI",
    countycode = "Dane",
    service = "ProjectMonitoringLocationWeighting"
  )

  expect_true(all(c(
    "OrganizationIdentifier",
    "OrganizationFormalName"
  ) %in% names(proj_mlwd)))

  # Data profiles: "narrow":
  samp_data <- readWQPdata(
    siteid = "USGS-04024315",
    service = "WQX",
    dataProfile = "narrow"
  )

  expect_true(all(c(
    "Activity_StartDateTime",
    "LastChangeDate"
  ) %in% names(samp_data)))

  # Data profiles: "Sample Results (biological metadata)"
  samp_bio <- readWQPdata(
    siteid = "USGS-04024315",
    dataProfile = "biological",
    service = "Result"
  )

  expect_true(all(c(
    "OrganizationIdentifier",
    "OrganizationFormalName"
  ) %in% names(samp_bio)))

  # Data profiles: "Sample Results (narrow)"
  samp_narrow <- readWQPdata(
    siteid = "USGS-04024315",
    dataProfile = "narrowResult",
    service = "Result"
  )

  expect_true(all(c(
    "OrganizationIdentifier",
    "OrganizationFormalName"
  ) %in% names(samp_narrow)))

  # Data profiles: "Sampling Activity"
  samp_activity <- readWQPdata(
    siteid = "USGS-04024315",
    dataProfile = "activityAll",
    service = "Activity"
  )

  expect_true(all(c(
    "OrganizationIdentifier",
    "OrganizationFormalName"
  ) %in% names(samp_activity)))

  # Data profile: "Sampling Activity Metrics"
  act_metrics <- readWQPdata(
    statecode = "WI",
    countycode = "Dane",
    service = "ActivityMetric"
  )

  expect_true(all(c(
    "OrganizationIdentifier",
    "OrganizationFormalName"
  ) %in% names(act_metrics)))

  # Data profile: "Result Detection Quantitation Limit Data"
  dl_data <- readWQPdata(
    siteid = "USGS-04024315",
    service = "ResultDetectionQuantitationLimit"
  )

  expect_true(all(c(
    "OrganizationIdentifier",
    "OrganizationFormalName"
  ) %in% names(dl_data)))
})

test_that("readWQPsummary", {
  testthat::skip_on_cran()

  dane_county_data <- readWQPsummary(
    countycode = "US:55:025",
    summaryYears = 5,
    siteType = "Stream"
  )

  summary_names <- c(
    "Provider",
    "MonitoringLocationIdentifier",
    "YearSummarized",
    "CharacteristicType",
    "CharacteristicName",
    "ActivityCount",
    "ResultCount",
    "LastResultSubmittedDate",
    "OrganizationIdentifier",
    "OrganizationFormalName",
    "MonitoringLocationName",
    "MonitoringLocationTypeName",
    "ResolvedMonitoringLocationTypeName",
    "HUCEightDigitCode",
    "MonitoringLocationUrl",
    "CountyName",
    "StateName",
    "MonitoringLocationLatitude",
    "MonitoringLocationLongitude"
  )

  expect_true(all(summary_names %in% names(dane_county_data)))
  expect_true(diff(range(dane_county_data$YearSummarized)) <= 5)

  lake_sites <- readWQPsummary(
    siteType = "Lake, Reservoir, Impoundment",
    CharacteristicName = "Temperature, water",
    countycode = "US:55:025"
  )

  expect_true(all(summary_names %in% names(lake_sites)))
  expect_true(diff(range(lake_sites$YearSummarized)) >= 5)

  site1 <- readWQPsummary(
    siteid = "USGS-07144100",
    summaryYears = 5
  )

  expect_type(site1$ActivityCount, "double")
  expect_type(site1$MonitoringLocationIdentifier, "character")
  # nolint start: line_length_linter
  expect_equal(
    attr(site1, "url"),
    "https://www.waterqualitydata.us/data/summary/monitoringLocation/search?siteid=USGS-07144100&summaryYears=5&dataProfile=periodOfRecord&mimeType=csv"
  )
  # nolint end
})

test_that("importWQP convertType", {
  testthat::skip_on_cran()

  rawSampleURL_NoZip <- constructWQPURL("USGS-01594440", "01075", "", "")
  rawSampleURL_NoZip_char <- importWQP(rawSampleURL_NoZip, convertType = FALSE)
  expect_is(rawSampleURL_NoZip_char$Result_Measure, "character")

  phos <- readWQPdata(statecode = "WI", countycode = "Dane",
                    characteristicName = "Phosphorus",
                    startDateLo = "2022-01-01",
                    convertType = FALSE)
  expect_is(phos$Result_Measure, "character")

  SC <- readWQPqw(siteNumbers = "USGS-05288705", parameterCd = "00300", convertType = FALSE)
  expect_is(SC$Result_Measure, "character")

  lakeSites_chars <- whatWQPdata(
    siteType = "Lake, Reservoir, Impoundment",
    statecode = "US:55", convertType = FALSE
  )
  expect_is(lakeSites_chars$lat, "character")
})
