context("General functions")

test_that("General USGS retrievals working", {
  testthat::skip_on_cran()
  
  cql <- '{
  "op": "and",
  "args": [
    {
      "op": "in",
      "args": [
        { "property": "parameter_code" },
        [ "00060", "00065" ]
      ]
    },
   {
      "op": "in",
      "args": [
        { "property": "monitoring_location_id" },
        [ "USGS-07367300", "USGS-03277200" ]
      ]
    }
  ]
  }'
  
  dv_data <- read_USGS_data(service = "daily",
                            CQL = cql,
                            time = c("2023-01-01", "2024-01-01"))
  expect_equal(as.Date(c("2023-01-01", "2024-01-01")), 
               range(dv_data$time))
  expect_true(all(unique(dv_data$monitoring_location_id) %in%
                    c("USGS-07367300", "USGS-03277200")))
  
  
  cql_not_active <- '{
  "op": "and",
  "args": [
    {
      "op": "in",
      "args": [
        { "property": "parameter_code" },
        [ "00060", "00065" ]
      ]
    },
   {
      "op": "in",
      "args": [
        { "property": "monitoring_location_id" },
        [ "USGS-05212700"]
      ]
    }
  ]
  }'
  
  notActiveUSGS <- read_USGS_data(CQL = cql_not_active,
                                  service = "daily",
                                  time =  c("2014-01-01", "2014-01-07"))
  

})

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

  siteInfo <- readNWISdata(
    stateCd = "WI",
    parameterCd = "00010",
    hasDataTypeCd = "iv",
    service = "site"
  )
  expect_is(siteInfo$station_nm, "character")

  gw_data <- readNWISdata(
    stateCd = "AL",
    service = "gwlevels",
    startDate = "2024-05-01",
    endDate = "2024-05-30") 
  
  expect_true(nrow(gw_data) > 0)
  expect_equal(attr(gw_data, "url"),
               "https://nwis.waterdata.usgs.gov/nwis/gwlevels?state_cd=AL&begin_date=2024-05-01&end_date=2024-05-30&date_format=YYYY-MM-DD&rdb_inventory_output=file&TZoutput=0&range_selection=date_range&list_of_search_criteria=state_cd&format=rdb")
  
  gw_data2 <- readNWISdata(
    state_cd = "AL",
    service = "gwlevels",
    startDate = "2024-05-01",
    endDate = "2024-05-30") 
  
  expect_equal(nrow(gw_data), nrow(gw_data2))
  
  # nolint start: line_length_linter
  url <- httr2::request("https://waterservices.usgs.gov/nwis/dv/?site=09037500&format=rdb&ParameterCd=00060&StatCd=00003&startDT=1985-10-02&endDT=2012-09-06")
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
  urlTest <- httr2::request("https://nwis.waterservices.usgs.gov/nwis/iv/?site=11447650&format=waterml,1.1&ParameterCd=63680&startDT=2016-12-13&endDT=2016-12-13")
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

  args2 <- list(
    monitoring_location_id = "USGS-05114000", 
    parameter_code = "00060",
    time = c("2014-05-01", endDate = "2014-05-01")
  )
  
  daily_USGS <- do.call(read_USGS_daily, args2)
  expect_lt(nrow(daily_USGS), nrow(instData))

  ohio <- read_USGS_monitoring_location(state_name = "Ohio", 
                                        site_type_code = "ST")
  bbox <- sf::st_bbox(ohio)
  what_sites <- read_USGS_ts_meta(parameter_code = "00665",
                                  bbox = as.numeric(bbox))
  expect_true(all(c("monitoring_location_id",
                "begin", "end", "parameter_name") %in% names(what_sites)))
  
  huc <- read_USGS_monitoring_location(hydrologic_unit_code = "02080202")
  expect_true(nrow(huc) > 0)

  # Test counties:
  
  county_code <- countyCdLookup(state = "Virginia", county = "Stafford")
  stafford <- read_USGS_monitoring_location(county_code = "179", 
                                            state_code = "51")
  stafford_bbox <- sf::st_bbox(stafford)
  
  dailyStaffordVA <- read_USGS_daily(
    bbox = as.numeric(stafford_bbox),
    parameter_code = "00060",
    time = c("2015-01-01", "2015-01-30")
  )
  expect_gt(nrow(dailyStaffordVA), 1)

  # America Samoa?
  AS <- read_USGS_monitoring_location(state_name = "American Samoa")
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
  multi_huc_sites <- read_USGS_monitoring_location(hydrologic_unit_code = multi_hucs)
  
  multi_huc <- read_USGS_daily(bbox = as.numeric(sf::st_bbox(multi_huc_sites)),
                               parameter_code = "63680",
                               statistic_id = "00003",
                               time = c("2015-06-18", "2015-06-18")
  )
  expect_equal(4, length(unique(multi_huc$monitoring_location_id)))

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
  pHData <- readWQPdata(siteid = "USGS-04024315",
                        characteristicName = nameToUse,
                        service = "ResultWQX3")
  expect_is(pHData$Activity_StartDateTime, "POSIXct")
  # 
  # # testing lists:
  startDate <- as.Date("2022-01-01")
  secchi.names <- c("Depth, Secchi disk depth",
  "Secchi depth",
  "Water transparency, Secchi disc",
  "Depth, Secchi disk depth (choice list)")
  # "Transparency, Secchi tube with disk",
  # "Secchi Reading Condition (choice list)",
  # "Depth, Secchi disk visible at bottom (Y/N) (choice list)")

  args_2 <- list(
    "startDateLo" = startDate,
    "startDateHi" = "2024-01-01",
    statecode = "WI",
    characteristicName = secchi.names
  )

  # # Testing multiple lists:
  arg_3 <- list(
    "startDateLo" = startDate,
    "startDateHi" = "2023-12-31"
  )
  arg_4 <- list(
    statecode = "WI",
    characteristicName = secchi.names
  )

  wqp.summary_no_atts <- readWQPdata(
    siteid = "USGS-04024315",
    characteristicName = nameToUse,
    ignore_attributes = TRUE,
    service = "ResultWQX3"
  )
  expect_true(!all(c("siteInfo", "variableInfo") %in% names(attributes(wqp.summary_no_atts))))

  rawPcode <- readWQPqw("USGS-01594440", "01075", "", "", legacy = FALSE)
  expect_true(all(c("url", "queryTime", "siteInfo", "headerInfo") %in%
                    names(attributes(rawPcode))))

  # This means wqp_check_status was called:
  expect_true("dataProviders" %in% names(attr(rawPcode, "headerInfo")))
  
  rawPcode2 <- readWQPqw("USGS-01594440", "01075", "", "", ignore_attributes = TRUE)
  expect_true(all(!c( "queryTime", "siteInfo") %in%
                    names(attributes(rawPcode2))))
  
  # This means wqp_check_status wasn't called:
  expect_false("dataProviders" %in% names(attr(rawPcode2, "headerInfo")))
  
  # pHData <- readWQPdata(siteid = "USGS-04024315",
  #                       characteristicName = "pH",
  #                       service = "ResultWQX3")
  # expect_true(all(c("url", "queryTime", "siteInfo", "headerInfo") %in%
  #                   names(attributes(pHData))))
  # 
  # # This means wqp_check_status was called:
  # expect_true("dataProviders" %in% names(attr(pHData, "headerInfo")))
  # 
  # pHData2 <- readWQPdata(siteid = "USGS-04024315",
  #                       characteristicName = "pH",
  #                       ignore_attributes = TRUE,
  #                       service = "ResultWQX3")
  # expect_true(all(!c("queryTime", "siteInfo") %in%
  #                   names(attributes(pHData2))))
  # 
  # # This means wqp_check_status was called:
  # expect_false("dataProviders" %in% names(attr(pHData2, "headerInfo")))
  # 
  # rawPcode <- readWQPqw("USGS-01594440", "01075",
  #                       ignore_attributes = TRUE, legacy = FALSE)
  # headerInfo <- attr(rawPcode, "headerInfo")
  # wqp_request_id <- headerInfo$`wqp-request-id`
  # count_info <- wqp_check_status(wqp_request_id)
  # 
  # expect_true("dataProviders" %in% names(count_info))
  
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
  
  #gwlevels:
  info <- whatNWISsites(stateCd = "NY", service="gwlevels") 
  expect_true(nrow(info) > 0)
  expect_equal(attr(info, "url"), "https://waterservices.usgs.gov/nwis/site/?stateCd=NY&hasDataTypeCd=gw&format=mapper")
})

context("readWQPdots")
test_that("readWQPdots working", {
  testthat::skip_on_cran()

  # bbox vector turned into single string with coords separated by semicolons
  formArgs_bbox <- dataRetrieval:::readWQPdots(bbox = c(-92.5, 45.4, -87, 47))
  expect_true(length(formArgs_bbox) == 2)
  expect_true(length(formArgs_bbox$values$bBox) == 1)

  # NWIS names (siteNumber) converted to WQP expected names (siteid)
  formArgs_site <- dataRetrieval:::readWQPdots(siteNumber = "04010301")
  expect_true(length(formArgs_site) == 2)
  expect_true("siteid" %in% names(formArgs_site$values))
  expect_false("siteNumber" %in% names(formArgs_site$values))

  # NWIS names (stateCd) converted to WQP expected names (statecode)
  formArgs <- dataRetrieval:::readWQPdots(stateCd = "OH", parameterCd = "00665")
  expect_true(length(formArgs$values) == 4)
  expect_true("statecode" %in% names(formArgs$values))
  expect_false("stateCd" %in% names(formArgs$values))
  
  bbox <- c(-86.97361, 34.48827, -86.61349,  34.65623)
  what_bbox <- whatWQPdata(bBox = bbox)
  expect_true(nrow(what_bbox) > 0)
  x <- whatWQPsites(bBox = bbox)
  expect_true(nrow(x) > 0)
  df <- readWQPdata(bBox = bbox,
                    characteristicName = "Total Coliform",
                    startDateLo = "2023-01-01",
                    startDateHi = "2023-12-31",
                    service = "Result",
                    dataProfile = "narrowResult")
  expect_true(nrow(df) > 0)
  df_legacy <- readWQPdata(bBox = bbox,
                           characteristicName = "Total Coliform",
                           startDateLo = "2023-01-01",
                           startDateHi = "2023-12-31",
                           service = "Result",
                           dataProfile = "narrowResult")
  expect_true(nrow(df_legacy) > 0)
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


test_that("importWQP convertType", {
  testthat::skip_on_cran()

  SC <- readWQPqw(siteNumbers = "USGS-05288705", parameterCd = "00300", 
                  convertType = FALSE, legacy = TRUE)
  expect_is(SC$ResultMeasureValue, "character")

  lakeSites_chars <- whatWQPdata(
    siteType = "Lake, Reservoir, Impoundment",
    statecode = "US:55", convertType = FALSE
  )
  expect_is(lakeSites_chars$lat, "character")
})
