context("Unit values")

test_that("Unit value data returns correct types", {
  testthat::skip_on_cran()

  siteNumber <- "05114000"
  parameterCd <- "00060"
  startDate <- "2014-10-10"
  endDate <- "2014-10-10"

  rawData <- readNWISuv(siteNumber, parameterCd, startDate, endDate)
  rawData <- renameNWISColumns(rawData)

  spreadOver120 <- readNWISuv(
    siteNumber, parameterCd,
    as.Date(Sys.Date() - 200),
    Sys.Date()
  )

  expect_true(min(spreadOver120$dateTime) < as.POSIXct(Sys.Date(), tz = "UTC"))

  recent_uv <- readNWISuv(
    siteNumber, parameterCd,
    as.Date(Sys.Date() - 10),
    Sys.Date()
  )
  expect_equal(grep(
    x = attr(recent_uv, "url"),
    pattern = "https://waterservices.usgs.gov/nwis/iv/"
  ), 1)
  expect_equal(grep(
    x = attr(spreadOver120, "url"),
    pattern = "https://nwis.waterservices.usgs.gov/nwis/iv/"
  ), 1)
  # nolint start: line_length_linter
  expect_equal(
    attr(rawData, "url"),
    "https://nwis.waterservices.usgs.gov/nwis/iv/?site=05114000&format=waterml,1.1&ParameterCd=00060&startDT=2014-10-10&endDT=2014-10-10"
  )
  # nolint end
  timeZoneChange <- readNWISuv(c("04024430", "04024000"), parameterCd,
    "2013-11-03", "2013-11-03",
    tz = "America/Chicago"
  )


  timeZoneChange <- renameNWISColumns(timeZoneChange)
  expect_is(rawData$dateTime, "POSIXct")
  expect_is(rawData$Flow_Inst, "numeric")
  # nolint start: line_length_linter
  expect_equal(attr(rawData, "url"), "https://nwis.waterservices.usgs.gov/nwis/iv/?site=05114000&format=waterml,1.1&ParameterCd=00060&startDT=2014-10-10&endDT=2014-10-10")
  # nolint end
  site <- "04087170"
  pCode <- "63680"
  startDate <- "2012-07-10"
  endDate <- "2012-07-17"
  dd_2 <- readNWISuv(site, pCode, startDate, endDate)
  expect_true(any(names(dd_2) %in% c(
    "agency_cd", "site_no",
    "dateTime",
    "X_63680_00000", "X_63680_00000_cd",
    "X_.YSI.6136.UP._63680_00000",
    "X_.YSI.6136.UP._63680_00000_cd",
    "tz_cd"
  )))
})

context("Peak, rating, meas, site")
test_that("peak, rating curves, surface-water measurements", {
  testthat::skip_on_cran()

  siteNumbers <- c("01594440", "040851325")
  data <- readNWISpeak(siteNumbers)
  expect_is(data$agency_cd, "character")

  # Rating curvs:
  siteNumber <- "01594440"
  data <- readNWISrating(siteNumber, "base")
  expect_that(length(attr(data, "RATING")), equals(7))

  # Surface meas:
  siteNumbers <- c("01594440", "040851325")
  data <- readNWISmeas(siteNumbers)
  expect_is(data$agency_cd, "character")

  siteINFO <- readNWISsite("05114000")
  expect_is(siteINFO$agency_cd, "character")
  expect_equal(siteINFO$site_no, "05114000")

  siteINFOMulti <- readNWISsite(c("05114000", "09423350"))
  expect_true(nrow(siteINFOMulti) == 2)

  Meas07227500.ex <- readNWISmeas("07227500", expanded = TRUE)
  expect_is(Meas07227500.ex$measurement_dt, "Date")
  expect_is(Meas07227500.ex$measurement_dateTime, "POSIXct")

  expect_equal(nrow(whatNWISdata(siteNumber = "10312000", parameterCd = "50286")), 0)
  expect_equal(ncol(whatNWISdata(siteNumber = "10312000", parameterCd = "50286")), 24)

  url <- "https://waterservices.usgs.gov/nwis/site/?format=rdb&seriesCatalogOutput=true&sites=05114000"
  x <- importRDB1(url)

  siteID <- "263819081585801"
  gwl_1 <- readNWISgwl(siteID)
  expect_equal(unique(gwl_1$site_no), siteID)
})

test_that("NWIS dv tests", {
  testthat::skip_on_cran()

  siteNumber <- "04085427"
  startDate <- "2012-01-01"
  endDate <- "2012-06-30"
  pCode <- "00060"

  rawDailyQ <- readNWISdv(siteNumber, pCode, startDate, endDate)
  expect_is(rawDailyQ$Date, "Date")

  rawDailyQAndTempMeanMax <- readNWISdv(siteNumber, c("00010", "00060"),
    startDate, endDate,
    statCd = c("00001", "00003")
  )
  expect_true(length(grep("00060", names(rawDailyQAndTempMeanMax))) >= 2 &
    length(grep("00010", names(rawDailyQAndTempMeanMax))) >= 2)


  rawDailyMultiSites <- readNWISdv(c("01491000", "01645000"),
    c("00010", "00060"),
    startDate, endDate,
    statCd = c("00001", "00003")
  )
  expect_true(length(unique(rawDailyMultiSites$site_no)) > 1)

  site <- "05212700"
  notActive <- readNWISdv(site, "00060", "2014-01-01", "2014-01-07")
  expect_true(nrow(notActive) == 0)
})

test_that("WQP qw tests", {
  testthat::skip_on_cran()
  nameToUse <- "Specific conductance"
  pcodeToUse <- "00095"

  INFO_WQP <- readWQPqw("USGS-04024315", pcodeToUse, startDate = "", endDate = "")
  expect_is(INFO_WQP$ActivityStartDateTime, "POSIXct")

  INFO2 <- readWQPqw("WIDNR_WQX-10032762", nameToUse, startDate = "", endDate = "")
  expect_is(INFO2$ActivityStartDateTime, "POSIXct")
})

context("readNWISstat tests")
test_that("readNWISstat tests", {
  testthat::skip_on_cran()
  data <- readNWISstat(
    siteNumbers = c("02171500"),
    parameterCd = c("00010", "00060"),
    statReportType = "daily",
    statType = c("mean", "p75", "p25"),
    startDate = "2000",
    endDate = "2010"
  )
  expect_is(data$begin_yr, "numeric")
  expect_true(length(data) > 3)

  monthData <- readNWISstat(
    siteNumbers = c("02171500"),
    parameterCd = c("00010", "00060"),
    statReportType = "monthly",
    startDate = "2000",
    endDate = "2010"
  )
  expect_is(monthData$mean_va, "numeric")

  annualData <- readNWISstat(
    siteNumbers = c("02171500"),
    parameterCd = c("00010", "00060"),
    statReportType = "annual",
    startDate = "2000",
    endDate = "2010"
  )
  expect_gt(nrow(annualData), 2)
})

context("readNWISuse tests")
test_that("readNWISuse tests", {
  testthat::skip_on_cran()
  dc <- readNWISuse(
    years = c(2000, 2005, 2010),
    stateCd = "DC", countyCd = NULL
  )
  expect_true(nrow(dc) == 3)
  expect_is(dc$state_cd, "character")

  ohio <- readNWISuse(
    years = 2005,
    stateCd = "OH",
    countyCd = "ALL"
  )
  expect_true(nrow(ohio) == 88)

  twoCounties <- readNWISuse(
    years = 2010,
    stateCd = "PA",
    countyCd = c("Cambria", "Indiana")
  )
  expect_true(nrow(twoCounties) == 2)
})

context("state tests")
test_that("state county tests", {
  testthat::skip_on_cran()

  fullName <- stateCdLookup("wi", "fullName")
  expect_equal(fullName, "Wisconsin")

  abbriev <- stateCdLookup("Wisconsin", "postal")
  expect_equal(abbriev, "WI")
  id <- stateCdLookup("WI", "id")
  expect_equal(id, 55)
  name <- stateCdLookup(55, "fullName")
  expect_equal(name, "Wisconsin")
  multipleStates <- stateCdLookup(c("West Virginia", "Wisconsin", 55, "MN"))
  expect_equal(multipleStates, c("WV", "WI", "WI", "MN"))

  id <- countyCdLookup(state = "WI", county = "Dane")
  expect_equal(id, "025")
  name <- countyCdLookup(state = "OH", county = 13, output = "fullName")
  expect_equal(name, "Belmont County")
  index <- countyCdLookup(state = "Pennsylvania", county = "ALLEGHENY COUNTY", output = "tableIndex")
  expect_equal(index, 2258)
  fromIDs <- countyCdLookup(state = 13, county = 5, output = "fullName")
  expect_equal(fromIDs, "Bacon County")
  counties <- c("51001", "51003")
  already_ready <- countyCdLookup(county = counties)
  expect_equal(counties, already_ready)
})

context("water year column")

df_test <- data.frame(
  site_no = as.character(1:13),
  dateTime = seq(as.Date("2010-01-01"),
    as.Date("2011-01-31"),
    by = "months"
  ),
  result_va = 1:13, stringsAsFactors = FALSE
)

test_that("addWaterYear works with Date, POSIXct, character, but breaks with numeric", {
  testthat::skip_on_cran()

  df_date <- df_test
  df_date_wy <- addWaterYear(df_date)
  expect_equal(ncol(df_date_wy), ncol(df_date) + 1)
  df_posixct <- df_test
  df_posixct$dateTime <- as.POSIXct(df_posixct$dateTime)
  df_posixct_wy <- addWaterYear(df_posixct)
  expect_equal(ncol(df_posixct_wy), ncol(df_posixct) + 1)
  df_char <- df_test
  df_char$dateTime <- as.character(df_char$dateTime)
  df_char_wy <- addWaterYear(df_char)
  expect_equal(ncol(df_char_wy), ncol(df_char) + 1)
  df_num <- df_test
  df_num$dateTime <- as.numeric(df_num$dateTime)
  dfnum2 <- addWaterYear(df_num)
  expect_equal(dfnum2$waterYear, rep(NA, 13))
})

test_that("addWaterYear works for each column name", {
  testthat::skip_on_cran()
  nwisqw_style <- df_test
  nwisqw_style_wy <- addWaterYear(nwisqw_style)
  expect_equal(ncol(nwisqw_style_wy), ncol(nwisqw_style) + 1)

  nwisdata_style <- df_test
  names(nwisdata_style)[2] <- "Date"
  nwisdata_style_wy <- addWaterYear(nwisdata_style)
  expect_equal(ncol(nwisdata_style_wy), ncol(nwisdata_style) + 1)

  wqp_style <- df_test
  names(wqp_style)[2] <- "ActivityStartDate"
  wqp_style[["ActivityEndDate"]] <- wqp_style[["ActivityStartDate"]]
  wqp_style_wy <- addWaterYear(wqp_style)
  expect_equal(ncol(wqp_style_wy), ncol(wqp_style) + 2)

  userspecified_style <- df_test
  names(userspecified_style)[2] <- "MyDateCol"
  expect_error(
    addWaterYear(userspecified_style),
    "specified date column does not exist in supplied data frame"
  )
})

test_that("addWaterYear correctly calculates the WY and is numeric", {
  testthat::skip_on_cran()
  df_test_wy <- addWaterYear(df_test)
  expect_is(df_test_wy[["waterYear"]], "numeric")
  expect_true(all(df_test_wy[["waterYear"]][1:9] == 2010))
  expect_true(all(df_test_wy[["waterYear"]][10:13] == 2011))
})

test_that("addWaterYear adds column next to dateTime", {
  testthat::skip_on_cran()
  df_test_wy <- addWaterYear(df_test)
  dateTime_col <- which(names(df_test_wy) == "dateTime")
  expect_equal(names(df_test_wy)[dateTime_col + 1], "waterYear")
})

test_that("addWaterYear can be used with pipes", {
  testthat::skip_on_cran()

  df_test_wy <- df_test
  df_test_wy <- addWaterYear(df_test_wy)
  expect_equal(ncol(df_test_wy), ncol(df_test) + 1)
})

test_that("addWaterYear doesn't add another WY column if it exists", {
  testthat::skip_on_cran()
  df_test_wy <- addWaterYear(df_test)
  expect_equal(ncol(df_test_wy), ncol(df_test) + 1)
  df_test_wy2 <- addWaterYear(df_test_wy)
  expect_equal(ncol(df_test_wy2), ncol(df_test_wy))
})

test_that("calcWaterYear can handle missing values", {
  dateVec <- seq(as.Date("2010-01-01"), as.Date("2011-01-31"), by = "months")
  dateVec[c(3, 7, 12)] <- NA
  wyVec <- dataRetrieval:::calcWaterYear(dateVec)

  expect_is(wyVec, "numeric")
  expect_true(all(is.na(wyVec[c(3, 7, 12)])))
})


context("Construct NWIS urls")
test_that("Construct NWIS urls", {
  testthat::skip_on_cran()

  siteNumber <- "01594440"
  startDate <- "1985-01-01"
  endDate <- ""
  pCode <- c("00060", "00010")
  url_daily <- constructNWISURL(siteNumber, pCode,
    startDate, endDate, "dv",
    statCd = c("00003", "00001")
  )
  # nolint start: line_length_linter
  expect_equal(url_daily, "https://waterservices.usgs.gov/nwis/dv/?site=01594440&format=waterml,1.1&ParameterCd=00060,00010&StatCd=00003,00001&startDT=1985-01-01")

  url_unit <- constructNWISURL(siteNumber, pCode, "2012-06-28", "2012-06-30", "iv")
  expect_equal(
    url_unit,
    "https://nwis.waterservices.usgs.gov/nwis/iv/?site=01594440&format=waterml,1.1&ParameterCd=00060,00010&startDT=2012-06-28&endDT=2012-06-30"
  )

  url_qw_single <- constructNWISURL(siteNumber, "01075", startDate, endDate, "qw")
  expect_equal(url_qw_single, "https://nwis.waterdata.usgs.gov/nwis/qwdata?search_site_no=01594440&search_site_no_match_type=exact&multiple_parameter_cds=01075&param_cd_operator=AND&list_of_search_criteria=search_site_no,multiple_parameter_cds&group_key=NONE&sitefile_output_format=html_table&column_name=agency_cd&column_name=site_no&column_name=station_nm&inventory_output=0&rdb_inventory_output=file&TZoutput=0&pm_cd_compare=Greater%20than&radio_parm_cds=previous_parm_cds&qw_attributes=0&format=rdb&rdb_qw_attributes=expanded&date_format=YYYY-MM-DD&rdb_compression=value&qw_sample_wide=0&begin_date=1985-01-01")

  url_qw <- constructNWISURL(
    siteNumber, c("01075", "00029", "00453"),
    startDate, endDate, "qw"
  )
  expect_equal(url_qw, "https://nwis.waterdata.usgs.gov/nwis/qwdata?search_site_no=01594440&search_site_no_match_type=exact&multiple_parameter_cds=01075,00029,00453&param_cd_operator=OR&list_of_search_criteria=search_site_no,multiple_parameter_cds&group_key=NONE&sitefile_output_format=html_table&column_name=agency_cd&column_name=site_no&column_name=station_nm&inventory_output=0&rdb_inventory_output=file&TZoutput=0&pm_cd_compare=Greater%20than&radio_parm_cds=previous_parm_cds&qw_attributes=0&format=rdb&rdb_qw_attributes=expanded&date_format=YYYY-MM-DD&rdb_compression=value&qw_sample_wide=0&begin_date=1985-01-01")

  url_daily_tsv <- constructNWISURL(siteNumber, pCode, startDate, endDate, "dv",
    statCd = c("00003", "00001"), format = "tsv"
  )
  expect_equal(url_daily_tsv, "https://waterservices.usgs.gov/nwis/dv/?site=01594440&format=rdb,1.0&ParameterCd=00060,00010&StatCd=00003,00001&startDT=1985-01-01")

  setAccess("internal")
  url_rating <- constructNWISURL(siteNumber, service = "rating", ratingType = "base")
  expect_equal(url_rating, "https://waterdata.usgs.gov/nwisweb/get_ratings/?Access=3&site_no=01594440&file_type=base")
  url_peak <- constructNWISURL(siteNumber, service = "peak")
  expect_equal(url_peak, "https://nwis.waterdata.usgs.gov/usa/nwis/peak/?Access=3&site_no=01594440&range_selection=date_range&format=rdb")

  url_meas <- constructNWISURL(siteNumber, service = "meas")
  expect_equal(url_meas, "https://waterdata.usgs.gov/nwis/measurements/?Access=3&site_no=01594440&range_selection=date_range&format=rdb_expanded")

  urlQW <- constructNWISURL("450456092225801", "70300",
    startDate = "", endDate = "",
    "qw", expanded = TRUE
  )
  expect_equal(urlQW, "https://nwis.waterdata.usgs.gov/nwis/qwdata?Access=3&search_site_no=450456092225801&search_site_no_match_type=exact&multiple_parameter_cds=70300&param_cd_operator=AND&list_of_search_criteria=search_site_no,multiple_parameter_cds&group_key=NONE&sitefile_output_format=html_table&column_name=agency_cd&column_name=site_no&column_name=station_nm&inventory_output=0&rdb_inventory_output=file&TZoutput=0&pm_cd_compare=Greater%20than&radio_parm_cds=previous_parm_cds&qw_attributes=0&format=rdb&rdb_qw_attributes=expanded&date_format=YYYY-MM-DD&rdb_compression=value&qw_sample_wide=0")

  setAccess("cooperator")
  urlQW <- constructNWISURL("450456092225801", "70300",
    startDate = "", endDate = "", "qw", expanded = TRUE
  )
  expect_equal(urlQW, "https://nwis.waterdata.usgs.gov/nwis/qwdata?Access=1&search_site_no=450456092225801&search_site_no_match_type=exact&multiple_parameter_cds=70300&param_cd_operator=AND&list_of_search_criteria=search_site_no,multiple_parameter_cds&group_key=NONE&sitefile_output_format=html_table&column_name=agency_cd&column_name=site_no&column_name=station_nm&inventory_output=0&rdb_inventory_output=file&TZoutput=0&pm_cd_compare=Greater%20than&radio_parm_cds=previous_parm_cds&qw_attributes=0&format=rdb&rdb_qw_attributes=expanded&date_format=YYYY-MM-DD&rdb_compression=value&qw_sample_wide=0")

  setAccess("USGS")
  urlQW <- constructNWISURL("450456092225801", "70300",
    startDate = "",
    endDate = "", "qw", expanded = TRUE
  )
  expect_equal(urlQW, "https://nwis.waterdata.usgs.gov/nwis/qwdata?Access=2&search_site_no=450456092225801&search_site_no_match_type=exact&multiple_parameter_cds=70300&param_cd_operator=AND&list_of_search_criteria=search_site_no,multiple_parameter_cds&group_key=NONE&sitefile_output_format=html_table&column_name=agency_cd&column_name=site_no&column_name=station_nm&inventory_output=0&rdb_inventory_output=file&TZoutput=0&pm_cd_compare=Greater%20than&radio_parm_cds=previous_parm_cds&qw_attributes=0&format=rdb&rdb_qw_attributes=expanded&date_format=YYYY-MM-DD&rdb_compression=value&qw_sample_wide=0")

  setAccess("public")
  urlQW <- constructNWISURL("450456092225801", "70300",
    startDate = "",
    endDate = "", "qw", expanded = TRUE
  )
  expect_equal(urlQW, "https://nwis.waterdata.usgs.gov/nwis/qwdata?search_site_no=450456092225801&search_site_no_match_type=exact&multiple_parameter_cds=70300&param_cd_operator=AND&list_of_search_criteria=search_site_no,multiple_parameter_cds&group_key=NONE&sitefile_output_format=html_table&column_name=agency_cd&column_name=site_no&column_name=station_nm&inventory_output=0&rdb_inventory_output=file&TZoutput=0&pm_cd_compare=Greater%20than&radio_parm_cds=previous_parm_cds&qw_attributes=0&format=rdb&rdb_qw_attributes=expanded&date_format=YYYY-MM-DD&rdb_compression=value&qw_sample_wide=0")

  url_use <- constructUseURL(
    years = c(1990, 1995),
    stateCd = "Ohio",
    countyCd = c(1, 3),
    categories = "ALL"
  )
  expect_equal(url_use, "https://waterdata.usgs.gov/OH/nwis/water_use?format=rdb&rdb_compression=value&wu_area=county&wu_county=1%2C3&wu_year=1990%2C1995&wu_category=ALL")
  # nolint end
})

context("Construct WQP urls")
test_that("Construct WQP urls", {
  testthat::skip_on_cran()

  site_id <- "01594440"
  startDate <- "1985-01-01"
  endDate <- ""
  pCode <- c("00060", "00010")
  url_wqp <- constructWQPURL(
    paste("USGS", site_id, sep = "-"),
    c("01075", "00029", "00453"),
    startDate, endDate
  )
  # nolint start: line_length_linter
  expect_equal(
    url_wqp,
    "https://www.waterqualitydata.us/data/Result/search?siteid=USGS-01594440&pCode=01075;00029;00453&startDateLo=01-01-1985&mimeType=tsv&zip=yes"
  )

  # Multiple characteristicNames
  charNames <- c(
    "Temperature", "Temperature, sample", "Temperature, water",
    "Temperature, water, deg F"
  )
  obs_url_orig <- constructWQPURL(
    siteNumbers = c("IIDFG-41WSSPAHS", "USGS-02352560"),
    parameterCd = c("Temperature", "Temperature, sample", "Temperature, water", "Temperature, water, deg F"),
    "", ""
  )
  expect_equal(
    obs_url_orig,
    "https://www.waterqualitydata.us/data/Result/search?siteid=IIDFG-41WSSPAHS;USGS-02352560&characteristicName=Temperature;Temperature%2C%20sample;Temperature%2C%20water;Temperature%2C%20water%2C%20deg%20F&mimeType=tsv&zip=yes"
  )

  # nolint end
})

context("Construct WQP urls")
test_that("Construct WQP urls", {
  siteNumber <- "01594440"
  startDate <- "1985-01-01"
  endDate <- ""
  pCode <- c("00060", "00010")
  url_wqp <- constructWQPURL(
    paste("USGS", siteNumber, sep = "-"),
    c("01075", "00029", "00453"),
    startDate, endDate
  )
  # nolint start: line_length_linter
  expect_equal(
    url_wqp,
    "https://www.waterqualitydata.us/data/Result/search?siteid=USGS-01594440&pCode=01075;00029;00453&startDateLo=01-01-1985&mimeType=tsv&zip=yes"
  )

  rawSampleURL_Zip <- constructWQPURL("USGS-01594440", "01075", "", "", TRUE)
  expect_equal(
    rawSampleURL_Zip,
    "https://www.waterqualitydata.us/data/Result/search?siteid=USGS-01594440&pCode=01075&mimeType=tsv&zip=yes"
  )
  # nolint end
})


context("pCode Stuff")
test_that("pCode Stuff", {
  testthat::skip_on_cran()

  paramINFO <- readNWISpCode(c("00060", "01075", "00931", NA))
  expect_equal(nrow(paramINFO), 4)
  expect_equal(paramINFO$parameter_cd, c("00060", "01075", "00931", NA))
  
  # pcode 12345 isn't a valid code:
  expect_warning(paramINFO <- readNWISpCode(c("12345")))
  expect_warning(paramINFO <- readNWISpCode(c("00060", "01075",
                                              "12345", NA_character_)))
  expect_equal(nrow(paramINFO), 4)
  expect_equal(paramINFO$parameter_cd, c("00060", "01075", 
                                         "12345", NA_character_))
  
  expect_equal(paramINFO$parameter_group_nm[3:4], c(NA_character_, NA_character_))

  paramINFO <- readNWISpCode("all")
  expect_true(nrow(paramINFO) > 24000)
  expect_equal(
    attr(paramINFO, "url"),
    "https://help.waterdata.usgs.gov/code/parameter_cd_query?fmt=rdb&group_cd=%"
  )
})

context("pCode Name Stuff")
test_that("pCode Stuff", {
  testthat::skip_on_cran()
  
  paramINFO <- pcode_to_name(c("00060", "01075", "00931", NA))
  expect_equal(nrow(paramINFO), 4)
  expect_equal(paramINFO$parm_cd, c("00060", "01075", "00931", NA))
  
  # pcode 12345 isn't a valid code:
  expect_warning(paramINFO <- pcode_to_name(c("12345")))
  expect_warning(paramINFO <- pcode_to_name(c("00060", "01075",
                                              "12345", NA_character_)))
  expect_equal(nrow(paramINFO), 4)
  expect_equal(paramINFO$parm_cd, c("00060", "01075", 
                                    "12345", NA_character_))
  
  expect_equal(paramINFO$description[3:4], c(NA_character_, NA_character_))
  
  paramINFO <- pcode_to_name("all")
  expect_true(nrow(paramINFO) > 19000)
  expect_equal(
    attr(paramINFO, "url"),
    "https://www.waterqualitydata.us/Codes/public_srsnames/?mimeType=json"
  )
})

