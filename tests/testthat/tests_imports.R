context("importRDB_noCRAN")

test_that("External importRDB1 tests", {
  testthat::skip_on_cran()

  siteNumber <- "02177000"
  startDate <- "2012-09-01"
  endDate <- "2012-10-01"
  offering <- "00003"
  property <- "00060"

  obs_url <- constructNWISURL(siteNumber, property,
    startDate, endDate, "dv",
    format = "tsv"
  )
  data <- importRDB1(obs_url)
  expect_is(data$datetime, "Date")

  urlMultiPcodes <- constructNWISURL("04085427", c("00060", "00010"),
    startDate, endDate, "dv",
    statCd = c("00003", "00001"), "tsv"
  )
  multiData <- importRDB1(urlMultiPcodes)
  pCodeCols <- grep("X", colnames(multiData))
  expect_true(length(pCodeCols) / 2 > 2)

  unitDataURL <- constructNWISURL(siteNumber, property,
    "2013-11-03", "2013-11-03", "uv",
    format = "tsv"
  ) # includes timezone switch
  unitData <- importRDB1(unitDataURL, asDateTime = TRUE)

  # Need to think of a way to automatically check timezone conversion:
  #   expect_that(as.numeric(unitData[which(unitData$tz_cd == "EST")[1],"datetime"]),
  #               equals(as.numeric(as.POSIXct("2013-11-03 01:00:00", tz="UTC")+60*60*5)))

  site <- "05427850"

})

context("importRDB")
test_that("CRAN-friendly importRDB test", {
  filePath <- system.file("extdata", package = "dataRetrieval")
  fileName <- "RDB1Example.txt"
  fullPath <- file.path(filePath, fileName)
  importUserRDB <- importRDB1(fullPath)

  # default is to turn dates to characters
  expect_is(importUserRDB$datetime, "Date")
})

context("importWaterML1")
test_that("CRAN-friendly importWaterML1 test", {
  filePath <- system.file("extdata", package = "dataRetrieval")
  fileName <- "WaterML1Example.xml"
  fullPath <- file.path(filePath, fileName)
  importUserWML1 <- importWaterML1(fullPath, asDateTime = TRUE)
  # default is to turn dates to characters
  expect_is(importUserWML1$dateTime, "POSIXct")
})

test_that("External importWaterML1 test", {
  testthat::skip_on_cran()

  siteNumber <- "02177000"
  startDate <- "2012-09-01"
  endDate <- "2012-10-01"
  offering <- "00003"
  property <- "00060"
  obs_url <- constructNWISURL(siteNumber, property, startDate, endDate, "dv")

  data <- importWaterML1(obs_url, TRUE)
  expect_is(data$dateTime, "POSIXct")

  groundWaterSite <- "431049071324301"
  startGW <- "2013-10-01"
  endGW <- "2014-06-30"
  groundwaterExampleURL <- constructNWISURL(groundWaterSite, NA,
    startGW, endGW,
    service = "gwlevels"
  )
  groundWater <- importRDB1(groundwaterExampleURL)

  expect_is(groundWater$lev_dt, "Date")

  unitDataURL <- constructNWISURL(
    siteNumber, property,
    "2020-10-30", "2020-11-01", "uv"
  )
  unitData <- importWaterML1(unitDataURL, TRUE)
  expect_is(unitData$dateTime, "POSIXct")

  # Two sites, two pcodes, one site has two data descriptors
  siteNumber <- c("01480015", "04085427") # one site seems to have lost it"s 2nd dd
  obs_url <- constructNWISURL(
    siteNumber, c("00060", "00010"),
    startDate, endDate, "dv"
  )
  data <- importWaterML1(obs_url)

  expect_true(length(unique(data$site_no)) == 2)
  expect_true(ncol(data) == 8) # 3 data, 3 remark codes, and 4 (agency, site, dateTime, tz)

  inactiveSite <- "05212700"
  inactiveSite <- constructNWISURL(
    inactiveSite, "00060",
    "2014-01-01", "2014-01-10", "dv"
  )
  inactiveSite <- importWaterML1(inactiveSite)
  expect_true(nrow(inactiveSite) == 0)

  inactiveAndActive <- c("07334200", "05212700")
  inactiveAndActive <- constructNWISURL(
    inactiveAndActive, "00060",
    "2014-01-01", "2014-12-31", "dv"
  )
  inactiveAndActive <- importWaterML1(inactiveAndActive)
  #
  # The inactive site became active, need a new test.

  # raw XML
  url <- constructNWISURL(
    service = "dv", siteNumber = "02319300", parameterCd = "00060",
    startDate = "2014-01-01", endDate = "2014-01-01"
  )
  raw <- httr2::req_perform(url)
  raw <- httr2::resp_body_xml(raw)
  rawParsed <- importWaterML1(raw)
  expect_true(nrow(rawParsed) > 0)
  expect_true(data.class(rawParsed$X_00060_00003) == "numeric")

  # no data
  url <- constructNWISURL("05212700", "00060", "2014-01-01", "2014-01-10", "dv", statCd = "00001")
  noData <- importWaterML1(url)
  expect_true(class(attr(noData, "url")) == "character")
  expect_true(all(dim(noData) == c(0, 4)))

  url <- constructNWISURL(
    service = "iv", site = c("02319300", "02171500"),
    startDate = "2015-04-04", endDate = "2015-04-05"
  )
  data <- importWaterML1(url, tz = "America/New_York", asDateTime = TRUE)
  expect_true(data.class(data$dateTime) == "POSIXct")
  expect_true(nrow(data) > 0)

  expect_error(readNWISdata(
    sites = "05114000",
    service = "iv",
    parameterCd = "00060",
    startDate = "2014-05-01T00:00",
    endDate = "2014-05-01T12:00",
    tz = "blah"
  ))

  arg.list <- list(
    sites = "05114000",
    parameterCd = "00060",
    startDate = "2014-05-01T00:00",
    endDate = "2014-05-01T12:00"
  )

  chi_iv <- readNWISdata(arg.list,
    service = "iv",
    tz = "America/Chicago"
  )

  expect_true(all(chi_iv$tz_cd == "America/Chicago"))
  expect_equal(chi_iv$dateTime[1], as.POSIXct("2014-05-01T00:00",
    format = "%Y-%m-%dT%H:%M",
    tz = "America/Chicago"
  ))
  expect_equal(chi_iv$dateTime[nrow(chi_iv)], as.POSIXct("2014-05-01T12:00",
    format = "%Y-%m-%dT%H:%M",
    tz = "America/Chicago"
  ))

  # Time over daylight saving switch:
  tzURL <- constructNWISURL(
    "04027000", c("00300", "63680"),
    "2011-11-05", "2011-11-07", "uv"
  )
  tzIssue <- importWaterML1(tzURL,
    asDateTime = TRUE,
    tz = "America/Chicago"
  )
  expect_false(any(duplicated(tzIssue$dateTime)))
})

context("importWaterML2")

test_that("importWaterML2 internal test", {
  filePath <- system.file("extdata", package = "dataRetrieval")
  fileName <- "WaterML2Example.xml"
  xml_full <- xml2::read_xml(file.path(filePath, fileName))
  measurementTS <- xml2::xml_find_all(xml_full, "//wml2:MeasurementTimeseries")
  UserData <- importWaterML2(measurementTS)
  expect_is(UserData$value, "numeric")
})


context("importWQP_noCRAN")

test_that("External WQP tests", {
  testthat::skip_on_cran()

  rawSampleURL <- constructWQPURL("USGS-01594440", "01075", "", "", legacy = FALSE)
  # rawSample <- importWQP(rawSampleURL)
  # expect_is(rawSample$Activity_StartDateTime, "POSIXct")

  url2 <- constructWQPURL("USGS-01594440", "01075", "", "", legacy = TRUE)
  rawSample2 <- suppressWarnings(importWQP(url2))
  expect_is(rawSample2$ActivityStartDateTime, "POSIXct")

  STORETex <- constructWQPURL("WIDNR_WQX-10032762", "Specific conductance", "", "", legacy = FALSE)
  # STORETdata <- importWQP(STORETex)
  # expect_is(STORETdata$Activity_StartDateTime, "POSIXct")
})
