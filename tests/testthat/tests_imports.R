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

  url2 <- constructWQPURL("USGS-01594440", "01075", "", "", legacy = TRUE)
  rawSample2 <- suppressWarnings(importWQP(url2))
  expect_is(rawSample2$ActivityStartDateTime, "POSIXct")
})
