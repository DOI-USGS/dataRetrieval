context("Unit values")

test_that("Unit value data returns correct types", {
  testthat::skip_on_cran()
  
  siteNumber <- '05114000'
  parameterCd <- '00060'
  startDate <- "2014-10-10"
  endDate <- "2014-10-10"

  rawData <- readNWISuv(siteNumber,parameterCd,startDate,endDate)
  rawData <- renameNWISColumns(rawData)
  timeZoneChange <- readNWISuv(c('04024430','04024000'),parameterCd,
                               "2013-11-03","2013-11-03", 
                               tz="America/Chicago")
  
  
  timeZoneChange <- renameNWISColumns(timeZoneChange)
  expect_is(rawData$dateTime, 'POSIXct')
  expect_is(rawData$Flow_Inst, 'numeric')
  expect_that(attr(rawData, "url"), equals(
      "http://nwis.waterservices.usgs.gov/nwis/iv/?site=05114000&format=waterml,1.1&ParameterCd=00060&startDT=2014-10-10&endDT=2014-10-10")
  )
  #First switchover to standard time:
  expect_that(as.numeric(timeZoneChange[which(timeZoneChange$tz_cd == "CST")[1],"dateTime"]),
              equals(as.numeric(as.POSIXct("2013-11-03 01:00:00", tz="UTC")+60*60*6)))
  
})

context("Peak, rating, meas")
test_that("peak, rating curves, surface-water measurements", {
  testthat::skip_on_cran()
  
  siteNumbers <- c('01594440','040851325')
  data <- readNWISpeak(siteNumbers)
  expect_is(data$agency_cd, 'character')
  
  #Rating curvs:
  siteNumber <- '01594440'
  data <- readNWISrating(siteNumber, "base")
  expect_that(length(attr(data, "RATING")),equals(7))
  
  #Surface meas:
  siteNumbers <- c('01594440','040851325')
  data <- readNWISmeas(siteNumbers)
  expect_is(data$agency_cd, 'character')
})

context("qw")

test_that("NWIS qw tests", {
  testthat::skip_on_cran()
  siteNumbers <- c('04024430','04024000')
  startDate <- '2010-01-01'
  endDate <- ''
  parameterCd <- c('34247','30234','32104','34220')
  
  rawNWISqwData <- readNWISqw(siteNumbers,parameterCd,startDate,endDate)
  expect_is(rawNWISqwData$startDateTime, 'POSIXct')
  
  rawNWISqwDataReshaped <- readNWISqw(siteNumbers,parameterCd,
            startDate,endDate,reshape=TRUE)
  expect_is(rawNWISqwDataReshaped$startDateTime, 'POSIXct')
  
  parameterCd <- "all"
  rawNWISall <- readNWISqw(siteNumbers,parameterCd,
           startDate,"2011-01-01",reshape=TRUE)
  expect_is(rawNWISall$startDateTime, 'POSIXct')
  
  pgroup <- c("NUT")
  rawNWISNutrients <- readNWISqw(siteNumbers,pgroup,
           startDate,endDate)
  rawNWISOpe <- readNWISqw(siteNumbers,"OPE",
                           startDate,endDate) 
  
  groups <- c("NUT","OPE")
  rawNWISNutOpe <- readNWISqw(siteNumbers,groups,
           startDate,endDate) 

  
})