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
#   #First switchover to standard time:
#   expect_that(as.numeric(timeZoneChange[which(timeZoneChange$tz_cd == "America/Chicago")[1],"dateTime"]),
#               equals(as.numeric(as.POSIXct("2013-11-03 01:00:00", tz="UTC")+60*60*6)))
  
})

context("Peak, rating, meas, site")
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
  
  siteINFO <- readNWISsite('05114000')
  expect_is(siteINFO$agency_cd, 'character')
  
  siteINFOMulti <- readNWISsite(c('05114000','09423350'))
  expect_that(nrow(siteINFOMulti) == 2, is_true())
  
  Meas07227500.ex <- readNWISmeas("07227500",expanded=TRUE)
  expect_is(Meas07227500.ex$measurement_dt, 'Date')
  expect_is(Meas07227500.ex$measurement_dateTime, 'POSIXct')
  
  emptyDF <- whatNWISdata("10312000",parameterCd = "50286")
  expect_that(nrow(emptyDF) == 0, is_true())
  
  url <- "http://waterservices.usgs.gov/nwis/site/?format=rdb&seriesCatalogOutput=true&sites=05114000"
  x <- importRDB1(url)

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
  expect_is(rawNWISNutrients$startDateTime, 'POSIXct')
  
  qwret <- readNWISqw("413437087150601", parameterCd = c("NUT","INN"),startDate = "",endDate = "")
  expect_that(nrow(qwret) == 0, is_true())
  
  siteNumber <- '455638089034501'
  wy_start <- paste0(2014, "-10-01")
  wy_end <- paste0(2015, "-09-30")
  
  #No data test:
  no.data <- readNWISqw(siteNumbers = siteNumber, 
                  parameterCd = '00060', 
                  startDate = wy_start, 
                  endDat = wy_end)
  
  expect_that(nrow(no.data) == 0, is_true())
  
})

context("dv")

test_that("NWIS dv tests", {
  testthat::skip_on_cran()
  
  siteNumber <- '04085427'
  startDate <- '2012-01-01'
  endDate <- '2012-06-30'
  pCode <- '00060'
  
  rawDailyQ <- readNWISdv(siteNumber,pCode, startDate, endDate)
  expect_is(rawDailyQ$Date, 'Date')
  
  rawDailyQAndTempMeanMax <- readNWISdv(siteNumber,c('00010','00060'),
        startDate, endDate, statCd=c('00001','00003'))
  expect_that(length(grep("00060", names(rawDailyQAndTempMeanMax))) >= 2 & 
                length(grep("00010", names(rawDailyQAndTempMeanMax))) >= 2, is_true())
  

  rawDailyMultiSites<- readNWISdv(c("01491000","01645000"),c('00010','00060'),
        startDate, endDate, statCd=c('00001','00003'))
  expect_that(length(unique(rawDailyMultiSites$site_no)) > 1, is_true())
  
#   # Site with no data:
#   x <- readNWISdv("10258500","00060", "2015-02-08", "2015-02-14")
#   expect_that(sum(is.na(x$X_00060_00003)) > 0, is_true())
  
  site <- "05212700"
  notActive <- readNWISdv(site, "00060", "2014-01-01","2014-01-07")
  expect_that(nrow(notActive) == 0, is_true())
})

test_that("WQP qw tests", {
  testthat::skip_on_cran()
  nameToUse <- 'Specific conductance'
  pcodeToUse <- '00095'
  
  INFO_WQP <- readWQPqw('USGS-04024315',pcodeToUse, startDate = "", endDate = "")
  expect_is(INFO_WQP$ActivityStartDateTime, 'POSIXct')
  
  INFO2 <- readWQPqw('WIDNR_WQX-10032762',nameToUse, startDate = "", endDate = "")
  expect_is(INFO2$ActivityStartDateTime, 'POSIXct')
  
})

context("readNWISstat tests")
test_that("readNWISstat tests", {
  testthat::skip_on_cran()
  data <- readNWISstat(siteNumbers=c("02171500"),parameterCd=c("00010","00060"),
                    statReportType="daily",statType=c("mean","p75","p25"),startDate="2000",endDate="2010")
  expect_is(data$begin_yr, 'integer')
  expect_that(length(data) > 3, is_true())
  
  monthData <- readNWISstat(siteNumbers=c("02171500"),parameterCd=c("00010","00060"),
                                                  statReportType="monthly",startDate="2000",endDate="2010")
  expect_is(monthData$mean_va, 'numeric')
  
  annualData <- readNWISstat(siteNumbers=c("02171500"),parameterCd=c("00010","00060"),statReportType="annual",
                             startDate="2000",endDate="2010")
  expect_gt(nrow(annualData),2)
})

context("readNWISuse tests")
test_that("readNWISuse tests", {
  testthat::skip_on_cran()
  dc <- readNWISuse(years=c(2000,2005,2010),stateCd = "DC", countyCd = NULL)
  expect_that(nrow(dc)==3, is_true())
  expect_is(dc$state_cd, 'character')
  
  ohio <- readNWISuse(years=2005,stateCd="OH",countyCd="ALL")
  expect_that(nrow(ohio)==88, is_true())
  
  twoCounties <- readNWISuse(years=2010,stateCd="PA",countyCd=c("Cambria","Indiana"))
  expect_that(nrow(twoCounties)==2, is_true())
})

context("state tests")
test_that("state county tests",{
  fullName <- stateCdLookup("wi", "fullName")
  expect_equal(fullName, "Wisconsin")
  
  abbriev <- stateCdLookup("Wisconsin", "postal")
  expect_equal(abbriev, "WI")
  id <- stateCdLookup("WI", "id")
  expect_equal(id, 55)
  name <- stateCdLookup(55, "fullName")
  expect_equal(name, "Wisconsin")
  multipleStates <- stateCdLookup(c("West Virginia", "Wisconsin", 55, "MN"))
  expect_equal(multipleStates, c("WV","WI","WI","MN"))
  
  id <- countyCdLookup(state = "WI", county = "Dane")
  expect_equal(id, "025")
  name <- countyCdLookup(state = "OH", county = 13, output = "fullName")
  expect_equal(name, "Belmont County")
  index <- countyCdLookup(state = "Pennsylvania", county = "ALLEGHENY COUNTY", output = "tableIndex")
  expect_equal(index, 2246)
  fromIDs <- countyCdLookup(state = 13, county = 5, output = "fullName")
  expect_equal(fromIDs, "Bacon County")
})
