context("Unit values")

test_that("Unit value data returns correct types", {
  testthat::skip_on_cran()
  
  siteNumber <- '05114000'
  parameterCd <- '00060'
  startDate <- "2014-10-10"
  endDate <- "2014-10-10"

  rawData <- readNWISuv(siteNumber,parameterCd,startDate,endDate)
  rawData <- renameNWISColumns(rawData)
  
  spreadOver120 <- readNWISuv(siteNumber,parameterCd,
                              as.Date(Sys.Date()-200),
                              Sys.Date())
  
  expect_true(min(spreadOver120$dateTime) < as.POSIXct(Sys.Date(), tz="UTC"))
  
  timeZoneChange <- readNWISuv(c('04024430','04024000'),parameterCd,
                               "2013-11-03","2013-11-03", 
                               tz="America/Chicago")
  
  
  timeZoneChange <- renameNWISColumns(timeZoneChange)
  expect_is(rawData$dateTime, 'POSIXct')
  expect_is(rawData$Flow_Inst, 'numeric')
  expect_equal(attr(rawData, "url"), "https://nwis.waterservices.usgs.gov/nwis/iv/?site=05114000&format=waterml,1.1&ParameterCd=00060&startDT=2014-10-10&endDT=2014-10-10")
#   #First switchover to standard time:
#   expect_that(as.numeric(timeZoneChange[which(timeZoneChange$tz_cd == "America/Chicago")[1],"dateTime"]),
#               equals(as.numeric(as.POSIXct("2013-11-03 01:00:00", tz="UTC")+60*60*6)))
  
  site <- "04087170"
  pCode <- "63680"
  startDate <- "2012-07-10"
  endDate <- "2012-07-17"
  dd_2 <- readNWISuv(site, pCode, startDate, endDate)
  expect_true(all(names(dd_2) %in% c("agency_cd","site_no",                   
                                 "dateTime","X_.YSI.6136.UP._63680_00000",   
                                 "X_YSI.6136.DOWN_63680_00000","X_.YSI.6136.UP._63680_00000_cd",
                                 "X_YSI.6136.DOWN_63680_00000_cd","tz_cd")))
  
  noData <- readNWISuv("01196500","00010", "2016-06-15", "2016-06-15")
  # expect_equal(noData$X_00010_00000[1], as.numeric(NA))
  
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
  
  url <- "https://waterservices.usgs.gov/nwis/site/?format=rdb&seriesCatalogOutput=true&sites=05114000"
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

context("water year column")

df_test <- data.frame(site_no = as.character(1:13),
                      dateTime = seq(as.Date("2010-01-01"),as.Date("2011-01-31"), 
                                     by="months"),
                      result_va = 1:13, stringsAsFactors = FALSE)

test_that("addWaterYear works with Date, POSIXct, character, but breaks with numeric", {
  library(dplyr)
  
  df_date <- df_test
  df_date_wy <- addWaterYear(df_date)
  expect_equal(ncol(df_date_wy), ncol(df_date) + 1)
  
  df_posixct <- mutate(df_test, dateTime = as.POSIXct(dateTime))
  df_posixct_wy <- addWaterYear(df_posixct)
  expect_equal(ncol(df_posixct_wy), ncol(df_posixct) + 1)
  
  df_char <- mutate(df_test, dateTime = as.character(dateTime))
  df_char_wy <- addWaterYear(df_char)
  expect_equal(ncol(df_char_wy), ncol(df_char) + 1)
  
  df_num <- mutate(df_test, dateTime = as.numeric(dateTime))
  expect_error(addWaterYear(df_num), "'origin' must be supplied")
})

test_that("addWaterYear works for each column name", {
  
  nwisqw_style <- df_test
  nwisqw_style_wy <- addWaterYear(nwisqw_style)
  expect_equal(ncol(nwisqw_style_wy), ncol(nwisqw_style) + 1)
  
  nwisdata_style <- df_test
  names(nwisdata_style)[2] <- "Date"
  nwisdata_style_wy <- addWaterYear(nwisdata_style)
  expect_equal(ncol(nwisdata_style_wy), ncol(nwisdata_style) + 1)
  
  wqp_style <- df_test
  names(wqp_style)[2] <- "ActivityStartDate"
  wqp_style[['ActivityEndDate']] <- wqp_style[["ActivityStartDate"]]
  wqp_style_wy <- addWaterYear(wqp_style)
  expect_equal(ncol(wqp_style_wy), ncol(wqp_style) + 2)
  
  userspecified_style <- df_test
  names(userspecified_style)[2] <- "MyDateCol"
  expect_error(addWaterYear(userspecified_style),
               "specified date column does not exist in supplied data frame")
})

test_that("addWaterYear correctly calculates the WY and is numeric", {
  df_test_wy <- addWaterYear(df_test)
  expect_is(df_test_wy[['waterYear']], "numeric")
  expect_true(all(df_test_wy[['waterYear']][1:9] == 2010))
  expect_true(all(df_test_wy[['waterYear']][10:13] == 2011))
})

test_that("addWaterYear adds column next to dateTime", {
  df_test_wy <- addWaterYear(df_test)
  dateTime_col <- which(names(df_test_wy) == "dateTime")
  expect_equal(names(df_test_wy)[dateTime_col + 1], "waterYear")
})

test_that("addWaterYear can be used with pipes", {
  library(dplyr)
  df_test_wy <- df_test %>% addWaterYear()
  expect_equal(ncol(df_test_wy), ncol(df_test) + 1)
})

test_that("addWaterYear doesn't add another WY column if it exists", {
  df_test_wy <- addWaterYear(df_test)
  expect_equal(ncol(df_test_wy), ncol(df_test) + 1)
  df_test_wy2 <- addWaterYear(df_test_wy)
  expect_equal(ncol(df_test_wy2), ncol(df_test_wy))
})

test_that("calcWaterYear can handle missing values", {
  dateVec <- seq(as.Date("2010-01-01"),as.Date("2011-01-31"), by="months")
  dateVec[c(3,7,12)] <- NA
  wyVec <- dataRetrieval:::calcWaterYear(dateVec)
  
  expect_is(wyVec, "numeric")
  expect_true(all(is.na(wyVec[c(3,7,12)])))
})
