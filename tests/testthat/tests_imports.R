context("importRDB_noCRAN")

test_that("External importRDB1 tests", {
  testthat::skip_on_cran()
  
  siteNumber <- "02177000"
  startDate <- "2012-09-01"
  endDate <- "2012-10-01"
  offering <- "00003"
  property <- "00060"
   
  obs_url <- constructNWISURL(siteNumber,property,
            startDate,endDate,"dv",format="tsv")
  data <- importRDB1(obs_url)
  expect_is(data$datetime, 'character') 
  
  urlMultiPcodes <- constructNWISURL("04085427",c("00060","00010"),
          startDate,endDate,"dv",statCd=c("00003","00001"),"tsv")
  multiData <- importRDB1(urlMultiPcodes)
  pCodeCols <- grep("X", colnames(multiData))
  expect_that(length(pCodeCols)/2 > 2, is_true() )
  
  unitDataURL <- constructNWISURL(siteNumber,property,
          "2013-11-03","2013-11-03","uv",format="tsv") #includes timezone switch
  unitData <- importRDB1(unitDataURL, asDateTime=TRUE)
  
  expect_that(as.numeric(unitData[which(unitData$tz_cd == "EST")[1],"datetime"]),
              equals(as.numeric(as.POSIXct("2013-11-03 01:00:00", tz="UTC")+60*60*5)))
  
  
  qwURL <- constructNWISURL(c('04024430','04024000'),
           c('34247','30234','32104','34220'),
          "2010-11-03","","qw",format="rdb") 
  qwData <- importRDB1(qwURL, qw=TRUE, tz="America/Chicago")
  expect_is(qwData$sample_dt, 'Date')
  expect_is(qwData$startDateTime, 'POSIXct')
  
  iceSite <- '04024430'
  start <- "2014-11-09"
  end <- "2014-11-28"
  urlIce <- constructNWISURL(iceSite,"00060",start, end,"uv",format="tsv")
  ice <- importRDB1(urlIce)
  expect_that(sum(is.na(ice$X01_00060)) > 0, is_true())
  
  iceNoConvert <- importRDB1(urlIce, convertType=FALSE)
  expect_that(sum(iceNoConvert$X01_00060 == "Ice") > 0, is_true())
})

context("importRDB")
test_that("CRAN-friendly importRDB test", {
  filePath <- system.file("extdata", package="dataRetrieval")
  fileName <- "RDB1Example.txt"
  fullPath <- file.path(filePath, fileName)
  importUserRDB <- importRDB1(fullPath)
  
  # default is to turn dates to characters
  expect_is(importUserRDB$datetime, 'character')
  
})

context("importWaterML1")
test_that("CRAN-friendly importWaterML1 test", {
  filePath <- system.file("extdata", package="dataRetrieval")
  fileName <- "WaterML1Example.xml"
  fullPath <- file.path(filePath, fileName)
  importUserWML1 <- importWaterML1(fullPath, asDateTime = TRUE)
  
  # default is to turn dates to characters
  expect_is(importUserWML1$dateTime, 'POSIXct')
  
})

test_that("External importWaterML1 test", {
  testthat::skip_on_cran()
  
  siteNumber <- "02177000"
  startDate <- "2012-09-01"
  endDate <- "2012-10-01"
  offering <- '00003'
  property <- '00060'
  obs_url <- constructNWISURL(siteNumber,property,startDate,endDate,'dv')
  
  data <- importWaterML1(obs_url,TRUE)
  expect_is(data$dateTime, 'POSIXct')
  
  groundWaterSite <- "431049071324301"
  startGW <- "2013-10-01"
  endGW <- "2014-06-30"
  groundwaterExampleURL <- constructNWISURL(groundWaterSite, NA,
           startGW,endGW, service="gwlevels")
  groundWater <- importWaterML1(groundwaterExampleURL)
  
  expect_is(groundWater$dateTime, 'character')
  
  unitDataURL <- constructNWISURL(siteNumber,property,
          "2013-11-03","2013-11-03",'uv')
  unitData <- importWaterML1(unitDataURL,TRUE)
  expect_is(unitData$dateTime, 'POSIXct')
  
  
  # Two sites, two pcodes, one site has two data descriptors:
  siteNumber <- c('01480015',"04085427")
  obs_url <- constructNWISURL(siteNumber,c("00060","00010"),startDate,endDate,'dv')
  data <- importWaterML1(obs_url)
  expect_that(length(unique(data$site_no)) == 2, is_true())
  expect_that(ncol(data) == 10, is_true()) # 3 data, 3 remark codes, and 4 (agency, site, dateTime, tz)

  inactiveSite <- "05212700"
  inactiveSite <- constructNWISURL(inactiveSite, "00060", "2014-01-01", "2014-01-10",'dv')
  inactiveSite <- importWaterML1(inactiveSite)
  expect_that(nrow(inactiveSite) == 0, is_true())
  
  inactiveAndActive <- c("07334200","05212700")
  inactiveAndActive <- constructNWISURL(inactiveAndActive, "00060", "2014-01-01", "2014-01-10",'dv')
  inactiveAndActive <- importWaterML1(inactiveAndActive)
  expect_that(length(unique(inactiveAndActive$site_no)) == 1, is_true())
  
})

context("importWQP_noCRAN")

test_that("External WQP tests", {
  testthat::skip_on_cran()

  rawSampleURL <- constructWQPURL('USGS-01594440','01075', '', '')
  rawSample <- importWQP(rawSampleURL)
  expect_is(rawSample$ActivityStartDateTime, 'POSIXct')
  
  url2 <- paste0(rawSampleURL,"&zip=yes")
  rawSample2 <- suppressWarnings(importWQP(url2, TRUE))
  expect_is(rawSample2$ActivityStartDateTime, 'POSIXct')
  
  STORETex <- constructWQPURL('WIDNR_WQX-10032762','Specific conductance', '', '')
  STORETdata <- importWQP(STORETex)
  expect_is(rawSample2$ActivityStartDateTime, 'POSIXct')
})