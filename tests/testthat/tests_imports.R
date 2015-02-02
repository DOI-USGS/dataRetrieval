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
  
  
  
  iceSite <- '04024430'
  start <- "2014-11-09"
  end <- "2014-11-28"
  urlIce <- constructNWISURL(iceSite,"00060",start, end,"uv",format="tsv")
  ice <- importRDB1(urlIce)
  iceNoConvert <- importRDB1(urlIce, convertType=FALSE)
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

