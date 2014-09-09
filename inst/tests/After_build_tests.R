#Unit tests for tabbedDataRetrivals.R

expect_that(formatCheckSiteNumber('01594440'),prints_text('01594440'))
expect_that(formatCheckParameterCd('00001'),prints_text('00001'))
expect_that(formatCheckDate('1985-01-01', "startDate"),prints_text('1985-01-01'))
expect_that(dateFormatCheck('1985-01-01'), is_true())
expect_that(dateFormatCheck('01/01/1985'), is_false())
expect_that(checkStartEndDate('1985-01-01', '1985-01-31'), equals(c('1985-01-01', '1985-01-31')))

DecYear <- c(1,1,2,3,4,4,5)
ConcHigh <- c(1,2,3,4,5,5,6)
dataInputForRemoveDuplicates <- data.frame(DecYear, ConcHigh)
DecYear <- c(1,1,2,3,4,5)
ConcHigh <- c(1,2,3,4,5,6)
dataReturnForRemoveDuplicates <- data.frame(DecYear, ConcHigh)

# actualReturnFromRemoveDuplicates <- removeDuplicates(dataInputForRemoveDuplicates)
# expect_that(actualReturnFromRemoveDuplicates$DecYear, equals(dataReturnForRemoveDuplicates$DecYear))
# expect_that(actualReturnFromRemoveDuplicates$ConcHigh, equals(dataReturnForRemoveDuplicates$ConcHigh))

value <- c(1,2,3)
code <- c("","","")
dateTime <- c('1985-01-01', '1985-01-02', '1985-01-03')
Date <- dateTime
Q <- c(0.5, 1.0, 1.5)
Qualifier <- code
Julian <- c(49308.25, 49309.25, 49310.25)
Month <- c(1,1,1)
Day <- c(1,2,3)
DecYear <- c(1985.00136612022, 1985.00409836066, 1985.00683060109)
MonthSeq <- c(1621, 1621, 1621)
LogQ <- c(-0.693147180559945, 0.0000000, 0.405465108108164)
i <- c(1,2,3)
comment1 <- c("","","")
comment2 <- c("","<","")
value2 <- c(2,3,4)
comment3 <- c("","","<")
value3 <- c(3,4,5)
ConcLow <- c(6,6,7)
ConcHigh <- c(6,9,12)
Uncen <- c(1,0,0)
ConcAve <- c(6.0,7.5,9.5)
SinDY <- c(0.00858348109043033,0.0257479136805711,0.0429047581805647)
CosDY <- c(0.999963161247638, 0.999668467513654, 0.999079166895931)

dataReturnForPopulateDateColumns <- data.frame(Date,Julian,Month,Day,DecYear,MonthSeq,stringsAsFactors=FALSE)
expect_that(populateDateColumns(dateTime), equals(dataReturnForPopulateDateColumns))

dataInputForPopulateDaily <- data.frame(dateTime, value, code, stringsAsFactors=FALSE)
Date <- as.Date(Date)
dataReturnForPopulateDaily <- data.frame(Date, Q, dataReturnForPopulateDateColumns[,-1], Qualifier, LogQ, i, stringsAsFactors=FALSE)
expect_that(populateDaily(dataInputForPopulateDaily, 2, interactive=FALSE), equals(dataReturnForPopulateDaily))

dataInputForCompressData <- data.frame(dateTime, comment1, value, comment2, value2, comment3, value3, stringsAsFactors=FALSE)
dataReturnForCompressData <- data.frame(dateTime, ConcLow, ConcHigh, Uncen, stringsAsFactors=FALSE)
expect_that(compressData(dataInputForCompressData), equals(dataReturnForCompressData))

dataInputForPopulateSampleData <- compressData(dataInputForCompressData)
Date <- as.character(Date)
dataReturnForPopulateSampleColumns <- data.frame(Date, ConcLow, ConcHigh, Uncen, ConcAve, dataReturnForPopulateDateColumns[,-1], SinDY, CosDY, stringsAsFactors=FALSE)
expect_that(populateSampleColumns(dataInputForPopulateSampleData), equals(dataReturnForPopulateSampleColumns))

code <- comment2
dataInputForPopulateConcentrations <- data.frame(value, code, stringsAsFactors=FALSE)

ConcLow <- c(1,0,3)
ConcHigh <- c(1,2,3)
Uncen <- c(1,0,1)
dataReturnForPopulateConcentrations <- data.frame(ConcLow, ConcHigh, Uncen, stringsAsFactors=FALSE)
expect_that(populateConcentrations(dataInputForPopulateConcentrations), equals(dataReturnForPopulateConcentrations))

data(susQ)
Daily1 <- getPreLoadedDailyData(susQ, interactive=FALSE)

expect_that(Daily1$Date[1], equals(as.Date("1977-10-01")))
expect_that(Daily1$Q[1], equals(2123.76348))
expect_that(Daily1$Julian[1], equals(46659))
expect_that(Daily1$Month[1], equals(10))
expect_that(Daily1$Day[1], equals(274))
expect_that(Daily1$DecYear[1], equals(1977.747268))
expect_that(Daily1$MonthSeq[1], equals(1534))
expect_that(Daily1$LogQ[1], equals(7.660945))
expect_that(Daily1$Q7[7], equals(1723.28236))
expect_that(Daily1$Q30[30], equals(2156.32785))












