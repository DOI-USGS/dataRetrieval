#Unit tests for tabbedDataRetrivals.R

# dateFormatCheck:
expect_that(dateFormatCheck('1985-01-01'), is_true())
expect_that(dateFormatCheck('01/01/1985'), is_false())
# formatCheckSiteNumber:
expect_that(formatCheckSiteNumber('01594440'),prints_text('01594440'))
# formatCheckParameterCd:
expect_that(formatCheckParameterCd('00001'),prints_text('00001'))
# formatCheckDate:
expect_that(formatCheckDate('1985-01-01', "StartDate"),prints_text('1985-01-01'))
# checkStartEndDate:
expect_that(checkStartEndDate('1985-01-01', '1985-01-31'), equals(c('1985-01-01', '1985-01-31')))

value <- c(1,2,3)
code <- c("","","")
dateTime <- c('1985-01-01', '1985-01-02', '1985-01-03')
Date <- as.Date(dateTime)
Q <- c(0.5, 1.0, 1.5)
Qualifier <- code
Julian <- c(49308, 49309, 49310)
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
# populateDateColumns:
dateTime <- as.Date(dateTime)
expect_that(populateDateColumns(dateTime), equals(dataReturnForPopulateDateColumns))

dataInputForPopulateDaily <- data.frame(dateTime, value, code, stringsAsFactors=FALSE)
Date <- as.Date(Date)

dataReturnForPopulateDaily <- data.frame(Date, Q, dataReturnForPopulateDateColumns[,-1], Qualifier, LogQ, i, stringsAsFactors=FALSE)
# populateDaily:
expect_that(populateDaily(dataInputForPopulateDaily, 2, interactive=FALSE), gives_warning())
# expect_that(populateDaily(dataInputForPopulateDaily, 2, interactive=FALSE), equals(dataReturnForPopulateDaily))

dataInputForCompressData <- data.frame(dateTime, comment1, value, comment2, value2, comment3, value3, stringsAsFactors=FALSE)
dataReturnForCompressData <- data.frame(dateTime, ConcLow, ConcHigh, Uncen, stringsAsFactors=FALSE)
# compressData:

expect_that(compressData(dataInputForCompressData), equals(dataReturnForCompressData))

dataInputForPopulateSampleData <- compressData(dataInputForCompressData)
# Date <- as.character(Date)
dataReturnForPopulateSampleColumns <- data.frame(Date, ConcLow, ConcHigh, Uncen, ConcAve, dataReturnForPopulateDateColumns[,-1], SinDY, CosDY, stringsAsFactors=FALSE)
# populateSampleColumns
####################################### next 1 has issues.....
expect_that(populateSampleColumns(dataInputForPopulateSampleData), equals(dataReturnForPopulateSampleColumns))

code <- comment2
dataInputForPopulateConcentrations <- data.frame(value, code, stringsAsFactors=FALSE)
ConcLow <- c(1,0,3)
ConcHigh <- c(1,2,3)
Uncen <- c(1,0,1)
dataReturnForPopulateConcentrations <- data.frame(ConcLow, ConcHigh, Uncen, stringsAsFactors=FALSE)
# populateConcentrations:
expect_that(populateConcentrations(dataInputForPopulateConcentrations), equals(dataReturnForPopulateConcentrations))

# removeDuplicates:
DecYear <- c(1,1,2,3,4,4,5)
ConcHigh <- c(1,2,3,4,5,5,6)
dataInputForRemoveDuplicates <- data.frame(DecYear, ConcHigh)
DecYear <- c(1,1,2,3,4,5)
ConcHigh <- c(1,2,3,4,5,6)
dataReturnForRemoveDuplicates <- data.frame(DecYear, ConcHigh)
actualReturnFromRemoveDuplicates <- removeDuplicates(dataInputForRemoveDuplicates)
expect_that(actualReturnFromRemoveDuplicates$DecYear, equals(dataReturnForRemoveDuplicates$DecYear))
expect_that(actualReturnFromRemoveDuplicates$ConcHigh, equals(dataReturnForRemoveDuplicates$ConcHigh))


# getPreLoadedDailyData:
#data(ChoptankRiverNitrate)
filePathToExampleData = "~/RData/"
pathToChopQ <- paste(filePathToExampleData, "ChoptankRiverFlow.txt", sep="")
pathToChopNit <- paste(filePathToExampleData, "ChoptankRiverNitrate.csv", sep="")
ChopQ <- read.delim(pathToChopQ, header = TRUE, colClasses=c('character'), sep="\t", fill = TRUE, comment.char="#")
ChopNit <- read.delim(pathToChopNit, header = TRUE, colClasses=c('character'), sep=";", fill = TRUE, comment.char="#")
Daily1 <- getPreLoadedDailyData(ChopQ, interactive=FALSE)

expect_that(Daily1$Date[1], equals(as.Date("1999-10-01")))
expect_that(Daily1$Q[1], equals(0.08579728533189906469758))
expect_that(Daily1$Julian[1], equals(54694))
expect_that(Daily1$Month[1], equals(10))
expect_that(Daily1$Day[1], equals(274))
expect_that(Daily1$DecYear[1], equals(1999.747267759562873835))
expect_that(Daily1$MonthSeq[1], equals(1798))
expect_that(Daily1$LogQ[1], equals(-2.455767912477115455516))
expect_that(Daily1$Q7[7], equals(0.07308233384405732335765))
expect_that(Daily1$Q30[30], equals(0.07724428493501958592482))

# retrieveNWISData:
Daily2 <- retrieveNWISData('01594440','00060', '1985-01-01', '1985-01-31')
expect_that(Daily2$agency[1], equals("USGS"))
expect_that(Daily2$site[1], equals("01594440"))
expect_that(Daily2$dateTime[1], equals(as.Date("1985-01-01")))
expect_that(Daily2$value[1], equals(157))
expect_that(Daily2$code[1], equals("A"))

# getSiteFileData:
INFO1 <- getSiteFileData('01594440')
expect_that(INFO1$station.nm, equals("PATUXENT RIVER NEAR BOWIE, MD"))

# getQWData:
Sample1 <- getQWData('01594440','01075', '1985-01-01', '1985-03-31')
expect_that(Sample1$dateTime[1], equals(as.Date("1985-03-13")))
expect_that(Sample1$qualifier.01075[1], equals("<"))
expect_that(Sample1$value.01075[1], equals(1))

# getParameterInfo:
INFO2 <- getParameterInfo('01075')
expect_that(INFO2$parameter_nm, equals("Silver, water, filtered, micrograms per liter"))

# populateSiteINFO:
INFO1 <- populateSiteINFO(INFO1, '01594440', interactive=FALSE)
expect_that(INFO1$drainSqKm, equals(901.3158588))

# populateParameterINFO:
INFO2$param.nm <- INFO2$parameter_nm
INFO2$param.units <- INFO2$parameter_units
INFO2$paramShortName <- INFO2$srsname
INFO2$paramNumber <- INFO2$parameter_cd
INFO2 <- populateParameterINFO(INFO2, '01075', interactive=FALSE)
expect_that(INFO2$constitAbbrev, equals(INFO2$paramShortName))

# getDVData:
Daily <- getDVData('01594440','00060', '1985-01-01', '1985-03-31', interactive=FALSE)
expect_that(Daily$Date[1], equals(as.Date("1985-01-01")))
expect_that(Daily$Q[1], equals(4.44574488))
expect_that(Daily$Julian[1], equals(49308))
expect_that(Daily$Month[1], equals(1))
expect_that(Daily$Day[1], equals(1))
expect_that(Daily$DecYear[1], equals(1985.001366))
expect_that(Daily$MonthSeq[1], equals(1621))
expect_that(Daily$LogQ[1], equals(1.49194743))
expect_that(Daily$Q7[7], equals(8.725634))

# getSampleData:
Sample <- getSampleData('01594440','01075', '1985-01-01', '1985-03-31', interactive=FALSE)
expect_that(Sample$Date, equals(as.Date("1985-03-13")))
expect_that(Sample$ConcLow, equals(NA))
expect_that(Sample$ConcHigh, equals(1))
expect_that(Sample$Uncen, equals(0))
expect_that(Sample$ConcAve, equals(0.5))
expect_that(Sample$Julian, equals(49379))
expect_that(Sample$Month, equals(3))
expect_that(Sample$Day, equals(72))
expect_that(Sample$MonthSeq, equals(1623))
expect_that(Sample$DecYear, equals(1985.195355))
expect_that(Sample$SinDY, equals(0.9416344))
expect_that(Sample$CosDY, equals(0.336637284))

# getMetaData:
INFO <- getMetaData('05114000','00010',interactive=FALSE)
expect_that(INFO$station.nm, equals("SOURIS RIVER NR SHERWOOD, ND"))
expect_that(INFO$paramShortName, equals("Temperature, water"))

# data(ChoptankRiverFlow)
filePathToExampleData = "D:/LADData/R Code/gitWRTDS/MainRCodes/dataRetrieval/data/"
pathToChopQ <- paste(filePathToExampleData, "ChoptankRiverFlow.txt", sep="")
pathToChopNit <- paste(filePathToExampleData, "ChoptankRiverNitrate.csv", sep="")
ChopQ <- read.delim(pathToChopQ, header = TRUE, colClasses=c('character'), sep="\t", fill = TRUE, comment.char="#")
ChopNit <- read.delim(pathToChopNit, header = TRUE, colClasses=c('character'), sep=";", fill = TRUE, comment.char="#")

# getPreLoadedSampleData:
Sample1 <- getPreLoadedSampleData(ChoptankRiverNitrate, interactive=FALSE)
expect_that(Sample1$Uncen[135], equals(1))
expect_that(Sample1$ConcLow[135], equals(1.42))
expect_that(Sample1$ConcAve[135], equals(1.42))



