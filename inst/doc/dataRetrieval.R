## ----openLibrary, echo=FALSE------------------------------
library(xtable)
options(continue=" ")
options(width=60)
library(knitr)


## ----include=TRUE ,echo=FALSE,eval=TRUE-------------------
opts_chunk$set(highlight=TRUE, tidy=TRUE, keep.space=TRUE, keep.blank.space=FALSE, keep.comment=TRUE, tidy=FALSE,comment="")
knit_hooks$set(inline = function(x) {
   if (is.numeric(x)) round(x, 3)})
knit_hooks$set(crop = hook_pdfcrop)

bold.colHeaders <- function(x) {
  x <- gsub("\\^(\\d)","$\\^\\1$",x)
  x <- gsub("\\%","\\\\%",x)
  x <- gsub("\\_"," ",x)
  returnX <- paste("\\multicolumn{1}{c}{\\textbf{\\textsf{", x, "}}}", sep = "")
}
addSpace <- function(x) ifelse(x != "1", "[5pt]","")

## ----workflow, echo=TRUE,eval=FALSE-----------------------
#  library(dataRetrieval)
#  # Choptank River near Greensboro, MD
#  siteNumber <- "01491000"
#  ChoptankInfo <- readNWISsite(siteNumber)
#  parameterCd <- "00060"
#  
#  #Raw daily data:
#  rawDailyData <- readNWISdv(siteNumber,parameterCd,
#                        "1980-01-01","2010-01-01")
#  
#  # Sample data Nitrate:
#  parameterCd <- "00618"
#  qwData <- readNWISqw(siteNumber,parameterCd,
#                        "1980-01-01","2010-01-01")
#  
#  pCode <- readNWISpCode(parameterCd)
#  

## ----tableParameterCodes, echo=FALSE,results='asis'-------
pCode <- c('00060', '00065', '00010','00045','00400')
shortName <- c("Discharge [ft$^3$/s]","Gage height [ft]","Temperature [C]", "Precipitation [in]", "pH")

data.df <- data.frame(pCode, shortName, stringsAsFactors=FALSE)

print(xtable(data.df,
       label="tab:params",
       caption="Common USGS Parameter Codes"),
       caption.placement="top",
       size = "\\footnotesize",
       latex.environment=NULL,
       sanitize.text.function = function(x) {x},
       sanitize.colnames.function =  bold.colHeaders,
       sanitize.rownames.function = addSpace
      )


## ----tableParameterCodesDataRetrieval, echo=TRUE, eval=TRUE----
library(dataRetrieval)
parameterCdFile <-  parameterCdFile
names(parameterCdFile)

## ----tableStatCodes, echo=FALSE,results='asis'------------
StatCode <- c('00001', '00002', '00003','00008')
shortName <- c("Maximum","Minimum","Mean", "Median")

data.df <- data.frame(StatCode, shortName, stringsAsFactors=FALSE)

print(xtable(data.df,label="tab:stat",
           caption="Commonly used USGS Stat Codes"),
       caption.placement="top",
       size = "\\footnotesize",
       latex.environment=NULL,
       sanitize.colnames.function = bold.colHeaders,
       sanitize.rownames.function = addSpace
      )


## ----getSite, echo=TRUE-----------------------------------
siteNumbers <- c("01491000","01645000") 
siteINFO <- readNWISsite(siteNumbers)

## ----siteNames2, echo=TRUE--------------------------------
siteINFO$station_nm

## ----siteNames3, echo=TRUE--------------------------------
comment(siteINFO)

## ----getSiteExtended, echo=TRUE---------------------------
# Continuing from the previous example:
# This pulls out just the daily, mean data:

dailyDataAvailable <- whatNWISdata(siteNumbers,
                    service="dv", statCd="00003")



## ----tablegda, echo=FALSE,results='asis'------------------
tableData <- with(dailyDataAvailable, 
      data.frame( 
      siteNumber= site_no,
      srsname=srsname, 
      startDate=as.character(begin_date), 
      endDate=as.character(end_date), 
      count=as.character(count_nu),
      units=parameter_units,
#       statCd = stat_cd,
      stringsAsFactors=FALSE)
      )

tableData$units[which(tableData$units == "ft3/s")] <- "ft$^3$/s"
tableData$units[which(tableData$units == "uS/cm @25C")] <- "$\\mu$S/cm @25C"


print(xtable(tableData,label="tab:gda",
    caption="Reformatted version of output from \\texttt{whatNWISdata} function for the Choptank River near Greensboro, MD, and from Seneca Creek at Dawsonville, MD from the daily values service [Some columns deleted for space considerations]"),
       caption.placement="top",
       size = "\\footnotesize",
       latex.environment=NULL,
       sanitize.text.function = function(x) {x},
       sanitize.colnames.function =  bold.colHeaders,
       sanitize.rownames.function = addSpace
      )


## ----label=getPCodeInfo, echo=TRUE------------------------
# Using defaults:
parameterCd <- "00618" 
parameterINFO <- readNWISpCode(parameterCd)
colnames(parameterINFO)

## ----siteNames, echo=TRUE---------------------------------
parameterINFO$parameter_nm

## ----label=getNWISDaily, echo=TRUE, eval=TRUE-------------

# Choptank River near Greensboro, MD:
siteNumber <- "01491000"
parameterCd <- "00060"  # Discharge
startDate <- "2009-10-01"  
endDate <- "2012-09-30" 

discharge <- readNWISdv(siteNumber, 
                    parameterCd, startDate, endDate)
names(discharge)

## ----label=getNWIStemperature, echo=TRUE------------------

parameterCd <- c("00010","00060")  # Temperature and discharge
statCd <- c("00001","00003")  # Mean and maximum
startDate <- "2012-01-01"
endDate <- "2012-05-01"

temperatureAndFlow <- readNWISdv(siteNumber, parameterCd, 
        startDate, endDate, statCd=statCd)


## ----label=renameColumns, echo=TRUE-----------------------
names(temperatureAndFlow)

temperatureAndFlow <- renameNWISColumns(temperatureAndFlow)
names(temperatureAndFlow)

## ----getNWIStemperaturePlot, echo=TRUE, fig.cap="Temperature and discharge plot of Choptank River in 2012.",out.width='1\\linewidth',out.height='1\\linewidth',fig.show='hold'----
variableInfo <- attr(temperatureAndFlow, "variableInfo")
siteInfo <- attr(temperatureAndFlow, "siteInfo")

par(mar=c(5,5,5,5)) #sets the size of the plot window

plot(temperatureAndFlow$dateTime, temperatureAndFlow$Wtemp_Max,
  ylab=variableInfo$parameter_desc[1],xlab="" )
par(new=TRUE)
plot(temperatureAndFlow$dateTime, temperatureAndFlow$Flow,
  col="red",type="l",xaxt="n",yaxt="n",xlab="",ylab="",axes=FALSE
  )
axis(4,col="red",col.axis="red")
mtext(variableInfo$parameter_desc[2],side=4,line=3,col="red")
title(paste(siteInfo$station_nm,"2012"))
legend("topleft", variableInfo$param_units, 
       col=c("black","red"),lty=c(NA,1),pch=c(1,NA))

## ----label=readNWISuv, echo=TRUE--------------------------

parameterCd <- "00060"  # Discharge
startDate <- "2012-05-12" 
endDate <- "2012-05-13" 
dischargeUnit <- readNWISuv(siteNumber, parameterCd, 
        startDate, endDate)

## ----dischargeData, echo=TRUE-----------------------------
head(dischargeUnit)

## ----label=getQW, echo=TRUE, eval=TRUE--------------------
 
# Dissolved Nitrate parameter codes:
parameterCd <- c("00618","71851")
startDate <- "1985-10-01"
endDate <- "2012-09-30"

dfLong <- readNWISqw(siteNumber, parameterCd, 
      startDate, endDate, expanded=TRUE,reshape=FALSE)

# Or the wide return:
# dfWide <- readNWISqw(siteNumber, parameterCd, 
#       startDate, endDate, expanded=TRUE, reshape=TRUE)


## ----qwmeta, echo=TRUE, eval=TRUE-------------------------

comment(dfLong)


## ----gwlexample, echo=TRUE, eval=TRUE---------------------
siteNumber <- "434400121275801"
groundWater <- readNWISgwl(siteNumber)

names(groundWater)


## ----peakexample, echo=TRUE, eval=TRUE--------------------
siteNumber <- '01594440'
peakData <- readNWISpeak(siteNumber)


names(peakData)


## ----ratingexample, echo=TRUE, eval=TRUE------------------
ratingData <- readNWISrating(siteNumber, "base")
attr(ratingData, "RATING")

names(ratingData)


## ----surfexample, echo=TRUE, eval=TRUE--------------------
surfaceData <- readNWISmeas(siteNumber)

names(surfaceData)


## ----label=geturl, echo=TRUE, eval=FALSE------------------
#  # Dissolved Nitrate parameter codes:
#  pCode <- c("00618","71851")
#  startDate <- "1964-06-11"
#  endDate <- "2012-12-18"
#  url_qw <- constructNWISURL(siteNumber,pCode,startDate,endDate,"qw")
#  url_dv <- constructNWISURL(siteNumber,"00060",startDate,endDate,
#                             "dv",statCd="00003")
#  url_uv <- constructNWISURL(siteNumber,"00060",startDate,endDate,"uv")

## ----label=getQWData, echo=TRUE, eval=FALSE---------------
#  specificCond <- readWQPqw('WIDNR_WQX-10032762',
#                  'Specific conductance','2011-05-01','2011-09-30')

## ----siteSearch-------------------------------------------
sites <- whatNWISsites(bBox="-83.0,36.5,-81.0,38.5", 
                      parameterCd="00010,00060",
                      hasDataTypeCd="dv")

names(sites)
nrow(sites)

## ----dataExample------------------------------------------
dischargeWI <- readNWISdata(service="dv",
                           stateCd="WI",
                           parameterCd="00060",
                           drainAreaMin="50",
                           statCd="00003")
names(dischargeWI)
nrow(dischargeWI)

## ----NJChloride, eval=FALSE-------------------------------
#  
#  sitesNJ <- whatWQPsites(statecode="US:34",
#                         characteristicName="Chloride")
#  

## ----phData, eval=FALSE-----------------------------------
#  
#  dataPH <- readWQPdata(statecode="US:55",
#                   characteristicName="pH")
#  

## ----meta1, eval=TRUE-------------------------------------

attr(dischargeWI, "url")

attr(dischargeWI, "queryTime")


## ----meta2, eval=TRUE-------------------------------------

names(attributes(dischargeWI))


## ----meta3, eval=TRUE-------------------------------------

siteInfo <- attr(dischargeWI, "siteInfo")
head(siteInfo)

variableInfo <- attr(dischargeWI, "variableInfo")



## ----meta5, eval=TRUE, eval=FALSE-------------------------
#  comment(peakData)
#  
#  #Which is equivalent to:
#  # attr(peakData, "comment")

## ----meta6, eval=TRUE, eval=TRUE--------------------------
comment(peakData)[c(1:15,58:66)]


## ----helpFunc,eval = FALSE--------------------------------
#  ?readNWISpCode

## ----seeVignette,eval = FALSE-----------------------------
#  vignette(dataRetrieval)

## ----installFromCran,eval = FALSE-------------------------
#  install.packages("dataRetrieval")

## ----openLibraryTest, eval=FALSE--------------------------
#  library(dataRetrieval)

## ----label=getSiteApp, echo=TRUE--------------------------
availableData <- whatNWISdata(siteNumber, "dv")
dailyData <- availableData["00003" == availableData$stat_cd,]

tableData <- with(dailyData, 
      data.frame(
        shortName=srsname, 
        Start=begin_date, 
        End=end_date, 
        Count=count_nu,
        Units=parameter_units)
      )
tableData

## ----label=saveData, echo=TRUE, eval=FALSE----------------
#  write.table(tableData, file="tableData.tsv",sep="\t",
#              row.names = FALSE,quote=FALSE)

