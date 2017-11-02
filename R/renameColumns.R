#' renameColumns
#'
#' Rename columns coming back from NWIS data retrievals.  Daily and unit value columns
#' have names derived from their data descriptor, parameter, and statistic codes. This
#' function reads information from the header and the arguments in the call to
#' to rename those columns.
#'
#' @param rawData the daily- or unit-values datset retrieved from NWISweb.
#' @param p00010 the base name for parameter code 00010.
#' @param p00045 the base name for parameter code 00045.
#' @param p00060 the base name for parameter code 00060.
#' @param p00065 the base name for parameter code 00065.
#' @param p00095 the base name for parameter code 00095.
#' @param p00300 the base name for parameter code 00300.
#' @param p00400 the base name for parameter code 00400.
#' @param p62611 the base name for parameter code 62611.
#' @param p63680 the base name for parameter code 63680.
#' @param p72019 the base name for parameter code 72019.
#' @param \dots named arguments for the base name for any other parameter code. The
#'form of the name must be like pXXXXX, where XXXXX is the parameter code.
#' @return A dataset like \code{data} with selected columns renamed.
#' @note The following statistics codes are converted by \code{renameNWISColumns}. 
#'\describe{
#'\item{00000}{Instantaneous Value, suffix: Inst}
#'\item{00001}{Maximum value, suffix: Max}
#'\item{00002}{Minimum value, suffix: Min}
#'\item{00003}{Mean value, no suffix}
#'\item{00006}{Sum of values, suffix: Sum}
#'\item{00007}{Modal value, suffix: Mode}
#'\item{00008}{Median value, suffix: Median}
#'\item{00012}{Equivalent mean value, suffix: EqMean}
#'\item{00021}{Tidal high-high value, suffix: HiHiTide}
#'\item{00022}{Tidal low-high value, suffix: LoHiTide}
#'\item{00023}{Tidal high-low value, suffix: HiLoTide}
#'\item{00024}{Tidal low-low value, suffix: LoLoTide}
#'}
#' @seealso \code{\link{readNWISdv}}, \code{\link{readNWISuv}}
#' @keywords manip IO
#' @export
#' @examples
#' siteWithTwo <- '01480015'
#' startDate <- "2012-09-01"
#' endDate <- "2012-10-01"
#' \dontrun{
#' url2 <- constructNWISURL(siteWithTwo, "00060",startDate,endDate,'dv')
#' twoResults <- importWaterML1(url2,TRUE)
#' twoResults <- renameNWISColumns(twoResults)
#' url2RDB <- constructNWISURL(siteWithTwo,"00060",
#'          startDate,endDate,"dv",format="tsv")
#' rdbResults <- importRDB1(url2RDB)
#' rdbResults <- renameNWISColumns(rdbResults) 
#' }
renameNWISColumns <- function(rawData, p00010="Wtemp", p00045="Precip",
                          p00060="Flow", p00065="GH", p00095="SpecCond", p00300="DO",
                          p00400="pH", p62611="GWL", p63680="Turb", p72019="WLBLS",
                          ...){
  
  Cnames <- names(rawData)
  
  Conv <- list(...)
  Conv$p00010 <- p00010
  Conv$p00060 <- p00060
  Conv$p00045 <- p00045
  Conv$p00065 <- p00065
  Conv$p00095 <- p00095
  Conv$p00300 <- p00300
  Conv$p00400 <- p00400
  Conv$p62611 <- p62611
  Conv$p63680 <- p63680
  Conv$p72019 <- p72019
  
  Conv$s00000<- "Inst" # Why is this in dv?
  Conv$s00001 <- "Max"
  Conv$s00002 <- "Min"
  Conv$s00003 <- ""
  Conv$s00006 <- "Sum"
  Conv$s00007 <- "Mode"
  Conv$s00008 <- "Median"
  Conv$s00012<- "EqMean"
  Conv$s00021<- "HiHiTide"
  Conv$s00022<- "LoHiTide"
  Conv$s00023<- "HiLoTide"
  Conv$s00024<- "LoLoTide"

  dataColumns <- c(grep("X_", Cnames),grep("X\\d{2}", Cnames))
  dataColumnsChangedParam <- NULL 
  for (i in dataColumns){
    chunks <- strsplit(Cnames[i], "_")[[1]]
    
    #Pcodes:
    for(j in 1:length(chunks)){
      if(paste0("p",chunks[j]) %in% names(Conv)){
        chunks[j] <- as.character(Conv[paste0("p",chunks[j])])
        Cnames[i] <- paste(chunks, collapse ="_")
        dataColumnsChangedParam <- c(dataColumnsChangedParam, i)
        break
      }
    }
    #Stat codes:
    for(j in 1:length(chunks)){
      if(paste0("s",chunks[j]) %in% names(Conv)){
        chunks[j] <- as.character(Conv[paste0("s",chunks[j])])
        chunks <- chunks[chunks != ""]
        Cnames[i] <- paste(chunks, collapse ="_")
        break
      }
    }
  }
  
  Cnames[dataColumnsChangedParam] <- gsub("X_","",Cnames[dataColumnsChangedParam])

  names(rawData) <- Cnames
  
  return(rawData)
}
