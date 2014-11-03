#' renameColumns
#'
#' Rename columns coming back from NWIS data retrievals
#'
#' @param rawData dataframe returned from retrieval functions
#' @keywords data import USGS web service
#' @return rawData dataframe with improved column names
#' @export
#' @examples
#' siteWithTwo <- '01480015'
#' startDate <- "2012-09-01"
#' endDate <- "2012-10-01"
#' url2 <- constructNWISURL(siteWithTwo, "00060",startDate,endDate,'dv')
#' twoResults <- importWaterML1(url2,TRUE)
#' twoResults <- renameColumns(twoResults)
renameColumns <- function(rawData, p00010="Wtemp", p00045="Precip",
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
  
  Conv$s00001 <- "Max"
  Conv$s00002 <- "Min"
  Conv$s00003 <- ""
  Conv$s00006 <- "Sum"
  Conv$s00007 <- "Mode"
  Conv$s00008 <- "Median"
  Conv$s00011<- "Inst" # Why is this in dv?
  Conv$s00012<- "EqMean"
  Conv$s00021<- "HiHiTide"
  Conv$s00022<- "LoHiTide"
  Conv$s00023<- "HiLoTide"
  Conv$s00024<- "LoLoTide"

  dataColumns <- grep("X_", Cnames)
  
  for (i in dataColumns){
    chunks <- strsplit(Cnames[i], "_")[[1]]
    
    #Pcodes:
    for(j in 1:length(chunks)){
      if(paste0("p",chunks[j]) %in% names(Conv)){
        chunks[j] <- as.character(Conv[paste0("p",chunks[j])])
        Cnames[i] <- paste(chunks, collapse ="_")
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
  
  Cnames <- gsub("X_","",Cnames)

  names(rawData) <- Cnames
  
  return(rawData)
}
