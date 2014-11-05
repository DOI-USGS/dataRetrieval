#' @rdname whatNWISsites
#' @export
getNWISSites<- function(...){
  message("This function is being deprecated. Please use whatNWISsites in the future.")
  whatNWISsites(...)
}

#' @rdname whatNWISdata
#' @export
getNWISDataAvailability <- function(siteNumber,service=c("uv","dv","qw")){
  message("This function is being deprecated. Please use whatNWISdata in the future.")
  whatNWISdata(siteNumber=siteNumber, service=service)
}


#' @rdname whatWQPsites
#' @export
getWQPSites<- function(...){
  message("This function is being deprecated. Please use whatWQPsites in the future.")
  whatWQPsites(...)
}

#' @rdname readNWISsite
#' @export
getNWISSiteInfo<- function(siteNumbers){
  message("This function is being deprecated. Please use readNWISsite in the future.")
  readNWISsite(siteNumbers)
}

#' @rdname readNWISpCode
#' @export
getNWISPcodeInfo<- function(parameterCd){
  message("This function is being deprecated. Please use readNWISpCode in the future.")
  readNWISpCode(parameterCd)
}

#' @rdname readNWISdata
#' @export
getNWISData<- function(service="dv", ...){
  message("This function is being deprecated. Please use readNWISdata in the future.")
  readNWISdata(service=service, ...)
}

#' @rdname readNWISdv
#' @export
getNWISdvData<- function(siteNumber,parameterCd,startDate="",endDate="",statCd="00003"){
  message("This function is being deprecated. Please use readNWISdv in the future.")
  readNWISdv(siteNumber,parameterCd,startDate,endDate,statCd)
}

#' @rdname readNWISuv
#' @export
getNWISunitData<- function(siteNumbers,parameterCd,startDate="",endDate="", tz=""){
  message("This function is being deprecated. Please use readNWISuv in the future.")
  readNWISuv(siteNumber,parameterCd,startDate,endDate, tz)
}

#' @rdname readNWISqw
#' @export
getNWISqwData<- function(siteNumber,pCodes,startDate="",endDate="",
                         expanded=FALSE,reshape=TRUE,tz=""){
  message("This function is being deprecated. Please use readNWISqw in the future.")
  readNWISqw(siteNumber,pCodes,startDate,endDate,
             expanded,reshape,tz)
}

#' @rdname readWQPqw
#' @export
getWQPqwData<- function(siteNumber,parameterCd,startDate="",endDate=""){
  message("This function is being deprecated. Please use readWQPqw in the future.")
  readWQPqw(siteNumber,parameterCd,startDate,endDate)
}

#' @rdname readWQPdata
#' @export
getWQPData<- function(...){
  message("This function is being deprecated. Please use readWQPdata in the future.")
  readWQPdata(...)
}


