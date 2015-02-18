#' General Data Import from NWIS
#'
#' Returns data from the NWIS web service.
#' Arguments to the function should be based on \url{http://waterservices.usgs.gov} service calls.
#'
#' @param service string. Possible values are "iv" (for instantaneous), "dv" (for daily values), "gwlevels" 
#' (for groundwater levels), and "site" (for site service)
#' @param \dots see \url{http://waterservices.usgs.gov/rest/Site-Service.html#Service} for a complete list of options
#' @keywords data import NWIS web service
#' @return A data frame with the following columns:
#' \tabular{lll}{
#' Name \tab Type \tab Description \cr
#' agency \tab character \tab The NWIS code for the agency reporting the data\cr
#' site \tab character \tab The USGS site number \cr
#' datetime \tab POSIXct \tab The date and time of the value converted to UTC (for unit value data), \cr 
#' \tab character \tab or raw character string \cr
#' tz_cd \tab character \tab The time zone code for datetime \cr
#' code \tab character \tab Any codes that qualify the corresponding value\cr
#' value \tab numeric \tab The numeric value for the parameter \cr
#' }
#' Note that code and value are repeated for the parameters requested. The names are of the form 
#' X_D_P_S, where X is literal, 
#' D is an option description of the parameter, 
#' P is the parameter code, 
#' and S is the statistic code (if applicable).
#' 
#' There are also several useful attributes attached to the data frame:
#' \tabular{lll}{
#' Name \tab Type \tab Description \cr
#' url \tab character \tab The url used to generate the data \cr
#' siteInfo \tab data.frame \tab A data frame containing information on the requested sites \cr
#' variableInfo \tab data.frame \tab A data frame containing information on the requested parameters \cr
#' statisticInfo \tab data.frame \tab A data frame containing information on the requested statistics on the data \cr
#' queryTime \tab POSIXct \tab The time the data was returned \cr
#' }
#' 
#' @seealso \code{\link{renameNWISColumns}},  \code{\link{importWaterML1}}, \code{\link{importRDB1}}
#' @export
#' @examples
#' \dontrun{
#' # Examples not run for time considerations
#' dataTemp <- readNWISdata(stateCd="OH",parameterCd="00010")
#' dataTempUnit <- readNWISdata(sites="03086500", service="iv", parameterCd="00010")
#' #Empty:
#' multiSite <- readNWISdata(sites=c("04025000","04072150"), service="iv", parameterCd="00010")
#' #Not empty:
#' multiSite <- readNWISdata(sites=c("04025500","040263491"), service="iv", parameterCd="00060")
#' bBoxEx <- readNWISdata(bBox=c(-83,36.5,-81,38.5), parameterCd="00010")
#' startDate <- as.Date("2013-10-01")
#' endDate <- as.Date("2014-09-30")
#' waterYear <- readNWISdata(bBox=c(-83,36.5,-81,38.5), parameterCd="00010", 
#'                   service="dv", startDate=startDate, endDate=endDate)
#' siteInfo <- readNWISdata(stateCd="WI", parameterCd="00010",hasDataTypeCd="iv", service="site")
#' }
readNWISdata <- function(service="dv", ...){
  
  matchReturn <- list(...)
  
  match.arg(service, c("dv","iv","gwlevels","site", "uv"))
  
  if(service == "uv"){
    service <- "iv"
  }
  
  if(length(service) > 1){
    stop("Only one service call allowed.")
  }
  
  values <- sapply(matchReturn, function(x) URLencode(as.character(paste(eval(x),collapse=",",sep=""))))
  
  names(values)[names(values) == "startDate"] <- "startDT"
  names(values)[names(values) == "endDate"] <- "endDT"
  names(values)[names(values) == "siteNumber"] <- "sites"
  names(values)[names(values) == "siteNumbers"] <- "sites"
  
  urlCall <- paste(paste(names(values),values,sep="="),collapse="&")
  
  format <- "waterml,1.1"
  baseURL <- "http://waterservices.usgs.gov/nwis/"
  
  if(service == "iv"){
    baseURL <- "http://nwis.waterservices.usgs.gov/nwis/"
  }
  
  if(service == "site"){
    format <- "rdb"
  }
  
  baseURL <- paste0(baseURL,service,"/?format=",format,"&")
  urlCall <- paste0(baseURL,urlCall)
  
  if(service == "site"){
    retval <- importRDB1(urlCall, asDateTime = FALSE, qw = FALSE)
  } else {
    retval <- importWaterML1(urlCall, asDateTime = ("iv" == service))
    if("dv" == service){
      retval$dateTime <- as.Date(retval$dateTime)
      retval$tz_cd <- NULL      
      names(retval)[names(retval) == "dateTime"] <- "Date"
    }
  }
  
  return(retval)
}
