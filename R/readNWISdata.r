#' General Data Import from NWIS
#'
#' Returns data from the NWIS web service.
#' Arguments to the function should be based on \url{http://waterservices.usgs.gov} service calls.
#'
#' @param service string. Possible values are "iv" (for instantaneous), "dv" (for daily values), "gwlevels" 
#' (for groundwater levels), and "qwdata" (for water quality)
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
#' }
readNWISdata <- function(service="dv", ...){
  
  matchReturn <- list(...)
  
  values <- sapply(matchReturn, function(x) URLencode(as.character(paste(eval(x),collapse=",",sep=""))))
  
  urlCall <- paste(paste(names(values),values,sep="="),collapse="&")
  
  if(service %in% c("dv","iv","gwlevels")){
    format <- "waterml"
  } else {
    format <- "rdb1,1"
  }
  
  baseURL <- paste0("http://waterservices.usgs.gov/nwis/",service,"/?format=",format,"&")
  urlCall <- paste0(baseURL,urlCall)
  
  if(service=="qwdata"){
    urlCall <- paste0(urlCall,"&siteOutput=expanded")
    retval <- importRDB1(urlCall)
  } else {

    retval <- importWaterML1(urlCall, asDateTime = ("iv" == service))
  }
  
  
  return(retval)
}
