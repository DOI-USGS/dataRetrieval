#' Sampling Activity Data Import from Water Quality Portal
#'
#' Returns a list of sites from the Water Quality Portal web service. This function gets the data from: \url{https://www.waterqualitydata.us}.
#' Arguments to the function should be based on \url{https://www.waterqualitydata.us/webservices_documentation}
#'
#' @param \dots see \url{https://www.waterqualitydata.us/webservices_documentation} for a complete list of options. A list of arguments can also be supplied.
#' @keywords data import WQP web service
#' @return A data frame 
#' 
#' @export
#' @import utils
#' @examples
#' \dontrun{
#' site1 <- whatWQPsamples(siteid="USGS-01594440")
#' 
#' type <- "Stream"
#' sites <- whatWQPsamples(countycode="US:55:025",siteType=type)
#' lakeSites <- whatWQPsamples(siteType = "Lake, Reservoir, Impoundment", statecode = "US:55")
#' }
whatWQPsamples <- function(...){
  
  matchReturn <- c(do.call("c",list(...)[sapply(list(...), class) == "list"]), #get the list parts
                   list(...)[sapply(list(...), class) != "list"]) # get the non-list parts
  
  values <- readWQPdots(...)
  
  values <- sapply(values, function(x) URLencode(x, reserved = TRUE))
  
  urlCall <- paste(paste(names(values),values,sep="="),collapse="&")
  
  
  baseURL <- drURL("wqpActivity")
  urlCall <- paste0(baseURL,
                    urlCall,
                    "&mimeType=tsv&sorted=no")
  
  retval <- importWQP(urlCall, zip=values["zip"] == "yes")
  
  attr(retval, "queryTime") <- Sys.time()
  attr(retval, "url") <- urlCall
  
  return(retval)
}

# Only STORET and not publically mapped yet:
# whatWQPmetrics <- function(...){
#   
#   matchReturn <- c(do.call("c",list(...)[sapply(list(...), class) == "list"]), #get the list parts
#                    list(...)[sapply(list(...), class) != "list"]) # get the non-list parts
#   
#   values <- readWQPdots(...)
#   
#   values <- sapply(values, function(x) URLencode(x, reserved = TRUE))
#   
#   urlCall <- paste(paste(names(values),values,sep="="),collapse="&")
#   
#   
#   baseURL <- drURL("wqpMetrics")
#   urlCall <- paste0(baseURL,
#                     urlCall,
#                     "&mimeType=tsv&sorted=no")
#   
#   retval <- importWQP(urlCall, zip=values["zip"] == "yes")
#   
#   attr(retval, "queryTime") <- Sys.time()
#   attr(retval, "url") <- urlCall
#   
#   return(retval)
# }
# 
