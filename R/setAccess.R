#'Set data endpoint
#'
#'access Indicate which dataRetrieval access code
#' you want to use options: \code{c('public','internal')}
#'
#' @param access code for data access. Either "public" or "internal"
#'
#'@author Luke Winslow, Jordan S Read
#'
#'@examples
#'
#'\dontrun{
#'setAccess('internal')
#'
#'setAccess('public')
#'
#'}
#'
#' @export
setAccess = function(access="public"){
  
  access = match.arg(access, c('public','internal'))
  
  if(access=="internal"){
    pkg.env$access = '3'
    message('setting access to internal')
  }else {
    pkg.env$access = '0'
    message('setting access to public')
  }
  
  pkg.env$waterservices = "http://waterservices.usgs.gov/nwis/site/"
  pkg.env$iv = "http://nwis.waterservices.usgs.gov/nwis/iv/"
  pkg.env$dv =  "http://waterservices.usgs.gov/nwis/dv/"
  pkg.env$gwlevels = "http://waterservices.usgs.gov/nwis/gwlevels/"
  
}

drURL <- function(base.name, ..., arg.list=NULL){
  
  
  queryString <- drQueryArgs(..., arg.list=arg.list)
  #to do: add something to check for redundant params
  
  return(paste0(pkg.env[[base.name]], '?', queryString))
}

drQueryArgs <- function(..., arg.list){
  args <- append(expand.grid(..., stringsAsFactors = FALSE), arg.list)
  # get the args into name=value strings
  keyValues <- paste0(names(args),unname(lapply(args, function(x) paste0('=',x[[1]]))))
  return(paste(keyValues, collapse='&'))
}

appendDrURL <- function(url, ..., arg.list=NULL){
  
  queryString <- drQueryArgs(..., arg.list=arg.list)
  return(paste0(url, "&", queryString))
}