#'Set data endpoint
#'
#'access Indicate which dataRetrieval access code
#' you want to use options: \code{c('public','internal')}
#'
#' @param access code for data access. Options are: "public","internal","cooperator", or "USGS".
#' \itemize{ 
#' \item{"internal" represents Access=3 ...for a single water science center}
#' \item{"USGS" represents Access=2 ...for all water science centers}
#' \item{"cooperator" represents Access=1}
#' \item{"public" represents Access=0, public access}
#' }
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
  
  access = match.arg(access, c('public','internal','cooperator','USGS'))
  
  if(access=="internal"){
    pkg.env$access = '3'
    message('setting access to internal')
  } else if(access=="cooperator"){
    pkg.env$access = '1'
    message('setting access to cooperator')
  } else if(access=="USGS"){
    pkg.env$access = '2'
    message('setting access to all USGS Water Science Centers')    
  } else {
    pkg.env$access = NULL
    message('setting access to public')
  }
  
  pkg.env$site = "http://waterservices.usgs.gov/nwis/site/"
  pkg.env$iv = "http://nwis.waterservices.usgs.gov/nwis/iv/"
  pkg.env$dv =  "http://waterservices.usgs.gov/nwis/dv/"
  pkg.env$gwlevels = "http://waterservices.usgs.gov/nwis/gwlevels/"
  pkg.env$measurements = "http://waterdata.usgs.gov/nwis/measurements/"
  pkg.env$peak = "http://nwis.waterdata.usgs.gov/usa/nwis/peak/"
  pkg.env$rating = "http://waterdata.usgs.gov/nwisweb/get_ratings/"
  pkg.env$qwdata = "http://nwis.waterdata.usgs.gov/nwis/qwdata"
  
  options(Access.dataRetrieval = access)
}

drURL <- function(base.name, ..., arg.list=NULL){
  
  
  queryString <- drQueryArgs(..., arg.list=arg.list)
  #to do: add something to check for redundant params
  
  return(paste0(pkg.env[[base.name]], '?', queryString))
}

drQueryArgs <- function(..., arg.list){
  dots <- list(...)
  dots <- dots[!vapply(X=dots,FUN=is.null,FUN.VALUE = TRUE)]
  
  args <- append(expand.grid(dots, stringsAsFactors = FALSE), arg.list)
  # get the args into name=value strings
  keyValues <- paste0(names(args),unname(lapply(args, function(x) paste0('=',x[[1]]))))
  return(paste(keyValues, collapse='&'))
}

appendDrURL <- function(url, ..., arg.list=NULL){
  
  queryString <- drQueryArgs(..., arg.list=arg.list)
  return(paste0(url, "&", queryString))
}