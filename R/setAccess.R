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
    access.param = '?Access=3'
    message('setting access to internal')
  }else {
    access.param = '?Access=1'
  }
  
  pkg.env$waterservices = paste0("http://waterservices.usgs.gov/nwis/site/", access.param)
  pkg.env$iv =  paste0("http://nwis.waterservices.usgs.gov/nwis/iv/", access.param)
  pkg.env$dv =  paste0("http://waterservices.usgs.gov/nwis/dv/", access.param)
  pkg.env$gwlevels = paste0("http://waterservices.usgs.gov/nwis/gwlevels/", access.param)
  
}

drURL = function(base.name, params){
  return(paste0(pkg.env[[base.name]], params))
}