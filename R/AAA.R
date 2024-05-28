pkg.env <- new.env()

.onLoad <- function(libname, pkgname) {
  suppressMessages(setAccess("public"))
  pkg.env$nldi_base <- "https://labs.waterdata.usgs.gov/api/nldi/linked-data/"
  pkg.env$local_sf <- requireNamespace("sf", quietly = TRUE)
}


#' Is this a dataRetrieval user
#'
#' Reveals if this is a user or not
#' @export
#' @examples
#' is_dataRetrieval_user()
is_dataRetrieval_user <- function() {
  interactive() ||
    !identical(Sys.getenv("CI"), "") ||
    identical(Sys.getenv("NOT_CRAN"), "true")
}


wqp_message_no_legacy <- function(){
  message("NEWS: data do not include newer USGS data. More details: 
https://doi-usgs.github.io/dataRetrieval/articles/Status.html.
Contact CompTools@usgs.gov with questions")
}

wqp_message <- function(){
  message("NEWS: Legacy data profiles will be retired. Please begin converting
workflows to the WQX profiles. ALso, data from legacy profiles do not
include newer USGS data. More details:
https://doi-usgs.github.io/dataRetrieval/articles/Status.html
Contact CompTools@usgs.gov with questions")
}

nwis_message <- function(){
  return("WARNING: NWIS does not deliver
new discrete water quality data or updates to existing data. 
For additional details, see:
https://doi-usgs.github.io/dataRetrieval/articles/Status.html
Contact CompTools@usgs.gov with questions")
}
